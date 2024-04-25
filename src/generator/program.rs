use std::{borrow::Borrow, rc::Rc};

use inkwell::{
    types::{BasicMetadataTypeEnum, BasicType},
    values::{BasicMetadataValueEnum, BasicValueEnum},
    AddressSpace,
};
use thiserror::Error;

use crate::{
    generator::{util::CodeGenerationErrorHint, VariableDefinition},
    parser::{
        general::{NumberNode, TypeMark},
        procedure::{
            ProcedureBodyNode, ProcedureCallNode, ProcedureDeclarationNode, ProcedureHeaderNode,
        },
        variable::VariableDeclarationNode,
        DeclarationNode, DeclarationType, ProgramNode,
    },
};

use super::{
    variable::{VariableDeclarationNodeCodeGenerationError, MAX_SUPPORTED_ARRAY_SIZE},
    CodeGenerator, CodeGeneratorContext, FunctionDefinition, OwnedCodeGenerator,
    VariableDefinitionData,
};

pub fn declare_variable_from_declaration<'a>(
    context: Rc<CodeGeneratorContext<'a>>,
    declaration: &VariableDeclarationNode,
    is_global: bool,
) -> anyhow::Result<Rc<VariableDefinition<'a>>> {
    let (data, size) = match declaration {
        VariableDeclarationNode::Unbounded(data) => (data, 1),
        VariableDeclarationNode::Bounded(data, size) => (
            data,
            match size {
                NumberNode::IntegerLiteral(i) => *i,
                NumberNode::FloatLiteral(_) => {
                    return Err(VariableDeclarationNodeCodeGenerationError::NonIntegerBound.into())
                }
            },
        ),
    };

    let ctx_type = match data.variable_type {
        TypeMark::Bool => context.context.bool_type().as_basic_type_enum(),
        TypeMark::Float => context.context.f64_type().as_basic_type_enum(),
        TypeMark::Integer => context.context.i64_type().as_basic_type_enum(),
        TypeMark::String => context
            .context
            .i8_type()
            .ptr_type(AddressSpace::default())
            .as_basic_type_enum(),
    };

    let (ctx_type, size) = match size {
        1 => (ctx_type, None),
        2..=MAX_SUPPORTED_ARRAY_SIZE => {
            let size = size as u32;
            (ctx_type.array_type(size).as_basic_type_enum(), Some(size))
        }
        _ => return Err(VariableDeclarationNodeCodeGenerationError::UnsupportedBound.into()),
    };

    let ptr_value = context.builder.build_alloca(ctx_type, &data.identifier)?;

    let identifier = data.identifier.clone();

    let data = VariableDefinitionData {
        identifier: data.identifier.clone(),
        ctx_type,
        ptr_value,
    };

    let definition = match size {
        None => VariableDefinition::NotIndexable(data),
        Some(size) => VariableDefinition::Indexable(data, size),
    };

    context.declare_variable(identifier.clone(), definition, is_global);

    Ok(CodeGeneratorContext::get_variable(context, identifier).unwrap())
}

pub fn declare_function_from_declaration<'a>(
    context: Rc<CodeGeneratorContext<'a>>,
    declaration: &ProcedureDeclarationNode,
    is_global: bool,
) -> anyhow::Result<Rc<FunctionDefinition<'a>>> {
    let return_type = match declaration.header.return_type {
        TypeMark::Bool => context.context.bool_type().as_basic_type_enum(),
        TypeMark::Float => context.context.f64_type().as_basic_type_enum(),
        TypeMark::Integer => context.context.i64_type().as_basic_type_enum(),
        TypeMark::String => context
            .context
            .i8_type()
            .ptr_type(AddressSpace::default())
            .as_basic_type_enum(),
    };

    let mut parameter_types: Vec<BasicMetadataTypeEnum<'a>> = vec![];

    for node in &declaration.header.parameters {
        let definition = declare_variable_from_declaration(Rc::clone(&context), node, false)?;

        let data = match definition.borrow() {
            VariableDefinition::NotIndexable(data) => data,
            VariableDefinition::Indexable(data, _) => data,
        };

        parameter_types.push(data.ctx_type.into());
    }

    let parameter_types = declaration
        .header
        .parameters
        .iter()
        .map(|node| {
            let definition = declare_variable_from_declaration(Rc::clone(&context), node, false)?;

            let data = match definition.borrow() {
                VariableDefinition::NotIndexable(data) => data,
                VariableDefinition::Indexable(data, _) => data,
            };

            Ok(data.ctx_type.into())
        })
        .collect::<anyhow::Result<Vec<_>>>()?;

    let new_fn_type = return_type.fn_type(&parameter_types, false);

    let fn_value = context.declare_function(
        declaration.header.identifier.clone(),
        new_fn_type,
        None,
        is_global,
    );

    let function_definition = CodeGeneratorContext::get_function(
        Rc::clone(&context),
        declaration.header.identifier.clone(),
    )
    .unwrap();

    let _new_context = context.new_scope(Rc::clone(&function_definition));
    let new_context = Rc::new(_new_context);

    let entry_bb = new_context.context.append_basic_block(fn_value, "entry");
    new_context.builder.position_at_end(entry_bb);

    declaration
        .body
        .declarations
        .iter()
        .try_for_each(|dec| dec.generate_code(Rc::clone(&new_context), None))?;

    declaration
        .body
        .statements
        .iter()
        .try_for_each(|stmt| stmt.generate_code(Rc::clone(&new_context), None))?;

    // Default return type is the "const_zero" of whatever the return is
    context
        .builder
        .build_return(Some(&return_type.const_zero()))?;

    Ok(function_definition)
}

impl<'a> CodeGenerator<'a> for DeclarationNode {
    type Item = ();

    fn generate_code(
        &self,
        context: Rc<CodeGeneratorContext<'a>>,
        _previous: Option<Self::Item>,
    ) -> anyhow::Result<Self::Item> {
        let (is_global, declaration_type) = match self {
            DeclarationNode::Global(ty) => (true, ty),
            DeclarationNode::Local(ty) => (false, ty),
        };

        let cloned_context = Rc::clone(&context);

        match declaration_type {
            DeclarationType::Variable(node) => {
                declare_variable_from_declaration(cloned_context, node, is_global)?;
            }
            DeclarationType::Procedure(node) => {
                declare_function_from_declaration(cloned_context, node, is_global)?;
            }
        };

        Ok(())
    }
}

impl<'a> OwnedCodeGenerator<'a> for ProgramNode {
    type Item = ();

    fn generate_code(
        self,
        context: Rc<CodeGeneratorContext<'a>>,
        _previous: Option<Self::Item>,
    ) -> anyhow::Result<Self::Item> {
        let _program_name = &self.0 .0;

        let main_procedure_node = ProcedureDeclarationNode {
            header: ProcedureHeaderNode {
                identifier: "main".to_string(),
                return_type: TypeMark::Integer,
                parameters: vec![],
            },
            body: ProcedureBodyNode {
                declarations: self.1.declarations,
                statements: self.1.statements,
            },
        };

        declare_function_from_declaration(context, &main_procedure_node, true)?;

        Ok(())
    }
}

#[derive(Error, Debug)]
pub enum ProcedureCallNodeCodeGenerationError {
    #[error("Undeclared function call {0}.")]
    UndeclaredFunction(String),

    #[error("Mismatched parameter types ({0}), expected ({1}).")]
    MismatchedParamterTypes(String, String),

    #[error("Unexpected void return. These are not supported.")]
    UnexpectedVoidReturn,
}

impl<'a> CodeGenerator<'a> for ProcedureCallNode {
    type Item = BasicValueEnum<'a>;

    fn generate_code(
        &self,
        context: Rc<CodeGeneratorContext<'a>>,
        _previous: Option<Self::Item>,
    ) -> anyhow::Result<Self::Item> {
        let reference =
            CodeGeneratorContext::get_function(Rc::clone(&context), self.identifier.clone());
        let reference = reference.ok_or(
            ProcedureCallNodeCodeGenerationError::UndeclaredFunction(self.identifier.clone()),
        )?;

        let expected_param_types = reference.fn_type.get_param_types();

        let parameters = self
            .arguments
            .iter()
            .map(|arg| arg.generate_code(Rc::clone(&context), None))
            .collect::<anyhow::Result<Vec<_>>>()?;

        let parameter_types = parameters
            .iter()
            .map(|param| param.get_type())
            .collect::<Vec<_>>();

        // NOTE: LANGUAGE SEMANTICS | RULE #8
        // The types of the procedure arguments must exactly match the expected argument types.
        if expected_param_types != parameter_types {
            let parameter_types = parameter_types
                .iter()
                .map(|ty| ty.error_hint())
                .collect::<Vec<String>>()
                .join(", ");

            let expected_param_types = expected_param_types
                .iter()
                .map(|ty| ty.error_hint())
                .collect::<Vec<String>>()
                .join(", ");

            return Err(
                ProcedureCallNodeCodeGenerationError::MismatchedParamterTypes(
                    parameter_types,
                    expected_param_types,
                )
                .into(),
            );
        }

        let parameters: Vec<BasicMetadataValueEnum> =
            parameters.iter().by_ref().map(|&val| val.into()).collect();

        // TODO: Implement pass-by-value for arrays
        let call_value =
            context
                .builder
                .build_call(reference.fn_value, parameters.as_slice(), "call")?;

        Ok(match call_value.try_as_basic_value().left() {
            Some(value) => value,
            None => return Err(ProcedureCallNodeCodeGenerationError::UnexpectedVoidReturn.into()),
        })
    }
}
