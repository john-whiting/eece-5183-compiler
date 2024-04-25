use inkwell::{
    types::BasicTypeEnum,
    values::{BasicValue, BasicValueEnum},
    IntPredicate,
};
use thiserror::Error;

use crate::{
    generator::util::CodeGenerationErrorHint,
    parser::statement::{
        AssignmentStatementNode, IfStatementNode, LoopStatementNode, ReturnStatementNode,
        StatementNode,
    },
};

use super::{CodeGenerator, CodeGeneratorContext, VariableDefinition};

#[derive(Error, Debug)]
pub enum AssignmentStatementNodeCodeGenerationError {
    #[error("Unable to assign to undeclared variable {0}.")]
    UndeclaredVariable(String),

    #[error("A value of type {1} cannot be assigned to a variable of type {0}.")]
    IncompatibleTypes(String, String),
}

impl<'a> CodeGenerator<'a> for AssignmentStatementNode {
    type Item = ();

    fn generate_code(
        &self,
        context: &'a CodeGeneratorContext,
        _previous: Option<Self::Item>,
    ) -> anyhow::Result<Self::Item> {
        let (data, index_of) = match self {
            AssignmentStatementNode::NotIndexed(data) => (data, None),
            AssignmentStatementNode::Indexed(data, i) => (data, Some(i)),
        };

        let reference = context.get_variable(&data.identifier);
        let reference = reference.ok_or(
            AssignmentStatementNodeCodeGenerationError::UndeclaredVariable(data.identifier.clone()),
        )?;

        // Index the variable (if applicable)
        let (ptr_value, ctx_type) = if let Some(index_of) = index_of {
            reference.index_of(context, index_of)?
        } else {
            match reference.as_ref() {
                VariableDefinition::NotIndexable(data) | VariableDefinition::Indexable(data, _) => {
                    (data.ptr_value, data.ctx_type)
                }
            }
        };

        let expression = data.expression.generate_code(context, None)?;

        match (ctx_type, expression) {
            (BasicTypeEnum::IntType(ty), BasicValueEnum::IntValue(value)) => {
                let value = if ty.get_bit_width() == 1 && value.get_type().get_bit_width() != 1 {
                    // NOTE: LANGUAGE SEMANTICS | RULE #7
                    // Integer cast to bool (any non-zero integer is true, otherwise false)
                    context.builder.build_int_compare(
                        IntPredicate::NE,
                        value,
                        value.get_type().const_zero(),
                        "int_casting",
                    )?
                } else {
                    value
                };

                context.builder.build_store(ptr_value, value)?
            }
            (BasicTypeEnum::FloatType(ty), BasicValueEnum::IntValue(value)) => {
                let value = context
                    .builder
                    .build_signed_int_to_float(value, ty, "i64_to_f64")?;
                context.builder.build_store(ptr_value, value)?
            }
            (BasicTypeEnum::FloatType(_), BasicValueEnum::FloatValue(value)) => {
                context.builder.build_store(ptr_value, value)?
            }
            (BasicTypeEnum::ArrayType(_), _) => todo!("Array assignments are not yet supported"),
            (BasicTypeEnum::PointerType(_), _) => {
                todo!("Pointer assignments (strings?) are not yet supported")
            }
            (ty, value) => {
                return Err(
                    AssignmentStatementNodeCodeGenerationError::IncompatibleTypes(
                        ty.error_hint(),
                        value.error_hint(),
                    )
                    .into(),
                )
            }
        };

        Ok(())
    }
}

#[derive(Error, Debug)]
pub enum IfStatementNodeCodeGenerationError {
    #[error("Conditions can only be booleans or integers. A type of {0} is not supported.")]
    UnsupportedConditionType(String),
}

impl<'a> CodeGenerator<'a> for IfStatementNode {
    type Item = ();

    fn generate_code(
        &self,
        context: &'a CodeGeneratorContext,
        _previous: Option<Self::Item>,
    ) -> anyhow::Result<Self::Item> {
        let cur_fn_value = context.fn_value();

        let then_bb = context.context.append_basic_block(cur_fn_value, "then");
        let else_bb = context.context.append_basic_block(cur_fn_value, "else");
        let cont_bb = context.context.append_basic_block(cur_fn_value, "ifcont");

        let cond = match self.condition.generate_code(context, None)? {
            BasicValueEnum::IntValue(value) => {
                if value.get_type().get_bit_width() != 1 {
                    // NOTE: LANGUAGE SEMANTICS | RULE #7
                    // Integer cast to bool (any non-zero integer is true, otherwise false)
                    context.builder.build_int_compare(
                        IntPredicate::NE,
                        value,
                        value.get_type().const_zero(),
                        "int_casting",
                    )?
                } else {
                    value
                }
            }
            value => {
                return Err(
                    IfStatementNodeCodeGenerationError::UnsupportedConditionType(
                        value.error_hint(),
                    )
                    .into(),
                )
            }
        };

        context
            .builder
            .build_conditional_branch(cond, then_bb, else_bb)?;

        // then block
        context.builder.position_at_end(then_bb);
        self.then_block
            .iter()
            .try_for_each(|stmt| stmt.generate_code(context, None))?;
        context.builder.build_unconditional_branch(cont_bb)?;

        // else block
        context.builder.position_at_end(else_bb);
        if let Some(else_block) = &self.else_block {
            else_block
                .iter()
                .try_for_each(|stmt| stmt.generate_code(context, None))?;
        }
        context.builder.build_unconditional_branch(cont_bb)?;

        // clean up
        context.builder.position_at_end(cont_bb);

        Ok(())
    }
}

#[derive(Error, Debug)]
pub enum LoopStatementNodeCodeGenerationError {
    #[error("Conditions can only be booleans or integers. A type of {0} is not supported.")]
    UnsupportedConditionType(String),
}

impl<'a> CodeGenerator<'a> for LoopStatementNode {
    type Item = ();

    fn generate_code(
        &self,
        context: &'a CodeGeneratorContext,
        _previous: Option<Self::Item>,
    ) -> anyhow::Result<Self::Item> {
        let cur_fn_value = context.fn_value();

        let loop_bb = context.context.append_basic_block(cur_fn_value, "loop");
        let after_loop_bb = context
            .context
            .append_basic_block(cur_fn_value, "afterloop");

        // loop block
        context.builder.position_at_end(loop_bb);

        // loop block: condition
        let cond = match self.condition.generate_code(context, None)? {
            BasicValueEnum::IntValue(value) => {
                if value.get_type().get_bit_width() != 1 {
                    // NOTE: LANGUAGE SEMANTICS | RULE #7
                    // Integer cast to bool (any non-zero integer is true, otherwise false)
                    context.builder.build_int_compare(
                        IntPredicate::NE,
                        value,
                        value.get_type().const_zero(),
                        "int_casting",
                    )?
                } else {
                    value
                }
            }
            value => {
                return Err(
                    LoopStatementNodeCodeGenerationError::UnsupportedConditionType(
                        value.error_hint(),
                    )
                    .into(),
                )
            }
        };

        context
            .builder
            .build_conditional_branch(cond, loop_bb, after_loop_bb)?;

        // loop block: body
        self.statements
            .iter()
            .try_for_each(|stmt| stmt.generate_code(context, None))?;

        // loop block: assignment
        self.assignment.generate_code(context, None)?;

        // loop block: back to top
        context.builder.build_unconditional_branch(loop_bb)?;

        // clean up
        context.builder.position_at_end(after_loop_bb);

        Ok(())
    }
}

#[derive(Error, Debug)]
pub enum ReturnStatementNodeCodeGenerationError {
    #[error("Unable to implicitly cast {0} to {1} for a return.")]
    UnsupportedTypeCast(String, String),
}

impl<'a> CodeGenerator<'a> for ReturnStatementNode {
    type Item = ();

    fn generate_code(
        &self,
        context: &'a CodeGeneratorContext,
        _previous: Option<Self::Item>,
    ) -> anyhow::Result<Self::Item> {
        let cur_fn_value = context.fn_value();

        let return_type = cur_fn_value.get_type().get_return_type();

        let expr = self.0.generate_code(context, None)?;

        let ret_value = match (expr, return_type) {
            (BasicValueEnum::IntValue(value), Some(BasicTypeEnum::IntType(ty))) => {
                if ty.get_bit_width() == 1 && value.get_type().get_bit_width() != 1 {
                    // NOTE: LANGUAGE SEMANTICS | RULE #7
                    // Integer cast to bool (any non-zero integer is true, otherwise false)
                    context.builder.build_int_compare(
                        IntPredicate::NE,
                        value,
                        value.get_type().const_zero(),
                        "int_casting",
                    )?
                } else {
                    value
                }
                .as_basic_value_enum()
            }
            (BasicValueEnum::IntValue(value), Some(BasicTypeEnum::FloatType(ty))) => context
                .builder
                .build_signed_int_to_float(value, ty, "int_to_float")?
                .as_basic_value_enum(),
            (BasicValueEnum::FloatValue(_), Some(BasicTypeEnum::FloatType(_))) => expr,
            (BasicValueEnum::ArrayValue(_), Some(BasicTypeEnum::ArrayType(_))) => expr,
            (BasicValueEnum::PointerValue(_), Some(BasicTypeEnum::PointerType(_))) => expr,
            (value, Some(ty)) => {
                return Err(ReturnStatementNodeCodeGenerationError::UnsupportedTypeCast(
                    value.error_hint(),
                    ty.error_hint(),
                )
                .into())
            }
            (value, None) => {
                return Err(ReturnStatementNodeCodeGenerationError::UnsupportedTypeCast(
                    value.error_hint(),
                    "void".to_string(),
                )
                .into())
            }
        };

        context.builder.build_return(Some(&ret_value))?;

        Ok(())
    }
}

impl<'a> CodeGenerator<'a> for StatementNode {
    type Item = ();

    fn generate_code(
        &self,
        context: &'a CodeGeneratorContext,
        _previous: Option<Self::Item>,
    ) -> anyhow::Result<Self::Item> {
        match self {
            StatementNode::Assignment(node) => node.generate_code(context, None)?,
            StatementNode::If(node) => node.generate_code(context, None)?,
            _ => todo!(),
        };

        Ok(())
    }
}
