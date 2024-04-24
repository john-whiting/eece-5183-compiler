use std::{cell::RefCell, collections::HashMap, ops::Deref, rc::Rc};

use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::{BasicTypeEnum, FunctionType},
    values::{BasicValueEnum, FunctionValue, IntValue, PointerValue},
};
use thiserror::Error;

use crate::parser::expression::ExpressionNode;

use self::{cstd::CStd, mstd::Std, util::CodeGenerationErrorHint};

mod cstd;
mod expression;
mod mstd;
mod util;
mod variable;

#[derive(Error, Debug)]
pub enum VariableDefinitionImplErr {
    #[error("Unable to index {0}.")]
    NonArrayIndexing(String),

    #[error("Variable is missing constant size.")]
    ArrayMissingSize,

    #[error("Unable to index an array with {0}.")]
    NonIntegerIndex(String),
}

#[derive(Clone)]
pub struct VariableDefinitionData<'a> {
    pub identifier: String,
    pub ctx_type: BasicTypeEnum<'a>,
    pub ptr_value: PointerValue<'a>,
}

#[derive(Clone)]
pub enum VariableDefinition<'a> {
    Indexable(VariableDefinitionData<'a>, u32),
    NotIndexable(VariableDefinitionData<'a>),
}

impl<'a> VariableDefinition<'a> {
    pub fn index_of(
        &self,
        context: &'a CodeGeneratorContext,
        index_of: ExpressionNode,
    ) -> anyhow::Result<(PointerValue<'a>, BasicTypeEnum<'a>)> {
        let (reference, size) = match self {
            VariableDefinition::Indexable(data, size) => Ok((data, Some(size))),
            // Only arrays can be indexed
            VariableDefinition::NotIndexable(data) => Err(
                VariableDefinitionImplErr::NonArrayIndexing(data.ctx_type.error_hint()),
            ),
        }?;

        // if indexing a variable, that variable must be sized
        let size = size.ok_or(VariableDefinitionImplErr::ArrayMissingSize)?;

        // ctx_type is the inside type
        let ctx_type = reference.ctx_type.into_array_type().get_element_type();

        let expr = index_of.generate_code(context, None)?;

        // LANGUAGE SEMANTICS | RULE #13
        // Arrays must be indexed by integers ONLY
        let expr = match expr {
            BasicValueEnum::IntValue(value) => value,
            unsupported_value => {
                return Err(VariableDefinitionImplErr::NonIntegerIndex(
                    unsupported_value.error_hint(),
                )
                .into())
            }
        };

        // LANGUAGE SEMANTICS | RULE #13
        // The lower bound of an array index is 0, and the upper bound is size - 1

        let fails_lower_bound_value = context.builder.build_int_compare(
            inkwell::IntPredicate::SLT,
            expr,
            context.context.i64_type().const_zero(),
            "array_lower_bounds_check",
        )?;
        let fails_upper_bound_value = context.builder.build_int_compare(
            inkwell::IntPredicate::SGE,
            expr,
            context.context.i64_type().const_int(*size as u64, true),
            "array_upper_bounds_check",
        )?;

        let fails_bound_value = context.builder.build_or(
            fails_lower_bound_value,
            fails_upper_bound_value,
            "fails_bound_check",
        )?;

        let current_fn_value = context.fn_value();
        let then_bb = context.context.append_basic_block(current_fn_value, "then");
        let cont_bb = context
            .context
            .append_basic_block(current_fn_value, "ifcont");

        context
            .builder
            .build_conditional_branch(fails_bound_value, then_bb, cont_bb)
            .unwrap();

        // build the then block
        context.builder.position_at_end(then_bb);
        // TODO: Add out of bounds message
        context.cstd.exit(1)?;

        // place builder at the continue block
        context.builder.position_at_end(cont_bb);

        Ok((
            unsafe {
                context
                    .builder
                    .build_gep(ctx_type, reference.ptr_value, &[expr], "array_index")?
            },
            ctx_type,
        ))
    }
}

#[derive(Clone, Debug)]
pub struct FunctionDefinition<'a> {
    pub identifier: String,
    pub fn_type: FunctionType<'a>,
    pub fn_value: FunctionValue<'a>,
}

impl<'a> FunctionDefinition<'a> {
    pub fn new(value: FunctionValue<'a>) -> Self {
        Self {
            identifier: value.get_name().to_string_lossy().as_ref().to_owned(),
            fn_type: value.get_type(),
            fn_value: value,
        }
    }
}

pub struct CodeGeneratorContext<'a> {
    pub context: &'a Context,
    pub module: Rc<Module<'a>>,
    pub builder: Rc<Builder<'a>>,

    pub cstd: Rc<CStd<'a>>,
    pub std: Rc<Std<'a>>,

    pub fn_value_opt: Option<FunctionValue<'a>>,

    // NOTE: LANGUAGE SEMANTICS | RULE #3
    // All "local" variables are for one procedure only.
    // There cannot be multiple "local" scopes at a time.
    local_variables: HashMap<String, Rc<VariableDefinition<'a>>>,
    global_variables: Rc<RefCell<HashMap<String, Rc<VariableDefinition<'a>>>>>,

    // NOTE: LANGUAGE SEMANTICS | RULE #3
    // All "local" functions are for one procedure only.
    // There cannot be multiple "local" scopes at a time.
    local_functions: HashMap<String, Rc<FunctionDefinition<'a>>>,
    global_functions: Rc<RefCell<HashMap<String, Rc<FunctionDefinition<'a>>>>>,
}

impl<'a> CodeGeneratorContext<'a> {
    pub fn new(context: &'a Context) -> Self {
        let module = Rc::new(context.create_module(env!("CARGO_PKG_NAME")));
        let builder = Rc::new(context.create_builder());
        let cstd = Rc::new(CStd::new(context, Rc::clone(&module), Rc::clone(&builder)));
        let _std = Rc::new(Std::new(
            &cstd,
            context,
            Rc::clone(&module),
            Rc::clone(&builder),
        ));

        let mut context = Self {
            context,
            module,
            builder,
            cstd,
            std: _std,
            fn_value_opt: None,
            global_variables: Rc::new(RefCell::new(HashMap::new())),
            local_variables: HashMap::new(),
            global_functions: Rc::new(RefCell::new(HashMap::new())),
            local_functions: HashMap::new(),
        };

        CStd::register_to_generator_context(&mut context);
        Std::register_to_generator_context(&mut context);

        context
    }

    pub fn new_scope(&self) -> Self {
        Self {
            context: self.context,
            module: Rc::clone(&self.module),
            builder: Rc::clone(&self.builder),
            cstd: Rc::clone(&self.cstd),
            std: Rc::clone(&self.std),
            fn_value_opt: self.fn_value_opt,
            global_variables: Rc::clone(&self.global_variables),
            local_variables: HashMap::new(),
            global_functions: Rc::clone(&self.global_functions),
            local_functions: HashMap::new(),
        }
    }

    pub fn get_variable(&self, identifier: &str) -> Option<Rc<VariableDefinition<'_>>> {
        if let Some(d) = self.local_variables.get(identifier) {
            return Some(Rc::clone(d));
        }
        if let Some(d) = self.global_variables.deref().borrow().get(identifier) {
            return Some(Rc::clone(d));
        }
        None
    }

    pub fn declare_variable(
        &mut self,
        identifier: String,
        definition: VariableDefinition<'a>,
        is_global: bool,
    ) {
        if is_global {
            self.global_variables
                .deref()
                .borrow_mut()
                .insert(identifier, Rc::new(definition));
        } else {
            self.local_variables.insert(identifier, Rc::new(definition));
        }
    }

    pub fn get_function(&self, identifier: &str) -> Option<Rc<FunctionDefinition<'_>>> {
        if let Some(d) = self.local_functions.get(identifier) {
            return Some(Rc::clone(d));
        }
        if let Some(d) = self.global_functions.deref().borrow().get(identifier) {
            return Some(Rc::clone(d));
        }
        None
    }

    pub fn declare_function(
        &mut self,
        identifier: String,
        fn_type: FunctionType<'a>,
        linkage: Option<Linkage>,
        is_global: bool,
    ) -> FunctionValue<'a> {
        let fn_value = self.module.add_function(&identifier, fn_type, linkage);

        let definition = FunctionDefinition {
            identifier,
            fn_type,
            fn_value,
        };

        self.declare_function_from_definition(definition, is_global);

        fn_value
    }

    pub fn declare_function_from_definition(
        &mut self,
        definition: FunctionDefinition<'a>,
        is_global: bool,
    ) {
        if is_global {
            self.global_functions
                .deref()
                .borrow_mut()
                .insert(definition.identifier.clone(), Rc::new(definition));
        } else {
            self.local_functions
                .insert(definition.identifier.clone(), Rc::new(definition));
        }
    }

    pub fn fn_value(&self) -> FunctionValue<'a> {
        // NOTE: This is unsafe... we should handle it better in the future
        self.fn_value_opt.unwrap()
    }
}

pub trait CodeGenerator<'a> {
    type Item;

    fn generate_code(
        self,
        context: &'a CodeGeneratorContext,
        previous: Option<Self::Item>,
    ) -> anyhow::Result<Self::Item>;
}

#[cfg(test)]
mod tests {
    use inkwell::execution_engine::JitFunction;

    use super::*;

    #[test]
    fn get_test() {
        let context = Context::create();
        let context = CodeGeneratorContext::new(&context);

        let jit_engine = context
            .module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();

        let function: JitFunction<unsafe extern "C" fn() -> f64> = unsafe {
            jit_engine
                .get_function("getFloat")
                .expect("getFloat should be defined")
        };

        context
            .module
            .get_functions()
            .for_each(|func| println!("{func}"));

        dbg!(unsafe { function.call() });
    }
}
