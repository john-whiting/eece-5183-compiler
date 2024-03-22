use std::rc::Rc;

use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    module::{Linkage, Module},
    values::{CallSiteValue, FunctionValue, PointerValue},
    AddressSpace,
};

use super::{CodeGeneratorContext, FunctionDefinition};

fn add_cstd_printf<'a>(context: &'a Context, module: &Module<'a>) -> FunctionValue<'a> {
    let fn_type = context.i32_type().fn_type(
        &[context.i8_type().ptr_type(AddressSpace::default()).into()],
        true,
    );

    module.add_function("printf", fn_type, Some(Linkage::External))
}

fn add_cstd_scanf<'a>(context: &'a Context, module: &Module<'a>) -> FunctionValue<'a> {
    let fn_type = context.i32_type().fn_type(
        &[context.i8_type().ptr_type(AddressSpace::default()).into()],
        true,
    );

    module.add_function("scanf", fn_type, Some(Linkage::External))
}

fn add_cstd_exit<'a>(context: &'a Context, module: &Module<'a>) -> FunctionValue<'a> {
    let fn_type = context
        .void_type()
        .fn_type(&[context.i32_type().into()], false);

    module.add_function("exit", fn_type, Some(Linkage::External))
}

fn add_cstd_sqrt<'a>(context: &'a Context, module: &Module<'a>) -> FunctionValue<'a> {
    let fn_type = context
        .f64_type()
        .fn_type(&[context.f64_type().into()], false);

    module.add_function("sqrt", fn_type, Some(Linkage::External))
}

pub struct CStd<'a> {
    context: &'a Context,
    builder: Rc<Builder<'a>>,
    fn_printf: FunctionValue<'a>,
    fn_scanf: FunctionValue<'a>,
    fn_exit: FunctionValue<'a>,
    fn_sqrt: FunctionValue<'a>,
}

impl<'a> CStd<'a> {
    pub fn new(context: &'a Context, module: Rc<Module<'a>>, builder: Rc<Builder<'a>>) -> Self {
        let fn_printf = add_cstd_printf(context, &module);
        let fn_scanf = add_cstd_scanf(context, &module);
        let fn_exit = add_cstd_exit(context, &module);
        let fn_sqrt = add_cstd_sqrt(context, &module);

        Self {
            context,
            builder,
            fn_printf,
            fn_scanf,
            fn_exit,
            fn_sqrt,
        }
    }

    pub fn printf(&self, string: PointerValue<'a>) -> Result<CallSiteValue<'a>, BuilderError> {
        self.builder
            .build_call(self.fn_printf, &[string.into()], "call_printf")
    }

    pub fn scanf(
        &self,
        format: &[u8],
        variable: PointerValue<'a>,
    ) -> Result<CallSiteValue<'a>, BuilderError> {
        let format = self.context.const_string(format, true);
        let format_ptr = self
            .builder
            .build_alloca(format.get_type(), "scanf_format")?;
        self.builder.build_store(format_ptr, format)?;
        self.builder.build_call(
            self.fn_scanf,
            &[format_ptr.into(), variable.into()],
            "call_scanf",
        )
    }

    pub fn exit(&self, value: i32) -> Result<CallSiteValue<'a>, BuilderError> {
        let value = self.context.i32_type().const_int(value as u64, true);
        self.builder
            .build_call(self.fn_exit, &[value.into()], "call_exit")
    }

    pub fn register_to_generator_context(context: &mut CodeGeneratorContext<'a>) {
        context.declare_function_from_definition(
            FunctionDefinition::new(context.cstd.fn_printf),
            true,
        );
        context
            .declare_function_from_definition(FunctionDefinition::new(context.cstd.fn_scanf), true);
        context
            .declare_function_from_definition(FunctionDefinition::new(context.cstd.fn_exit), true);
        context
            .declare_function_from_definition(FunctionDefinition::new(context.cstd.fn_sqrt), true);
    }
}
