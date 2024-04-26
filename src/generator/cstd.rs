use std::rc::Rc;

use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    module::{Linkage, Module},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallSiteValue, FunctionValue, IntValue, PointerValue},
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

fn add_cstd_strcmp<'a>(context: &'a Context, module: &Module<'a>) -> FunctionValue<'a> {
    let fn_type = context.i32_type().fn_type(
        &[
            context.i8_type().ptr_type(AddressSpace::default()).into(),
            context.i8_type().ptr_type(AddressSpace::default()).into(),
        ],
        false,
    );

    module.add_function("strcmp", fn_type, Some(Linkage::External))
}

pub struct CStd<'a> {
    context: &'a Context,
    builder: Rc<Builder<'a>>,
    fn_printf: FunctionValue<'a>,
    fn_scanf: FunctionValue<'a>,
    fn_strcmp: FunctionValue<'a>,
    fn_exit: FunctionValue<'a>,
    fn_sqrt: FunctionValue<'a>,
}

impl<'a> CStd<'a> {
    pub fn new(context: &'a Context, module: Rc<Module<'a>>, builder: Rc<Builder<'a>>) -> Self {
        let fn_printf = add_cstd_printf(context, &module);
        let fn_scanf = add_cstd_scanf(context, &module);
        let fn_strcmp = add_cstd_strcmp(context, &module);
        let fn_exit = add_cstd_exit(context, &module);
        let fn_sqrt = add_cstd_sqrt(context, &module);

        Self {
            context,
            builder,
            fn_printf,
            fn_scanf,
            fn_strcmp,
            fn_exit,
            fn_sqrt,
        }
    }

    pub fn printf(
        &self,
        format: &[u8],
        args: Vec<BasicValueEnum<'a>>,
    ) -> Result<CallSiteValue<'a>, BuilderError> {
        let format = self.context.const_string(format, true);
        let format_ptr = self
            .builder
            .build_alloca(format.get_type(), "printf_format")?;
        self.builder.build_store(format_ptr, format)?;

        let mut final_args = args
            .into_iter()
            .map(|value| value.into())
            .collect::<Vec<BasicMetadataValueEnum>>();

        final_args.insert(0, format_ptr.into());

        self.builder
            .build_call(self.fn_printf, final_args.as_slice(), "call_printf")
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

    pub fn strcmp(
        &self,
        str1: PointerValue<'a>,
        str2: PointerValue<'a>,
    ) -> Result<IntValue<'a>, BuilderError> {
        let call_value = self.builder.build_call(self.fn_strcmp, &[str1.into(), str2.into()], "call_strcmp")?;

        Ok(match call_value.try_as_basic_value().left() {
            Some(value) => {
                self.printf(b"%s == %s -> %d\n", vec![str1.as_basic_value_enum(), str2.as_basic_value_enum(), value])?;
                self.builder.build_int_s_extend(value.into_int_value(), self.context.i64_type(), "i32_to_i64")?
            },
            None => unreachable!("LibC's strcmp should always return a basic value."),
        })
    }

    pub fn exit(&self, value: i32) -> Result<CallSiteValue<'a>, BuilderError> {
        let value = self.context.i32_type().const_int(value as u64, true);
        self.builder
            .build_call(self.fn_exit, &[value.into()], "call_exit")
    }

    pub fn register_to_generator_context(context: &mut CodeGeneratorContext<'a>) {
        context
            .declare_function_from_definition(FunctionDefinition::new(context.cstd.fn_printf), true)
            .expect("CStd functions should never be duplicated.");
        context
            .declare_function_from_definition(FunctionDefinition::new(context.cstd.fn_scanf), true)
            .expect("CStd functions should never be duplicated.");
        context
            .declare_function_from_definition(FunctionDefinition::new(context.cstd.fn_strcmp), true)
            .expect("CStd functions should never be duplicated.");
        context
            .declare_function_from_definition(FunctionDefinition::new(context.cstd.fn_exit), true)
            .expect("CStd functions should never be duplicated.");
        context
            .declare_function_from_definition(FunctionDefinition::new(context.cstd.fn_sqrt), true)
            .expect("CStd functions should never be duplicated.");
    }
}
