use std::rc::Rc;

use inkwell::{
    builder::{Builder, BuilderError},
    context::Context,
    module::Module,
    values::FunctionValue,
    AddressSpace, IntPredicate,
};

use super::{cstd::CStd, CodeGeneratorContext, FunctionDefinition};

fn add_std_get_bool<'a>(
    cstd: &CStd<'a>,
    context: &'a Context,
    module: &Module<'a>,
    builder: &Builder<'a>,
) -> Result<FunctionValue<'a>, BuilderError> {
    let fn_type = context.bool_type().fn_type(&[], false);

    let fn_value = module.add_function("getbool", fn_type, None);

    let basic_block = context.append_basic_block(fn_value, "entry");
    builder.position_at_end(basic_block);

    let ptr_type = context.i32_type();

    let bool_variable = builder.build_alloca(ptr_type, "local_integer")?;

    cstd.scanf(b"%i", bool_variable)?;

    let value = builder.build_load(ptr_type, bool_variable, "get_integer")?;

    let value = match value {
        inkwell::values::BasicValueEnum::IntValue(value) => value,
        _ => unreachable!("getBool: The variable is known here to be an integer."),
    };

    let boolean_result = builder.build_int_compare(
        IntPredicate::NE,
        value,
        ptr_type.const_zero(),
        "cast_int_to_bool",
    )?;

    builder.build_return(Some(&boolean_result))?;

    Ok(fn_value)
}

fn add_std_get_integer<'a>(
    cstd: &CStd<'a>,
    context: &'a Context,
    module: &Module<'a>,
    builder: &Builder<'a>,
) -> Result<FunctionValue<'a>, BuilderError> {
    let fn_type = context.i64_type().fn_type(&[], false);

    let fn_value = module.add_function("getinteger", fn_type, None);

    let basic_block = context.append_basic_block(fn_value, "entry");
    builder.position_at_end(basic_block);

    let ptr_type = context.i32_type();

    let int_variable = builder.build_alloca(ptr_type, "local_integer")?;

    cstd.scanf(b"%i", int_variable)?;

    let value = builder.build_load(ptr_type, int_variable, "get_integer")?;

    let value = builder.build_int_s_extend_or_bit_cast(
        value.into_int_value(),
        fn_type.get_return_type().unwrap().into_int_type(),
        "i32_to_i64",
    )?;
    builder.build_return(Some(&value))?;

    Ok(fn_value)
}

fn add_std_get_float<'a>(
    cstd: &CStd<'a>,
    context: &'a Context,
    module: &Module<'a>,
    builder: &Builder<'a>,
) -> Result<FunctionValue<'a>, BuilderError> {
    let fn_type = context.f64_type().fn_type(&[], false);

    let fn_value = module.add_function("getfloat", fn_type, None);

    let basic_block = context.append_basic_block(fn_value, "entry");
    builder.position_at_end(basic_block);

    let ptr_type = context.f32_type();

    let float_variable = builder.build_alloca(ptr_type, "local_float")?;

    cstd.scanf(b"%f", float_variable)?;

    let value = builder.build_load(ptr_type, float_variable, "get_float")?;

    let value = builder.build_float_cast(
        value.into_float_value(),
        fn_type.get_return_type().unwrap().into_float_type(),
        "f32_to_f64",
    )?;
    builder.build_return(Some(&value))?;

    Ok(fn_value)
}

fn add_std_get_string<'a>(
    cstd: &CStd<'a>,
    context: &'a Context,
    module: &Module<'a>,
    builder: &Builder<'a>,
) -> Result<FunctionValue<'a>, BuilderError> {
    let fn_type = context
        .i8_type()
        .ptr_type(AddressSpace::default())
        .fn_type(&[], false);

    let fn_value = module.add_function("getstring", fn_type, None);

    let basic_block = context.append_basic_block(fn_value, "entry");
    builder.position_at_end(basic_block);

    // NOTE: Hardcoded max read size of 1024 characters
    let ptr_type = context.i8_type().array_type(1024);

    let str_variable = builder.build_alloca(ptr_type, "local_string")?;

    cstd.scanf(b"%s", str_variable)?;

    builder.build_return(Some(&str_variable))?;

    Ok(fn_value)
}

fn add_std_put_bool<'a>(
    cstd: &CStd<'a>,
    context: &'a Context,
    module: &Module<'a>,
    builder: &Builder<'a>,
) -> Result<FunctionValue<'a>, BuilderError> {
    let fn_type = context
        .bool_type()
        .fn_type(&[context.bool_type().into()], false);

    let fn_value = module.add_function("putbool", fn_type, None);

    let basic_block = context.append_basic_block(fn_value, "entry");
    builder.position_at_end(basic_block);

    let arg0 = fn_value.get_first_param().unwrap();

    cstd.printf(b"%d\n", vec![arg0])?;

    builder.build_return(Some(&context.bool_type().const_int(1, true)))?;

    Ok(fn_value)
}

fn add_std_put_integer<'a>(
    cstd: &CStd<'a>,
    context: &'a Context,
    module: &Module<'a>,
    builder: &Builder<'a>,
) -> Result<FunctionValue<'a>, BuilderError> {
    let fn_type = context
        .bool_type()
        .fn_type(&[context.i64_type().into()], false);

    let fn_value = module.add_function("putinteger", fn_type, None);

    let basic_block = context.append_basic_block(fn_value, "entry");
    builder.position_at_end(basic_block);

    let arg0 = fn_value.get_first_param().unwrap();

    cstd.printf(b"%d\n", vec![arg0])?;

    builder.build_return(Some(&context.bool_type().const_int(1, true)))?;

    Ok(fn_value)
}

fn add_std_put_float<'a>(
    cstd: &CStd<'a>,
    context: &'a Context,
    module: &Module<'a>,
    builder: &Builder<'a>,
) -> Result<FunctionValue<'a>, BuilderError> {
    let fn_type = context
        .bool_type()
        .fn_type(&[context.f64_type().into()], false);

    let fn_value = module.add_function("putfloat", fn_type, None);

    let basic_block = context.append_basic_block(fn_value, "entry");
    builder.position_at_end(basic_block);

    let arg0 = fn_value.get_first_param().unwrap();

    cstd.printf(b"%f\n", vec![arg0])?;

    builder.build_return(Some(&context.bool_type().const_int(1, true)))?;

    Ok(fn_value)
}

fn add_std_put_string<'a>(
    cstd: &CStd<'a>,
    context: &'a Context,
    module: &Module<'a>,
    builder: &Builder<'a>,
) -> Result<FunctionValue<'a>, BuilderError> {
    let fn_type = context.bool_type().fn_type(
        &[context.i8_type().ptr_type(AddressSpace::default()).into()],
        false,
    );

    let fn_value = module.add_function("putstring", fn_type, None);

    let basic_block = context.append_basic_block(fn_value, "entry");
    builder.position_at_end(basic_block);

    let arg0 = fn_value.get_first_param().unwrap();

    cstd.printf(b"%s", vec![arg0])?;

    builder.build_return(Some(&context.bool_type().const_int(1, true)))?;

    Ok(fn_value)
}

pub struct Std<'a> {
    pub fn_get_bool: FunctionValue<'a>,
    pub fn_get_integer: FunctionValue<'a>,
    pub fn_get_float: FunctionValue<'a>,
    pub fn_get_string: FunctionValue<'a>,
    pub fn_put_bool: FunctionValue<'a>,
    pub fn_put_integer: FunctionValue<'a>,
    pub fn_put_float: FunctionValue<'a>,
    pub fn_put_string: FunctionValue<'a>,
}

impl<'a> Std<'a> {
    pub fn new(
        cstd: &CStd<'a>,
        context: &'a Context,
        module: Rc<Module<'a>>,
        builder: Rc<Builder<'a>>,
    ) -> Self {
        let fn_get_bool = add_std_get_bool(cstd, context, &module, &builder)
            .expect("getBool std function is constant");
        let fn_get_integer = add_std_get_integer(cstd, context, &module, &builder)
            .expect("getInteger std function is constant");
        let fn_get_float = add_std_get_float(cstd, context, &module, &builder)
            .expect("getFloat std function is constant");
        let fn_get_string = add_std_get_string(cstd, context, &module, &builder)
            .expect("getString std function is constant");
        let fn_put_bool = add_std_put_bool(cstd, context, &module, &builder)
            .expect("putBool std function is constant");
        let fn_put_integer = add_std_put_integer(cstd, context, &module, &builder)
            .expect("putInteger std function is constant");
        let fn_put_float = add_std_put_float(cstd, context, &module, &builder)
            .expect("putFloat std function is constant");
        let fn_put_string = add_std_put_string(cstd, context, &module, &builder)
            .expect("putStr std function is constant");

        Self {
            fn_get_bool,
            fn_get_integer,
            fn_get_float,
            fn_get_string,
            fn_put_bool,
            fn_put_integer,
            fn_put_float,
            fn_put_string,
        }
    }

    pub fn register_to_generator_context(context: &mut CodeGeneratorContext<'a>) {
        context
            .declare_function_from_definition(
                FunctionDefinition::new(context.std.fn_get_bool),
                true,
            )
            .expect("Std functions should never be duplicated.");
        context
            .declare_function_from_definition(
                FunctionDefinition::new(context.std.fn_get_integer),
                true,
            )
            .expect("Std functions should never be duplicated.");
        context
            .declare_function_from_definition(
                FunctionDefinition::new(context.std.fn_get_float),
                true,
            )
            .expect("Std functions should never be duplicated.");
        context
            .declare_function_from_definition(
                FunctionDefinition::new(context.std.fn_get_string),
                true,
            )
            .expect("Std functions should never be duplicated.");
        context
            .declare_function_from_definition(
                FunctionDefinition::new(context.std.fn_put_bool),
                true,
            )
            .expect("Std functions should never be duplicated.");
        context
            .declare_function_from_definition(
                FunctionDefinition::new(context.std.fn_put_integer),
                true,
            )
            .expect("Std functions should never be duplicated.");
        context
            .declare_function_from_definition(
                FunctionDefinition::new(context.std.fn_put_float),
                true,
            )
            .expect("Std functions should never be duplicated.");
        context
            .declare_function_from_definition(
                FunctionDefinition::new(context.std.fn_put_string),
                true,
            )
            .expect("Std functions should never be duplicated.");
    }
}
