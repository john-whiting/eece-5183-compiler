use inkwell::{
    types::{ArrayType, BasicTypeEnum, FloatType, IntType},
    values::{BasicValue, BasicValueEnum, FloatValue, IntValue},
};

use super::CodeGeneratorContext;

pub enum BasicValueTypeCasted<'a> {
    Integer(IntValue<'a>, IntValue<'a>),
    Float(FloatValue<'a>, FloatValue<'a>),
    Unsupported(BasicValueEnum<'a>, BasicValueEnum<'a>),
}
// NOTE: LANGUAGE SEMANTICS | RULE #7
// Support (limited) type-casting
pub fn basic_value_type_casted<'a>(
    context: &'a CodeGeneratorContext,
    left: BasicValueEnum<'a>,
    right: BasicValueEnum<'a>,
) -> anyhow::Result<BasicValueTypeCasted<'a>> {
    match (left, right) {
        (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
            Ok(BasicValueTypeCasted::Integer(lhs, rhs))
        }
        (BasicValueEnum::IntValue(lhs), BasicValueEnum::FloatValue(rhs)) => Ok(
            BasicValueTypeCasted::Float(lhs.const_signed_to_float(context.context.f64_type()), rhs),
        ),
        (BasicValueEnum::FloatValue(lhs), BasicValueEnum::IntValue(rhs)) => Ok(
            BasicValueTypeCasted::Float(lhs, rhs.const_signed_to_float(context.context.f64_type())),
        ),
        (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
            Ok(BasicValueTypeCasted::Float(lhs, rhs))
        }
        (lhs, rhs) => Ok(BasicValueTypeCasted::Unsupported(lhs, rhs)),
    }
}

pub trait CodeGenerationErrorHint {
    fn error_hint(self) -> String;
}

impl<'a> CodeGenerationErrorHint for IntType<'a> {
    fn error_hint(self) -> String {
        let bit_width = self.get_bit_width();

        match bit_width {
            1 => "boolean".to_string(),
            w => format!("i{w}"),
        }
    }
}

impl<'a> CodeGenerationErrorHint for FloatType<'a> {
    fn error_hint(self) -> String {
        let llvm_type = self.print_to_string().to_string();
        match &llvm_type[..] {
            "half" => "f16".to_string(),
            "float" => "f32".to_string(),
            "double" => "f64".to_string(),
            "fp128" => "f128".to_string(),
            _ => llvm_type,
        }
    }
}

impl<'a> CodeGenerationErrorHint for ArrayType<'a> {
    fn error_hint(self) -> String {
        let type_error_hint = self.get_element_type().error_hint();

        format!("{type_error_hint}[]")
    }
}

impl<'a> CodeGenerationErrorHint for BasicTypeEnum<'a> {
    fn error_hint(self) -> String {
        match self {
            BasicTypeEnum::IntType(int_type) => int_type.error_hint(),
            BasicTypeEnum::FloatType(float_type) => float_type.error_hint(),
            BasicTypeEnum::ArrayType(array_type) => array_type.error_hint(),
            BasicTypeEnum::StructType(_) => "struct".to_string(),
            BasicTypeEnum::VectorType(_) => "vector".to_string(),
            BasicTypeEnum::PointerType(_) => "pointer".to_string(),
        }
    }
}

impl<'a> CodeGenerationErrorHint for BasicValueEnum<'a> {
    fn error_hint(self) -> String {
        match self {
            BasicValueEnum::IntValue(int_value) => int_value.get_type().error_hint(),
            BasicValueEnum::FloatValue(float_value) => float_value.get_type().error_hint(),
            BasicValueEnum::ArrayValue(array_value) => array_value.get_type().error_hint(),
            BasicValueEnum::StructValue(_) => "struct".to_string(),
            BasicValueEnum::VectorValue(_) => "vector".to_string(),
            BasicValueEnum::PointerValue(_) => "pointer".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use inkwell::{context::Context, values::BasicValue};

    use super::*;

    #[test]
    fn basic_value_enum_error_hint() {
        let context = Context::create();

        // Int Type Hints
        vec![
            (context.bool_type(), "boolean"),
            (context.i8_type(), "i8"),
            (context.i16_type(), "i16"),
            (context.i32_type(), "i32"),
            (context.i64_type(), "i64"),
            (context.i128_type(), "i128"),
        ]
        .into_iter()
        .for_each(|(i_type, result)| {
            assert_eq!(
                i_type.const_zero().as_basic_value_enum().error_hint(),
                result.to_string()
            )
        });

        // Float Type Hints
        vec![
            (context.f16_type(), "f16"),
            (context.f32_type(), "f32"),
            (context.f64_type(), "f64"),
            (context.f128_type(), "f128"),
        ]
        .into_iter()
        .for_each(|(i_type, result)| {
            assert_eq!(
                i_type.const_zero().as_basic_value_enum().error_hint(),
                result.to_string()
            )
        });

        assert_eq!(
            context
                .i64_type()
                .const_array(&[])
                .as_basic_value_enum()
                .error_hint(),
            "i64[]".to_string()
        );
        assert_eq!(
            context
                .f64_type()
                .const_array(&[])
                .as_basic_value_enum()
                .error_hint(),
            "f64[]".to_string()
        );
        assert_eq!(
            context
                .const_string("hello world".as_bytes(), true)
                .as_basic_value_enum()
                .error_hint(),
            "i8[]".to_string()
        );
    }
}
