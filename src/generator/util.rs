use inkwell::values::{BasicValueEnum, FloatValue, IntValue};

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

impl<'a> CodeGenerationErrorHint for BasicValueEnum<'a> {
    fn error_hint(self) -> String {
        match self {
            BasicValueEnum::IntValue(_) => "i64".to_string(),
            BasicValueEnum::FloatValue(_) => "f64".to_string(),
            BasicValueEnum::ArrayValue(_) => "array".to_string(),
            BasicValueEnum::StructValue(_) => "struct".to_string(),
            BasicValueEnum::VectorValue(_) => "vector".to_string(),
            BasicValueEnum::PointerValue(_) => "pointer".to_string(),
        }
    }
}
