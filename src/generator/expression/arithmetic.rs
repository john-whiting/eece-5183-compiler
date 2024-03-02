use inkwell::values::{BasicValue, BasicValueEnum};
use thiserror::Error;

use crate::{
    generator::{
        util::{basic_value_type_casted, BasicValueTypeCasted, CodeGenerationErrorHint},
        CodeGenerator, CodeGeneratorContext,
    },
    parser::expression::ArithmeticNode,
};

#[derive(Error, Debug)]
pub enum ArithmeticNodeCodeGenerationError {
    /// Indicates an issue with the compiler itself.
    #[error("COMPILER CODE GENERATION ERROR: ArithmeticNode::Nop has no expansion, generate_code should not have been called for this term. There may be a bug in the parser or the code generator.")]
    MetaNopExpansion,

    /// Indicates an issue with the compiler itself.
    #[error("COMPILER CODE GENERATION ERROR: Multiple ArithmeticNode::Start nodes found in a chain. There is likely a bug in the parser.")]
    MetaDoubleStart,

    #[error("Unable to add {0} with {1}.")]
    AdditionUnsupportedTypes(String, String),

    #[error("Unable to subtract {1} against {0}.")]
    SubtractionUnsupportedTypes(String, String),
}

impl<'a> CodeGenerator<'a> for ArithmeticNode {
    type Item = BasicValueEnum<'a>;

    fn generate_code(self, context: &'a CodeGeneratorContext) -> anyhow::Result<Self::Item> {
        let (left, right) = match self {
            ArithmeticNode::Start(left, right)
            | ArithmeticNode::Add(left, right)
            | ArithmeticNode::Sub(left, right) => Ok((left, right)),
            ArithmeticNode::Nop => Err(ArithmeticNodeCodeGenerationError::MetaNopExpansion),
        }?;

        let left = left.generate_code(context)?;

        match right.as_ref() {
            ArithmeticNode::Start(_, _) => {
                Err(ArithmeticNodeCodeGenerationError::MetaDoubleStart.into())
            }
            ArithmeticNode::Add(_, _) => {
                let right = right.generate_code(context)?;

                match basic_value_type_casted(context, left, right)? {
                    BasicValueTypeCasted::Integer(lhs, rhs) => Ok(context
                        .builder
                        .build_int_add(lhs, rhs, "addtmp")?
                        .as_basic_value_enum()),
                    BasicValueTypeCasted::Float(lhs, rhs) => Ok(context
                        .builder
                        .build_float_add(lhs, rhs, "addtmp")?
                        .as_basic_value_enum()),
                    BasicValueTypeCasted::Unsupported(lhs, rhs) => {
                        Err(ArithmeticNodeCodeGenerationError::AdditionUnsupportedTypes(
                            lhs.error_hint(),
                            rhs.error_hint(),
                        )
                        .into())
                    }
                }
            }
            ArithmeticNode::Sub(_, _) => {
                let right = right.generate_code(context)?;

                match basic_value_type_casted(context, left, right)? {
                    BasicValueTypeCasted::Integer(lhs, rhs) => Ok(context
                        .builder
                        .build_int_sub(lhs, rhs, "subtmp")?
                        .as_basic_value_enum()),
                    BasicValueTypeCasted::Float(lhs, rhs) => Ok(context
                        .builder
                        .build_float_sub(lhs, rhs, "subtmp")?
                        .as_basic_value_enum()),
                    BasicValueTypeCasted::Unsupported(lhs, rhs) => Err(
                        ArithmeticNodeCodeGenerationError::SubtractionUnsupportedTypes(
                            lhs.error_hint(),
                            rhs.error_hint(),
                        )
                        .into(),
                    ),
                }
            }
            ArithmeticNode::Nop => Ok(left),
        }
    }
}
