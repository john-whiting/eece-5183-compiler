use inkwell::values::{BasicValue, BasicValueEnum};
use thiserror::Error;

use crate::{
    generator::{
        util::{basic_value_type_casted, BasicValueTypeCasted, CodeGenerationErrorHint},
        CodeGenerator, CodeGeneratorContext,
    },
    parser::expression::TermNode,
};

#[derive(Error, Debug)]
pub enum TermNodeCodeGenerationError {
    /// Indicates an issue with the compiler itself.
    #[error("COMPILER CODE GENERATION ERROR: TermNode::Nop has no expansion, generate_code should not have been called for this term. There may be a bug in the parser or the code generator.")]
    MetaNopExpansion,

    /// Indicates an issue with the compiler itself.
    #[error("COMPILER CODE GENERATION ERROR: Multiple TermNode::Start nodes found in a chain. There is likely a bug in the parser.")]
    MetaDoubleStart,

    #[error("Unable to multiply {0} with {1}.")]
    MultiplicationUnsupportedTypes(String, String),

    #[error("Unable to divide {1} against {0}.")]
    DivisionUnsupportedTypes(String, String),
}

impl<'a> CodeGenerator<'a> for TermNode {
    type Item = BasicValueEnum<'a>;

    fn generate_code(self, context: &'a CodeGeneratorContext) -> anyhow::Result<Self::Item> {
        let (left, right) = match self {
            TermNode::Start(left, right)
            | TermNode::Multiply(left, right)
            | TermNode::Divide(left, right) => Ok((left, right)),
            TermNode::Nop => Err(TermNodeCodeGenerationError::MetaNopExpansion),
        }?;

        let left = left.generate_code(context)?;

        match right.as_ref() {
            TermNode::Start(_, _) => Err(TermNodeCodeGenerationError::MetaDoubleStart.into()),
            TermNode::Multiply(_, _) => {
                let right = right.generate_code(context)?;

                match basic_value_type_casted(context, left, right)? {
                    BasicValueTypeCasted::Integer(lhs, rhs) => Ok(context
                        .builder
                        .build_int_mul(lhs, rhs, "multmp")?
                        .as_basic_value_enum()),
                    BasicValueTypeCasted::Float(lhs, rhs) => Ok(context
                        .builder
                        .build_float_mul(lhs, rhs, "multmp")?
                        .as_basic_value_enum()),
                    BasicValueTypeCasted::Unsupported(lhs, rhs) => {
                        Err(TermNodeCodeGenerationError::MultiplicationUnsupportedTypes(
                            lhs.error_hint(),
                            rhs.error_hint(),
                        )
                        .into())
                    }
                }
            }
            TermNode::Divide(_, _) => {
                let right = right.generate_code(context)?;

                match basic_value_type_casted(context, left, right)? {
                    BasicValueTypeCasted::Integer(lhs, rhs) => Ok(context
                        .builder
                        .build_int_signed_div(lhs, rhs, "divtmp")?
                        .as_basic_value_enum()),
                    BasicValueTypeCasted::Float(lhs, rhs) => Ok(context
                        .builder
                        .build_float_div(lhs, rhs, "divtmp")?
                        .as_basic_value_enum()),
                    BasicValueTypeCasted::Unsupported(lhs, rhs) => {
                        Err(TermNodeCodeGenerationError::DivisionUnsupportedTypes(
                            lhs.error_hint(),
                            rhs.error_hint(),
                        )
                        .into())
                    }
                }
            }
            TermNode::Nop => Ok(left),
        }
    }
}
