use inkwell::values::{BasicValue, BasicValueEnum};
use thiserror::Error;

use crate::parser::expression::{ExpressionData, ExpressionNode};

use super::{
    util::{basic_value_type_casted, BasicValueTypeCasted, CodeGenerationErrorHint},
    CodeGenerator, CodeGeneratorContext,
};

mod arithmetic;
mod factor;
mod relation;
mod term;

#[derive(Error, Debug)]
pub enum ExpressionNodeCodeGenerationError {
    /// Indicates an issue with the compiler itself.
    #[error("COMPILER CODE GENERATION ERROR: TermNode::Nop has no expansion, generate_code should not have been called for this term. There may be a bug in the parser or the code generator.")]
    MetaNopExpansion,

    /// Indicates an issue with the compiler itself.
    #[error("COMPILER CODE GENERATION ERROR: Multiple TermNode::Start nodes found in a chain. There is likely a bug in the parser.")]
    MetaDoubleStart,

    #[error("Unable to AND {0} with {1}.")]
    BitwiseAndUnsupportedTypes(String, String),

    #[error("Unable to OR {0} with {1}.")]
    BitwiseOrUnsupportedTypes(String, String),
}

impl<'a> CodeGenerator<'a> for ExpressionNode {
    type Item = BasicValueEnum<'a>;

    fn generate_code(self, context: &'a CodeGeneratorContext) -> anyhow::Result<Self::Item> {
        let (left, right) = match self {
            ExpressionNode::Start(left, right) => {
                let left = match left {
                    ExpressionData::Not(_) => {
                        todo!("NOT needs support still... boolean vs. bitwise not")
                    }
                    ExpressionData::Nop(node) => node.generate_code(context)?,
                };
                Ok((left, right))
            }
            ExpressionNode::And(left, right) | ExpressionNode::Or(left, right) => {
                Ok((left.generate_code(context)?, right))
            }
            ExpressionNode::Nop => Err(ExpressionNodeCodeGenerationError::MetaNopExpansion),
        }?;

        match right.as_ref() {
            ExpressionNode::Start(_, _) => {
                Err(ExpressionNodeCodeGenerationError::MetaDoubleStart.into())
            }
            ExpressionNode::And(_, _) => {
                let right = right.generate_code(context)?;

                match basic_value_type_casted(context, left, right)? {
                    BasicValueTypeCasted::Integer(lhs, rhs) => Ok(context
                        .builder
                        .build_and(lhs, rhs, "andtmp")?
                        .as_basic_value_enum()),
                    BasicValueTypeCasted::Float(_, _) | BasicValueTypeCasted::Unsupported(_, _) => {
                        Err(
                            ExpressionNodeCodeGenerationError::BitwiseAndUnsupportedTypes(
                                left.error_hint(),
                                right.error_hint(),
                            )
                            .into(),
                        )
                    }
                }
            }
            ExpressionNode::Or(_, _) => {
                let right = right.generate_code(context)?;

                match basic_value_type_casted(context, left, right)? {
                    BasicValueTypeCasted::Integer(lhs, rhs) => Ok(context
                        .builder
                        .build_or(lhs, rhs, "ortmp")?
                        .as_basic_value_enum()),
                    BasicValueTypeCasted::Float(_, _) | BasicValueTypeCasted::Unsupported(_, _) => {
                        Err(
                            ExpressionNodeCodeGenerationError::BitwiseOrUnsupportedTypes(
                                left.error_hint(),
                                right.error_hint(),
                            )
                            .into(),
                        )
                    }
                }
            }
            ExpressionNode::Nop => Ok(left),
        }
    }
}
