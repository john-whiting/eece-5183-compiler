use inkwell::{
    values::{BasicValue, BasicValueEnum},
    FloatPredicate, IntPredicate,
};
use thiserror::Error;

use crate::{
    generator::{
        util::{basic_value_type_casted, BasicValueTypeCasted, CodeGenerationErrorHint},
        CodeGenerator, CodeGeneratorContext,
    },
    parser::expression::RelationNode,
};

#[derive(Error, Debug)]
pub enum RelationNodeCodeGenerationError {
    /// Indicates an issue with the compiler itself.
    #[error("COMPILER CODE GENERATION ERROR: RelationNode::Nop has no expansion, generate_code should not have been called for this term. There may be a bug in the parser or the code generator.")]
    MetaNopExpansion,

    /// Indicates an issue with the compiler itself.
    #[error("COMPILER CODE GENERATION ERROR: Multiple RelationNode::Start nodes found in a chain. There is likely a bug in the parser.")]
    MetaDoubleStart,

    #[error("Comparison of types {0} and {1} is not supported.")]
    ComparisonUnsupportedTypes(String, String),
}

impl<'a> CodeGenerator<'a> for RelationNode {
    type Item = BasicValueEnum<'a>;

    fn generate_code(self, context: &'a CodeGeneratorContext) -> anyhow::Result<Self::Item> {
        let (left, right) = match self {
            RelationNode::Start(left, right)
            | RelationNode::LessThan(left, right)
            | RelationNode::LessThanEqual(left, right)
            | RelationNode::GreaterThan(left, right)
            | RelationNode::GreaterThanEqual(left, right)
            | RelationNode::Equal(left, right)
            | RelationNode::NotEqual(left, right) => Ok((left, right)),
            RelationNode::Nop => Err(RelationNodeCodeGenerationError::MetaNopExpansion),
        }?;

        let (int_predicate, float_predicate) = match right.as_ref() {
            RelationNode::Start(_, _) => {
                return Err(RelationNodeCodeGenerationError::MetaDoubleStart.into())
            }
            RelationNode::LessThan(_, _) => (IntPredicate::SLT, FloatPredicate::OLT),
            RelationNode::LessThanEqual(_, _) => (IntPredicate::SLE, FloatPredicate::OLE),
            RelationNode::GreaterThan(_, _) => (IntPredicate::SGT, FloatPredicate::OGT),
            RelationNode::GreaterThanEqual(_, _) => (IntPredicate::SGE, FloatPredicate::OGE),
            RelationNode::Equal(_, _) => (IntPredicate::EQ, FloatPredicate::OEQ),
            RelationNode::NotEqual(_, _) => (IntPredicate::NE, FloatPredicate::ONE),
            RelationNode::Nop => return left.generate_code(context),
        };

        let left = left.generate_code(context)?;
        let right = right.generate_code(context)?;

        match basic_value_type_casted(context, left, right)? {
            BasicValueTypeCasted::Integer(lhs, rhs) => Ok(context
                .builder
                .build_int_compare(int_predicate, lhs, rhs, "cmptmp")?
                .as_basic_value_enum()),
            BasicValueTypeCasted::Float(lhs, rhs) => Ok(context
                .builder
                .build_float_compare(float_predicate, lhs, rhs, "cmptmp")?
                .as_basic_value_enum()),
            BasicValueTypeCasted::Unsupported(lhs, rhs) => {
                Err(RelationNodeCodeGenerationError::ComparisonUnsupportedTypes(
                    lhs.error_hint(),
                    rhs.error_hint(),
                )
                .into())
            }
        }
    }
}
