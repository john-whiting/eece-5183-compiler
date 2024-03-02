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

#[cfg(test)]
mod tests {
    use inkwell::context::Context;

    use crate::parser::{
        expression::{FactorNode, TermNode},
        general::NumberNode,
    };

    use super::*;

    macro_rules! comparison_tests {
        ($context: ident, $comparison: expr, $low_high: expr, $equal: expr, $high_low: expr) => {
            vec![
                (
                    RelationNode::Start(
                        TermNode::Start(
                            FactorNode::Number(NumberNode::IntegerLiteral(3)),
                            Box::new(TermNode::Nop),
                        ),
                        Box::new($comparison(
                            TermNode::Start(
                                FactorNode::Number(NumberNode::IntegerLiteral(4)),
                                Box::new(TermNode::Nop),
                            ),
                            Box::new(RelationNode::Nop),
                        )),
                    ),
                    $context.context.bool_type().const_int($low_high, false),
                ),
                (
                    RelationNode::Start(
                        TermNode::Start(
                            FactorNode::Number(NumberNode::IntegerLiteral(4)),
                            Box::new(TermNode::Nop),
                        ),
                        Box::new($comparison(
                            TermNode::Start(
                                FactorNode::Number(NumberNode::IntegerLiteral(4)),
                                Box::new(TermNode::Nop),
                            ),
                            Box::new(RelationNode::Nop),
                        )),
                    ),
                    $context.context.bool_type().const_int($equal, false),
                ),
                (
                    RelationNode::Start(
                        TermNode::Start(
                            FactorNode::Number(NumberNode::IntegerLiteral(5)),
                            Box::new(TermNode::Nop),
                        ),
                        Box::new($comparison(
                            TermNode::Start(
                                FactorNode::Number(NumberNode::IntegerLiteral(4)),
                                Box::new(TermNode::Nop),
                            ),
                            Box::new(RelationNode::Nop),
                        )),
                    ),
                    $context.context.bool_type().const_int($high_low, false),
                ),
            ]
        };
    }

    #[test]
    fn relation_node_generation() {
        let outside_context = Context::create();
        let context = CodeGeneratorContext::new(&outside_context);
        let function_type = context.context.void_type().fn_type(&[], false);
        let function_value = context.module.add_function("main", function_type, None);
        let function_entry_block = context.context.append_basic_block(function_value, "entry");
        context.builder.position_at_end(function_entry_block);

        let tests = vec![
            comparison_tests!(context, RelationNode::LessThan, 1, 0, 0),
            comparison_tests!(context, RelationNode::LessThanEqual, 1, 1, 0),
            comparison_tests!(context, RelationNode::GreaterThan, 0, 0, 1),
            comparison_tests!(context, RelationNode::GreaterThanEqual, 0, 1, 1),
            comparison_tests!(context, RelationNode::Equal, 0, 1, 0),
            comparison_tests!(context, RelationNode::NotEqual, 1, 0, 1),
        ];

        let failed_tests: Vec<_> = tests
            .into_iter()
            .flatten()
            .filter_map(|(relation_node, expected_result)| {
                let formatted_relation_node = format!("{relation_node:?}");

                let result = relation_node.generate_code(&context);

                match result {
                    Ok(result) => {
                        if result != expected_result {
                            Some(format!(
                                "\t{formatted_relation_node}: {result} != {expected_result}"
                            ))
                        } else {
                            None
                        }
                    }
                    Err(e) => Some(format!(
                        "\t{formatted_relation_node} encountered an error on code generation!\n{e}"
                    )),
                }
            })
            .collect();

        if !failed_tests.is_empty() {
            let formatted = failed_tests.join("\n");
            panic!("FactorNode Code Generation test(s) failed:\n{formatted}");
        }
    }
}
