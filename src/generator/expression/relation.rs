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
    #[error("COMPILER CODE GENERATION ERROR: Invalid previous result. There is likely a bug in the code generator or parser.")]
    MetaInvalidPrevious,

    #[error("Comparison of types {0} and {1} is not supported.")]
    ComparisonUnsupportedTypes(String, String),
}

impl<'a> CodeGenerator<'a> for RelationNode {
    type Item = BasicValueEnum<'a>;

    fn generate_code(
        self,
        context: &'a CodeGeneratorContext,
        previous: Option<Self::Item>,
    ) -> anyhow::Result<Self::Item> {
        let (int_predicate, float_predicate, current, next, prev) = match (self, previous) {
            (RelationNode::Start(cur, next), None) => {
                return next.generate_code(context, Some(cur.generate_code(context, None)?));
            }
            (RelationNode::LessThan(cur, next), Some(prev)) => {
                (IntPredicate::SLT, FloatPredicate::OLT, cur, next, prev)
            }
            (RelationNode::LessThanEqual(cur, next), Some(prev)) => {
                (IntPredicate::SLE, FloatPredicate::OLE, cur, next, prev)
            }
            (RelationNode::GreaterThan(cur, next), Some(prev)) => {
                (IntPredicate::SGT, FloatPredicate::OGT, cur, next, prev)
            }
            (RelationNode::GreaterThanEqual(cur, next), Some(prev)) => {
                (IntPredicate::SGE, FloatPredicate::OGE, cur, next, prev)
            }
            (RelationNode::Equal(cur, next), Some(prev)) => {
                (IntPredicate::EQ, FloatPredicate::OEQ, cur, next, prev)
            }
            (RelationNode::NotEqual(cur, next), Some(prev)) => {
                (IntPredicate::NE, FloatPredicate::ONE, cur, next, prev)
            }
            (RelationNode::Nop, Some(previous)) => return Ok(previous),
            (_, _) => return Err(RelationNodeCodeGenerationError::MetaInvalidPrevious.into()),
        };

        let current = current.generate_code(context, None)?;
        let result = match basic_value_type_casted(context, prev, current)? {
            BasicValueTypeCasted::Integer(lhs, rhs) => context
                .builder
                .build_int_compare(int_predicate, lhs, rhs, "cmptmp")?
                .as_basic_value_enum(),
            BasicValueTypeCasted::Float(lhs, rhs) => context
                .builder
                .build_float_compare(float_predicate, lhs, rhs, "cmptmp")?
                .as_basic_value_enum(),
            BasicValueTypeCasted::Unsupported(lhs, rhs) => {
                return Err(RelationNodeCodeGenerationError::ComparisonUnsupportedTypes(
                    lhs.error_hint(),
                    rhs.error_hint(),
                )
                .into())
            }
        };

        next.generate_code(context, Some(result))
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

                let result = relation_node.generate_code(&context, None);

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
