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
    #[error("COMPILER CODE GENERATION ERROR: Invalid previous result. There is likely a bug in the code generator or parser.")]
    MetaInvalidPrevious,

    #[error("Unable to perform NOT on {0}.")]
    BitwiseNotUnsupportedType(String),

    #[error("Unable to AND {0} with {1}.")]
    BitwiseAndUnsupportedTypes(String, String),

    #[error("Unable to OR {0} with {1}.")]
    BitwiseOrUnsupportedTypes(String, String),
}

impl<'a> CodeGenerator<'a> for ExpressionNode {
    type Item = BasicValueEnum<'a>;

    fn generate_code(
        &self,
        context: &'a CodeGeneratorContext,
        previous: Option<Self::Item>,
    ) -> anyhow::Result<Self::Item> {
        let (result, next) = match (self, previous) {
            (ExpressionNode::Start(current, next), None) => match current {
                ExpressionData::Not(current) => {
                    let current = current.generate_code(context, None)?;
                    let result = match current {
                        BasicValueEnum::IntValue(int_value) => context
                            .builder
                            .build_not(int_value, "tmpnot")?
                            .as_basic_value_enum(),
                        x => {
                            return Err(
                                ExpressionNodeCodeGenerationError::BitwiseNotUnsupportedType(
                                    x.error_hint(),
                                )
                                .into(),
                            )
                        }
                    };

                    (result, next)
                }
                ExpressionData::Nop(current) => (current.generate_code(context, None)?, next),
            },
            (ExpressionNode::And(current, next), Some(previous)) => {
                let current = current.generate_code(context, None)?;
                let result = match basic_value_type_casted(context, previous, current)? {
                    BasicValueTypeCasted::Integer(lhs, rhs) => context
                        .builder
                        .build_and(lhs, rhs, "andtmp")?
                        .as_basic_value_enum(),
                    BasicValueTypeCasted::Float(_, _) | BasicValueTypeCasted::Unsupported(_, _) => {
                        return Err(
                            ExpressionNodeCodeGenerationError::BitwiseAndUnsupportedTypes(
                                previous.error_hint(),
                                current.error_hint(),
                            )
                            .into(),
                        )
                    }
                };

                (result, next)
            }
            (ExpressionNode::Or(current, next), Some(previous)) => {
                let current = current.generate_code(context, None)?;
                let result = match basic_value_type_casted(context, previous, current)? {
                    BasicValueTypeCasted::Integer(lhs, rhs) => context
                        .builder
                        .build_or(lhs, rhs, "ortmp")?
                        .as_basic_value_enum(),
                    BasicValueTypeCasted::Float(_, _) | BasicValueTypeCasted::Unsupported(_, _) => {
                        return Err(
                            ExpressionNodeCodeGenerationError::BitwiseOrUnsupportedTypes(
                                previous.error_hint(),
                                current.error_hint(),
                            )
                            .into(),
                        )
                    }
                };

                (result, next)
            }
            (ExpressionNode::Nop, Some(previous)) => return Ok(previous),
            (_, _) => return Err(ExpressionNodeCodeGenerationError::MetaInvalidPrevious.into()),
        };

        next.generate_code(context, Some(result))
    }
}

#[cfg(test)]
mod tests {
    use inkwell::context::Context;

    use crate::parser::{
        expression::{ArithmeticNode, FactorNode, RelationNode, TermNode},
        general::NumberNode,
    };

    use super::*;

    macro_rules! simple_expression_node {
        (!$factor_node: expr) => {
            ExpressionNode::Start(
                ExpressionData::Not(ArithmeticNode::Start(
                    RelationNode::Start(
                        TermNode::Start($factor_node, Box::new(TermNode::Nop)),
                        Box::new(RelationNode::Nop),
                    ),
                    Box::new(ArithmeticNode::Nop),
                )),
                Box::new(ExpressionNode::Nop),
            )
        };
        (($factor_node: expr) & ($factor_node_2: expr)) => {
            ExpressionNode::Start(
                ExpressionData::Nop(ArithmeticNode::Start(
                    RelationNode::Start(
                        TermNode::Start($factor_node, Box::new(TermNode::Nop)),
                        Box::new(RelationNode::Nop),
                    ),
                    Box::new(ArithmeticNode::Nop),
                )),
                Box::new(ExpressionNode::And(
                    ArithmeticNode::Start(
                        RelationNode::Start(
                            TermNode::Start($factor_node_2, Box::new(TermNode::Nop)),
                            Box::new(RelationNode::Nop),
                        ),
                        Box::new(ArithmeticNode::Nop),
                    ),
                    Box::new(ExpressionNode::Nop),
                )),
            )
        };
        (($factor_node: expr) | ($factor_node_2: expr)) => {
            ExpressionNode::Start(
                ExpressionData::Nop(ArithmeticNode::Start(
                    RelationNode::Start(
                        TermNode::Start($factor_node, Box::new(TermNode::Nop)),
                        Box::new(RelationNode::Nop),
                    ),
                    Box::new(ArithmeticNode::Nop),
                )),
                Box::new(ExpressionNode::Or(
                    ArithmeticNode::Start(
                        RelationNode::Start(
                            TermNode::Start($factor_node_2, Box::new(TermNode::Nop)),
                            Box::new(RelationNode::Nop),
                        ),
                        Box::new(ArithmeticNode::Nop),
                    ),
                    Box::new(ExpressionNode::Nop),
                )),
            )
        };
        ($factor_node: expr) => {
            ExpressionNode::Start(
                ExpressionData::Nop(ArithmeticNode::Start(
                    RelationNode::Start(
                        TermNode::Start($factor_node, Box::new(TermNode::Nop)),
                        Box::new(RelationNode::Nop),
                    ),
                    Box::new(ArithmeticNode::Nop),
                )),
                Box::new(ExpressionNode::Nop),
            )
        };
    }

    #[test]
    fn expression_node_generation() {
        let outside_context = Context::create();
        let context = CodeGeneratorContext::new(&outside_context);
        let function_type = context.context.void_type().fn_type(&[], false);
        let function_value = context.module.add_function("main", function_type, None);
        let function_entry_block = context.context.append_basic_block(function_value, "entry");
        context.builder.position_at_end(function_entry_block);

        let tests = vec![
            (
                simple_expression_node!(!FactorNode::False),
                context.context.bool_type().const_int(1, false),
            ),
            (
                simple_expression_node!(!FactorNode::True),
                context.context.bool_type().const_int(0, false),
            ),
            (
                simple_expression_node!((FactorNode::True) & (FactorNode::True)),
                context.context.bool_type().const_int(1, false),
            ),
            (
                simple_expression_node!((FactorNode::True) & (FactorNode::False)),
                context.context.bool_type().const_int(0, false),
            ),
            (
                simple_expression_node!((FactorNode::False) & (FactorNode::True)),
                context.context.bool_type().const_int(0, false),
            ),
            (
                simple_expression_node!((FactorNode::False) & (FactorNode::False)),
                context.context.bool_type().const_int(0, false),
            ),
            (
                simple_expression_node!((FactorNode::True) | (FactorNode::True)),
                context.context.bool_type().const_int(1, false),
            ),
            (
                simple_expression_node!((FactorNode::True) | (FactorNode::False)),
                context.context.bool_type().const_int(1, false),
            ),
            (
                simple_expression_node!((FactorNode::False) | (FactorNode::True)),
                context.context.bool_type().const_int(1, false),
            ),
            (
                simple_expression_node!((FactorNode::False) | (FactorNode::False)),
                context.context.bool_type().const_int(0, false),
            ),
            (
                simple_expression_node!(!FactorNode::Number(NumberNode::IntegerLiteral(1))),
                context
                    .context
                    .i64_type()
                    .const_int(0xfffffffffffffffe, true),
            ),
            (
                simple_expression_node!(!FactorNode::Number(NumberNode::IntegerLiteral(0))),
                context
                    .context
                    .i64_type()
                    .const_int(0xffffffffffffffff, true),
            ),
            (
                simple_expression_node!(
                    (FactorNode::Number(NumberNode::IntegerLiteral(12)))
                        & (FactorNode::Number(NumberNode::IntegerLiteral(5)))
                ),
                context.context.i64_type().const_int(4, true),
            ),
            (
                simple_expression_node!(
                    (FactorNode::Number(NumberNode::IntegerLiteral(12)))
                        | (FactorNode::Number(NumberNode::IntegerLiteral(5)))
                ),
                context.context.i64_type().const_int(13, true),
            ),
        ];

        let failed_tests: Vec<_> = tests
            .into_iter()
            .filter_map(|(expression_node, expected_result)| {
                let formatted_expression_node = format!("{expression_node:?}");

                let result = expression_node.generate_code(&context, None);

                match result {
                    Ok(result) => {
                        if result != expected_result {
                            Some(format!(
                                "\t{formatted_expression_node}: {result} != {expected_result}"
                            ))
                        } else {
                            None
                        }
                    }
                    Err(e) => Some(format!(
                        "\t{formatted_expression_node} encountered an error on code generation!\n{e}"
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
