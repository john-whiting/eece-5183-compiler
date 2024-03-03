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

    #[error("Unable to perform NOT on {0}.")]
    BitwiseNotUnsupportedType(String),

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
                    ExpressionData::Not(node) => {
                        let node = node.generate_code(context)?;

                        match node {
                            BasicValueEnum::IntValue(int_value) => Ok(context
                                .builder
                                .build_not(int_value, "tmpnot")?
                                .as_basic_value_enum()),
                            x => Err(
                                ExpressionNodeCodeGenerationError::BitwiseNotUnsupportedType(
                                    x.error_hint(),
                                ),
                            ),
                        }
                    }
                    ExpressionData::Nop(node) => Ok(node.generate_code(context)?),
                }?;
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

                let result = expression_node.generate_code(&context);

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
