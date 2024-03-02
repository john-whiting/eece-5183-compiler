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

#[cfg(test)]
mod tests {
    use inkwell::context::Context;

    use crate::parser::{
        expression::{FactorNode, RelationNode, TermNode},
        general::NumberNode,
    };

    use super::*;

    #[test]
    fn arithmetic_node_generation() {
        let outside_context = Context::create();
        let context = CodeGeneratorContext::new(&outside_context);
        let function_type = context.context.void_type().fn_type(&[], false);
        let function_value = context.module.add_function("main", function_type, None);
        let function_entry_block = context.context.append_basic_block(function_value, "entry");
        context.builder.position_at_end(function_entry_block);

        let tests = vec![
            (
                ArithmeticNode::Start(
                    RelationNode::Start(
                        TermNode::Start(
                            FactorNode::Number(NumberNode::IntegerLiteral(3)),
                            Box::new(TermNode::Nop),
                        ),
                        Box::new(RelationNode::Nop),
                    ),
                    Box::new(ArithmeticNode::Add(
                        RelationNode::Start(
                            TermNode::Start(
                                FactorNode::Number(NumberNode::IntegerLiteral(3)),
                                Box::new(TermNode::Nop),
                            ),
                            Box::new(RelationNode::Nop),
                        ),
                        Box::new(ArithmeticNode::Nop),
                    )),
                ),
                context
                    .context
                    .i64_type()
                    .const_int(6, true)
                    .as_basic_value_enum(),
            ),
            (
                ArithmeticNode::Start(
                    RelationNode::Start(
                        TermNode::Start(
                            FactorNode::Number(NumberNode::IntegerLiteral(3)),
                            Box::new(TermNode::Nop),
                        ),
                        Box::new(RelationNode::Nop),
                    ),
                    Box::new(ArithmeticNode::Add(
                        RelationNode::Start(
                            TermNode::Start(
                                FactorNode::Number(NumberNode::IntegerLiteral(-3)),
                                Box::new(TermNode::Nop),
                            ),
                            Box::new(RelationNode::Nop),
                        ),
                        Box::new(ArithmeticNode::Nop),
                    )),
                ),
                context
                    .context
                    .i64_type()
                    .const_int(0, true)
                    .as_basic_value_enum(),
            ),
            (
                ArithmeticNode::Start(
                    RelationNode::Start(
                        TermNode::Start(
                            FactorNode::Number(NumberNode::IntegerLiteral(3)),
                            Box::new(TermNode::Nop),
                        ),
                        Box::new(RelationNode::Nop),
                    ),
                    Box::new(ArithmeticNode::Add(
                        RelationNode::Start(
                            TermNode::Start(
                                FactorNode::Number(NumberNode::FloatLiteral(3.0)),
                                Box::new(TermNode::Nop),
                            ),
                            Box::new(RelationNode::Nop),
                        ),
                        Box::new(ArithmeticNode::Nop),
                    )),
                ),
                context
                    .context
                    .f64_type()
                    .const_float(6.0)
                    .as_basic_value_enum(),
            ),
            (
                ArithmeticNode::Start(
                    RelationNode::Start(
                        TermNode::Start(
                            FactorNode::Number(NumberNode::FloatLiteral(3.0)),
                            Box::new(TermNode::Nop),
                        ),
                        Box::new(RelationNode::Nop),
                    ),
                    Box::new(ArithmeticNode::Add(
                        RelationNode::Start(
                            TermNode::Start(
                                FactorNode::Number(NumberNode::FloatLiteral(3.0)),
                                Box::new(TermNode::Nop),
                            ),
                            Box::new(RelationNode::Nop),
                        ),
                        Box::new(ArithmeticNode::Nop),
                    )),
                ),
                context
                    .context
                    .f64_type()
                    .const_float(6.0)
                    .as_basic_value_enum(),
            ),
            (
                ArithmeticNode::Start(
                    RelationNode::Start(
                        TermNode::Start(
                            FactorNode::Number(NumberNode::IntegerLiteral(3)),
                            Box::new(TermNode::Nop),
                        ),
                        Box::new(RelationNode::Nop),
                    ),
                    Box::new(ArithmeticNode::Sub(
                        RelationNode::Start(
                            TermNode::Start(
                                FactorNode::Number(NumberNode::IntegerLiteral(3)),
                                Box::new(TermNode::Nop),
                            ),
                            Box::new(RelationNode::Nop),
                        ),
                        Box::new(ArithmeticNode::Nop),
                    )),
                ),
                context
                    .context
                    .i64_type()
                    .const_int(0, true)
                    .as_basic_value_enum(),
            ),
            (
                ArithmeticNode::Start(
                    RelationNode::Start(
                        TermNode::Start(
                            FactorNode::Number(NumberNode::IntegerLiteral(3)),
                            Box::new(TermNode::Nop),
                        ),
                        Box::new(RelationNode::Nop),
                    ),
                    Box::new(ArithmeticNode::Sub(
                        RelationNode::Start(
                            TermNode::Start(
                                FactorNode::Number(NumberNode::IntegerLiteral(-3)),
                                Box::new(TermNode::Nop),
                            ),
                            Box::new(RelationNode::Nop),
                        ),
                        Box::new(ArithmeticNode::Nop),
                    )),
                ),
                context
                    .context
                    .i64_type()
                    .const_int(6, true)
                    .as_basic_value_enum(),
            ),
            (
                ArithmeticNode::Start(
                    RelationNode::Start(
                        TermNode::Start(
                            FactorNode::Number(NumberNode::IntegerLiteral(3)),
                            Box::new(TermNode::Nop),
                        ),
                        Box::new(RelationNode::Nop),
                    ),
                    Box::new(ArithmeticNode::Sub(
                        RelationNode::Start(
                            TermNode::Start(
                                FactorNode::Number(NumberNode::FloatLiteral(3.0)),
                                Box::new(TermNode::Nop),
                            ),
                            Box::new(RelationNode::Nop),
                        ),
                        Box::new(ArithmeticNode::Nop),
                    )),
                ),
                context
                    .context
                    .f64_type()
                    .const_float(0.0)
                    .as_basic_value_enum(),
            ),
            (
                ArithmeticNode::Start(
                    RelationNode::Start(
                        TermNode::Start(
                            FactorNode::Number(NumberNode::FloatLiteral(3.0)),
                            Box::new(TermNode::Nop),
                        ),
                        Box::new(RelationNode::Nop),
                    ),
                    Box::new(ArithmeticNode::Sub(
                        RelationNode::Start(
                            TermNode::Start(
                                FactorNode::Number(NumberNode::FloatLiteral(3.0)),
                                Box::new(TermNode::Nop),
                            ),
                            Box::new(RelationNode::Nop),
                        ),
                        Box::new(ArithmeticNode::Nop),
                    )),
                ),
                context
                    .context
                    .f64_type()
                    .const_float(0.0)
                    .as_basic_value_enum(),
            ),
        ];

        let failed_tests: Vec<_> = tests
            .into_iter()
            .filter_map(|(arithmetic_node, expected_result)| {
                let formatted_arithmetic_node = format!("{arithmetic_node:?}");

                let result = arithmetic_node.generate_code(&context);

                match result {
                    Ok(result) => {
                        if result != expected_result {
                            Some(format!(
                                "\t{formatted_arithmetic_node}: {result} != {expected_result}"
                            ))
                        } else {
                            None
                        }
                    }
                    Err(e) => Some(format!(
                        "\t{formatted_arithmetic_node} encountered an error on code generation!\n{e}"
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
