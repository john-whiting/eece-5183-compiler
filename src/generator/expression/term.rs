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

#[cfg(test)]
mod tests {
    use inkwell::context::Context;

    use crate::parser::{
        expression::{FactorNode, TermNode},
        general::NumberNode,
    };

    use super::*;

    #[test]
    fn term_node_generation() {
        let outside_context = Context::create();
        let context = CodeGeneratorContext::new(&outside_context);
        let function_type = context.context.void_type().fn_type(&[], false);
        let function_value = context.module.add_function("main", function_type, None);
        let function_entry_block = context.context.append_basic_block(function_value, "entry");
        context.builder.position_at_end(function_entry_block);

        let tests = vec![
            (
                TermNode::Start(
                    FactorNode::Number(NumberNode::IntegerLiteral(5)),
                    Box::new(TermNode::Multiply(
                        FactorNode::Number(NumberNode::IntegerLiteral(4)),
                        Box::new(TermNode::Nop),
                    )),
                ),
                context
                    .context
                    .i64_type()
                    .const_int(20, true)
                    .as_basic_value_enum(),
            ),
            (
                TermNode::Start(
                    FactorNode::Number(NumberNode::IntegerLiteral(4)),
                    Box::new(TermNode::Multiply(
                        FactorNode::Number(NumberNode::FloatLiteral(5.5)),
                        Box::new(TermNode::Nop),
                    )),
                ),
                context
                    .context
                    .f64_type()
                    .const_float(22.0)
                    .as_basic_value_enum(),
            ),
            (
                TermNode::Start(
                    FactorNode::Number(NumberNode::FloatLiteral(5.5)),
                    Box::new(TermNode::Multiply(
                        FactorNode::Number(NumberNode::FloatLiteral(5.5)),
                        Box::new(TermNode::Nop),
                    )),
                ),
                context
                    .context
                    .f64_type()
                    .const_float(30.25)
                    .as_basic_value_enum(),
            ),
            (
                TermNode::Start(
                    FactorNode::Number(NumberNode::FloatLiteral(5.5)),
                    Box::new(TermNode::Multiply(
                        FactorNode::Number(NumberNode::FloatLiteral(5.5)),
                        Box::new(TermNode::Multiply(
                            FactorNode::Number(NumberNode::FloatLiteral(5.5)),
                            Box::new(TermNode::Nop),
                        )),
                    )),
                ),
                context
                    .context
                    .f64_type()
                    .const_float(166.375)
                    .as_basic_value_enum(),
            ),
            (
                TermNode::Start(
                    FactorNode::Number(NumberNode::IntegerLiteral(5)),
                    Box::new(TermNode::Divide(
                        FactorNode::Number(NumberNode::IntegerLiteral(4)),
                        Box::new(TermNode::Nop),
                    )),
                ),
                context
                    .context
                    .i64_type()
                    .const_int(1, true)
                    .as_basic_value_enum(),
            ),
            (
                TermNode::Start(
                    FactorNode::Number(NumberNode::IntegerLiteral(4)),
                    Box::new(TermNode::Divide(
                        FactorNode::Number(NumberNode::FloatLiteral(5.0)),
                        Box::new(TermNode::Nop),
                    )),
                ),
                context
                    .context
                    .f64_type()
                    .const_float(0.8)
                    .as_basic_value_enum(),
            ),
            (
                TermNode::Start(
                    FactorNode::Number(NumberNode::FloatLiteral(5.5)),
                    Box::new(TermNode::Divide(
                        FactorNode::Number(NumberNode::FloatLiteral(5.5)),
                        Box::new(TermNode::Nop),
                    )),
                ),
                context
                    .context
                    .f64_type()
                    .const_float(1.0)
                    .as_basic_value_enum(),
            ),
        ];

        let failed_tests: Vec<_> = tests
            .into_iter()
            .filter_map(|(term_node, expected_result)| {
                let formatted_term_node = format!("{term_node:?}");

                let result = term_node.generate_code(&context);

                match result {
                    Ok(result) => {
                        if result != expected_result {
                            Some(format!(
                                "\t{formatted_term_node}: {result} != {expected_result}"
                            ))
                        } else {
                            None
                        }
                    }
                    Err(e) => Some(format!(
                        "\t{formatted_term_node} encountered an error on code generation!\n{e}"
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
