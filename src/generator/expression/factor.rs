use inkwell::values::{BasicValue, BasicValueEnum};
use thiserror::Error;

use crate::{
    generator::{
        util::CodeGenerationErrorHint, CodeGenerator, CodeGeneratorContext, VariableDefinition,
    },
    parser::{expression::FactorNode, general::NumberNode},
};

#[derive(Error, Debug)]
pub enum FactorNodeCodeGenerationError {
    #[error("Undeclared variable {0}")]
    UndeclaredVariable(String),

    #[error("Unable to negate {0} since only integer and float values can be negated.")]
    UnsupportedNegation(String),
}

impl<'a> CodeGenerator<'a> for FactorNode {
    type Item = BasicValueEnum<'a>;

    fn generate_code(
        self,
        context: &'a CodeGeneratorContext,
        _previous: Option<Self::Item>,
    ) -> anyhow::Result<Self::Item> {
        Ok(match self {
            FactorNode::Expression(x) => x.generate_code(context, None)?,
            FactorNode::Number(x) => match x {
                NumberNode::IntegerLiteral(n) => context
                    .context
                    .i64_type()
                    .const_int(n as u64, true)
                    .as_basic_value_enum(),
                NumberNode::FloatLiteral(n) => context
                    .context
                    .f64_type()
                    .const_float(n)
                    .as_basic_value_enum(),
            },
            FactorNode::Name {
                identifier,
                negated,
                index_of,
            } => {
                let reference = context.get_variable(&identifier);
                let reference = reference.ok_or(
                    FactorNodeCodeGenerationError::UndeclaredVariable(identifier.clone()),
                )?;

                // Index the variable (if applicable)
                let (ptr_value, ctx_type) = if let Some(index_of) = index_of {
                    reference.index_of(context, *index_of)?
                } else {
                    match reference.as_ref() {
                        VariableDefinition::NotIndexable(data)
                        | VariableDefinition::Indexable(data, _) => (data.ptr_value, data.ctx_type),
                    }
                };

                let mut value = context.builder.build_load(ctx_type, ptr_value, "load")?;

                if negated {
                    value = match value {
                        BasicValueEnum::IntValue(value) => context
                            .builder
                            .build_int_neg(value, "tmp_int_negation")?
                            .as_basic_value_enum(),
                        BasicValueEnum::FloatValue(value) => context
                            .builder
                            .build_float_neg(value, "tmp_float_negation")?
                            .as_basic_value_enum(),
                        unsupported_value => {
                            return Err(FactorNodeCodeGenerationError::UnsupportedNegation(
                                unsupported_value.error_hint(),
                            )
                            .into())
                        }
                    };
                };

                value.as_basic_value_enum()
            }
            FactorNode::ProcedureCall(_) => todo!("Procedure calls are not yet supported!"),

            // NOTE: LANGUAGE SEMANTICS | RULE #12
            // Strings are NULL TERMINATED
            FactorNode::String(x) => context
                .context
                .const_string(x.as_bytes(), true)
                .as_basic_value_enum(),

            // NOTE: LANGUAGE SEMANTICS | RULE #7
            // "True" bool values are represented as 1
            // "False bool values are represented as 0
            FactorNode::True => context
                .context
                .bool_type()
                .const_int(1, false)
                .as_basic_value_enum(),
            FactorNode::False => context
                .context
                .bool_type()
                .const_int(0, false)
                .as_basic_value_enum(),
        })
    }
}

#[cfg(test)]
mod tests {
    use inkwell::context::Context;

    use crate::parser::expression::{
        ArithmeticNode, ExpressionData, ExpressionNode, RelationNode, TermNode,
    };

    use super::*;

    #[test]
    fn factor_node_generation() {
        let outside_context = Context::create();
        let context = CodeGeneratorContext::new(&outside_context);

        let tests = vec![
            (
                FactorNode::Expression(Box::new(ExpressionNode::Start(
                    ExpressionData::Nop(ArithmeticNode::Start(
                        RelationNode::Start(
                            TermNode::Start(FactorNode::True, Box::new(TermNode::Nop)),
                            Box::new(RelationNode::Nop),
                        ),
                        Box::new(ArithmeticNode::Nop),
                    )),
                    Box::new(ExpressionNode::Nop),
                ))),
                context
                    .context
                    .bool_type()
                    .const_int(1, false)
                    .as_basic_value_enum(),
            ),
            // (FactorNode::ProcedureCall(ProcedureCallNode { identifier: "test_procedure".to_string(), arguments: vec![] } ), todo!("Test Procedure calling")),
            // (FactorNode::Name("test_var".to_string()), todo!("Test Name Lookup")),
            // (FactorNode::NameNegated("test_var".to_string()), todo!("Test Name Lookup with Negation")),
            (
                FactorNode::Number(NumberNode::IntegerLiteral(-5)),
                context
                    .context
                    .i64_type()
                    .const_int(0xfffffffffffffffb, true)
                    .as_basic_value_enum(),
            ),
            (
                FactorNode::Number(NumberNode::IntegerLiteral(2)),
                context
                    .context
                    .i64_type()
                    .const_int(2, true)
                    .as_basic_value_enum(),
            ),
            (
                FactorNode::Number(NumberNode::FloatLiteral(-1.6)),
                context
                    .context
                    .f64_type()
                    .const_float(-1.6)
                    .as_basic_value_enum(),
            ),
            (
                FactorNode::Number(NumberNode::FloatLiteral(1.6)),
                context
                    .context
                    .f64_type()
                    .const_float(1.6)
                    .as_basic_value_enum(),
            ),
            (
                FactorNode::String("Hello World!".to_string()),
                context
                    .context
                    .const_string(b"Hello World!", true)
                    .as_basic_value_enum(),
            ),
            (
                FactorNode::True,
                context
                    .context
                    .bool_type()
                    .const_int(1, false)
                    .as_basic_value_enum(),
            ),
            (
                FactorNode::False,
                context
                    .context
                    .bool_type()
                    .const_int(0, false)
                    .as_basic_value_enum(),
            ),
        ];

        let failed_tests: Vec<_> = tests
            .into_iter()
            .filter_map(|(factor_node, expected_result)| {
                let formatted_factor_node = format!("{factor_node:?}");

                let result = factor_node.generate_code(&context, None);

                if let Ok(result) = result {
                    if result != expected_result {
                        Some(format!(
                            "\t{formatted_factor_node}: {result} != {expected_result}"
                        ))
                    } else {
                        None
                    }
                } else {
                    Some(format!(
                        "\t{formatted_factor_node} encountered an error on code generation!"
                    ))
                }
            })
            .collect();

        if !failed_tests.is_empty() {
            let formatted = failed_tests.join("\n");
            panic!("FactorNode Code Generation test(s) failed:\n{formatted}");
        }
    }
}
