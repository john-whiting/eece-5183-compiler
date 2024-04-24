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

    #[error("Unable to index {0}.")]
    NonArrayIndexing(String),

    #[error("Index out of bounds for {0} (size {1}).")]
    IndexOutOfBounds(String, u32),

    #[error("Variable {0} is missing constant size.")]
    ArrayMissingSize(String),

    #[error("Unable to index an array with {0}.")]
    NonIntegerIndex(String),
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

                let (reference, size) = match reference.as_ref() {
                    VariableDefinition::NotIndexable(data) => (data, None),
                    VariableDefinition::Indexable(data, size) => (data, Some(size)),
                };

                let mut ctx_type = reference.ctx_type;

                // Index the variable (if applicable)
                let ptr_value = if let Some(index_of) = index_of {
                    // Only arrays can be indexed
                    if !ctx_type.is_array_type() {
                        return Err(FactorNodeCodeGenerationError::NonArrayIndexing(
                            ctx_type.error_hint(),
                        )
                        .into());
                    }

                    // if indexing a variable, that variable must be sized
                    let size =
                        size.ok_or(FactorNodeCodeGenerationError::ArrayMissingSize(identifier))?;

                    let array_type = ctx_type.into_array_type();

                    // ctx_type is now the inside type
                    ctx_type = array_type.get_element_type();

                    let expr = index_of.generate_code(context, None)?;

                    // LANGUAGE SEMANTICS | RULE #13
                    // Arrays must be indexed by integers ONLY
                    let expr = match expr {
                        BasicValueEnum::IntValue(value) => value,
                        unsupported_value => {
                            return Err(FactorNodeCodeGenerationError::NonIntegerIndex(
                                unsupported_value.error_hint(),
                            )
                            .into())
                        }
                    };

                    // LANGUAGE SEMANTICS | RULE #13
                    // The lower bound of an array index is 0, and the upper bound is size - 1

                    let fails_lower_bound_value = context.builder.build_int_compare(
                        inkwell::IntPredicate::SLT,
                        expr,
                        context.context.i64_type().const_zero(),
                        "array_lower_bounds_check",
                    )?;
                    let fails_upper_bound_value = context.builder.build_int_compare(
                        inkwell::IntPredicate::SGE,
                        expr,
                        context.context.i64_type().const_int(*size as u64, true),
                        "array_upper_bounds_check",
                    )?;

                    let fails_bound_value = context.builder.build_or(
                        fails_lower_bound_value,
                        fails_upper_bound_value,
                        "fails_bound_check",
                    )?;

                    let current_fn_value = context.fn_value();
                    let then_bb = context.context.append_basic_block(current_fn_value, "then");
                    let cont_bb = context
                        .context
                        .append_basic_block(current_fn_value, "ifcont");

                    context
                        .builder
                        .build_conditional_branch(fails_bound_value, then_bb, cont_bb)
                        .unwrap();

                    // build the then block
                    context.builder.position_at_end(then_bb);
                    // TODO: Add out of bounds message
                    context.cstd.exit(1)?;

                    // place builder at the continue block
                    context.builder.position_at_end(cont_bb);

                    unsafe {
                        context.builder.build_gep(
                            ctx_type,
                            reference.ptr_value,
                            &[expr],
                            "array_index",
                        )?
                    }
                } else {
                    reference.ptr_value
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
