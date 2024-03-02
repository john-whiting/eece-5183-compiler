use inkwell::values::{BasicValue, BasicValueEnum};

use crate::{
    generator::{CodeGenerator, CodeGeneratorContext},
    parser::{expression::FactorNode, general::NumberNode},
};

impl<'a> CodeGenerator<'a> for FactorNode {
    type Item = BasicValueEnum<'a>;

    fn generate_code(self, context: &'a CodeGeneratorContext) -> anyhow::Result<Self::Item> {
        Ok(match self {
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
            _ => todo!("This branch of code generation is not yet implemented"),
        })
    }
}
