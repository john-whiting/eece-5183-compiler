use inkwell::{types::BasicType, AddressSpace};
use thiserror::Error;

use crate::parser::{
    general::{NumberNode, TypeMark},
    variable::VariableDeclarationNode,
};

use super::{CodeGenerator, VariableDefinition};

pub const MAX_SUPPORTED_ARRAY_SIZE: i64 = u32::MAX as i64;

#[derive(Error, Debug)]
pub enum VariableDeclarationNodeCodeGenerationError {
    #[error(
        "Bounds must be a positive number no larger than {}.",
        MAX_SUPPORTED_ARRAY_SIZE
    )]
    UnsupportedBound,

    #[error("Bounds for an array must be an integer.")]
    NonIntegerBound,
}

impl<'a> CodeGenerator<'a> for VariableDeclarationNode {
    type Item = VariableDefinition<'a>;

    fn generate_code(
        self,
        context: &'a super::CodeGeneratorContext,
        _previous: Option<Self::Item>,
    ) -> anyhow::Result<Self::Item> {
        let (data, size) = match self {
            VariableDeclarationNode::Unbounded(data) => (data, 1),
            VariableDeclarationNode::Bounded(data, size) => (
                data,
                match size {
                    NumberNode::IntegerLiteral(i) => i,
                    NumberNode::FloatLiteral(_) => {
                        return Err(
                            VariableDeclarationNodeCodeGenerationError::NonIntegerBound.into()
                        )
                    }
                },
            ),
        };

        let ctx_type = match data.variable_type {
            TypeMark::Bool => context.context.bool_type().as_basic_type_enum(),
            TypeMark::Float => context.context.f64_type().as_basic_type_enum(),
            TypeMark::Integer => context.context.i64_type().as_basic_type_enum(),
            TypeMark::String => context
                .context
                .i8_type()
                .ptr_type(AddressSpace::default())
                .as_basic_type_enum(),
        };

        let ctx_type = match size {
            1 => ctx_type,
            2..=MAX_SUPPORTED_ARRAY_SIZE => ctx_type.array_type(size as u32).as_basic_type_enum(),
            _ => return Err(VariableDeclarationNodeCodeGenerationError::UnsupportedBound.into()),
        };

        let ptr_value = context.builder.build_alloca(ctx_type, &data.identifier)?;

        Ok(VariableDefinition {
            identifier: data.identifier,
            ctx_type,
            ptr_value,
        })
    }
}
