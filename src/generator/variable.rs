use inkwell::types::BasicType;
use thiserror::Error;

use crate::parser::{general::NumberNode, variable::VariableDeclarationNode};

use super::{
    util::type_mark_to_llvm_type, CodeGenerator, VariableDefinition, VariableDefinitionData,
};

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
        &self,
        context: &'a super::CodeGeneratorContext,
        _previous: Option<Self::Item>,
    ) -> anyhow::Result<Self::Item> {
        let (data, size) = match self {
            VariableDeclarationNode::Unbounded(data) => (data, 1),
            VariableDeclarationNode::Bounded(data, size) => (
                data,
                match size {
                    NumberNode::IntegerLiteral(i) => *i,
                    NumberNode::FloatLiteral(_) => {
                        return Err(
                            VariableDeclarationNodeCodeGenerationError::NonIntegerBound.into()
                        )
                    }
                },
            ),
        };

        let ctx_type = type_mark_to_llvm_type(context, &data.variable_type);

        let (ctx_type, size) = match size {
            1 => (ctx_type, None),
            2..=MAX_SUPPORTED_ARRAY_SIZE => {
                let size = size as u32;
                (ctx_type.array_type(size).as_basic_type_enum(), Some(size))
            }
            _ => return Err(VariableDeclarationNodeCodeGenerationError::UnsupportedBound.into()),
        };

        let ptr_value = context.builder.build_alloca(ctx_type, &data.identifier)?;

        let data = VariableDefinitionData {
            identifier: data.identifier.clone(),
            ctx_type,
            ptr_value,
        };

        Ok(match size {
            None => VariableDefinition::NotIndexable(data),
            Some(size) => VariableDefinition::Indexable(data, size),
        })
    }
}
