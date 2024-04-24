use inkwell::{
    types::BasicTypeEnum,
    values::{BasicValueEnum, PointerValue},
    IntPredicate,
};
use thiserror::Error;

use crate::{
    generator::util::CodeGenerationErrorHint,
    parser::{expression::ExpressionNode, statement::AssignmentStatementNode},
};

use super::{CodeGenerator, CodeGeneratorContext, VariableDefinition};

#[derive(Error, Debug)]
pub enum AssignmentStatementNodeCodeGenerationError {
    #[error("Unable to assign to undeclared variable {0}.")]
    UndeclaredVariable(String),

    #[error("A value of type {0} cannot be used to index an array.")]
    NonIntegerIndex(String),
}

// trait IndexOf<'a> {
//     fn index_of(self, context: &'a CodeGeneratorContext, index: Option<BasicValueEnum>) -> Result<PointerValue<'a>, AssignmentStatementNodeCodeGenerationError>;
// }
//
// impl<'a> IndexOf<'a> for VariableDefinition<'a> {
//     fn index_of(self, context: &'a CodeGeneratorContext, index: Option<BasicValueEnum>) -> Result<PointerValue<'a>, AssignmentStatementNodeCodeGenerationError> {
//         let (ctx_type, ptr_value) = match (self.ctx_type, index) {
//             (BasicTypeEnum::ArrayType(array), Some(index)) => {
//                 // NOTE: LANGUAGE SEMANTICS | RULE #13
//                 // Array Indices must be an integer value
//                 let ptr_value = match index {
//                     BasicValueEnum::IntValue(value) => {
//                     },
//                     unsupported_type => return Err(AssignmentStatementNodeCodeGenerationError::NonIntegerIndex(unsupported_type.error_hint()).into()),
//                 }
//                 todo!()
//                 // (array.get_element_type(), ptr_value) => todo!()
//             }
//         };
//
//         todo!()
//     }
// }

impl<'a> CodeGenerator<'a> for AssignmentStatementNode {
    type Item = String;

    fn generate_code(
        self,
        context: &'a CodeGeneratorContext,
        previous: Option<Self::Item>,
    ) -> anyhow::Result<Self::Item> {
        let (data, index) = match self {
            AssignmentStatementNode::NotIndexed(data) => (data, None),
            AssignmentStatementNode::Indexed(data, i) => (data, Some(i)),
        };

        // let instruction = match context.get_variable(&data.identifier) {
        //     Some(variable) => {
        //         let expression = data.expression.generate_code(context, None)?;
        //         let (ctx_type, ptr_value) = match (variable.ctx_type, index) {
        //             (BasicTypeEnum::ArrayType(array), Some(index)) => {
        //                 let offset = index.generate_code(context, None)?;
        //                 // NOTE: LANGUAGE SEMANTICS | RULE #13
        //                 // Array Indices must be an integer value
        //                 let ptr_value = match offset {
        //                     BasicValueEnum::IntValue(value) => {
        //                         if value.is_const() {
        //
        //                         } else {
        //                             context.builder.build_gep(, ptr, ordered_indexes, name)
        //                         }
        //                     },
        //                     unsupported_type => return Err(AssignmentStatementNodeCodeGenerationError::NonIntegerIndex(unsupported_type.error_hint()).into()),
        //                 }
        //                 (array.get_element_type(), ptr_value)
        //             }
        //         };
        //         match (variable.ctx_type, expression) {
        //             (BasicTypeEnum::IntType(ty), BasicValueEnum::IntValue(value)) => {
        //                 let value =
        //                     if ty.get_bit_width() == 1 && value.get_type().get_bit_width() != 1 {
        //                         // NOTE: LANGUAGE SEMANTICS | RULE #7
        //                         // Integer cast to bool (any non-zero integer is true, otherwise false)
        //                         context.builder.build_int_compare(
        //                             IntPredicate::NE,
        //                             value,
        //                             value.get_type().const_zero(),
        //                             "int_casting",
        //                         )?
        //                     } else {
        //                         value
        //                     };
        //
        //                 context.builder.build_store(ptr_value, value)?
        //             }
        //         }
        //     }
        //     None => {
        //         return Err(
        //             AssignmentStatementNodeCodeGenerationError::UndeclaredVariable(data.identifier)
        //                 .into(),
        //         )
        //     }
        // }

        todo!()
    }
}
