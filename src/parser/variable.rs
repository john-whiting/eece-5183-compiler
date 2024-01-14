use crate::token::Token;

use super::{
    general::{bound, identifier, type_mark, NumberNode, TypeMark},
    util::{not_partial, opt, token, ParseInput, ParseResult},
};

#[derive(Debug)]
pub struct VariableDeclarationData {
    pub identifier: String,
    pub variable_type: TypeMark,
}
#[derive(Debug)]
pub enum VariableDeclarationNode {
    Unbounded(VariableDeclarationData),
    Bounded(VariableDeclarationData, NumberNode),
}
pub fn variable_declaration(input: ParseInput<'_>) -> ParseResult<VariableDeclarationNode> {
    let (input, _) = not_partial!(token!(input, Token::KwVariable))?;
    let (input, variable_identifier) = identifier(input)?;
    let (input, _) = token!(input, Token::Colon)?;
    let (input, variable_type) = type_mark(input)?;
    let (input, bound) = opt(bound)(input)?;

    let data = VariableDeclarationData {
        identifier: variable_identifier,
        variable_type,
    };

    Ok((
        input,
        match bound {
            Some(number) => VariableDeclarationNode::Bounded(data, number),
            None => VariableDeclarationNode::Unbounded(data),
        },
    ))
}
