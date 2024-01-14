use crate::token::Token;

use super::{
    declaration,
    general::{colon, comma, identifier, left_parenthesis, right_parenthesis, type_mark, TypeMark},
    statement::{statement, StatementNode},
    util::{
        delimited, many0, not_partial, separated_list0, separated_pair, token, ParseInput,
        ParseResult,
    },
    variable::{variable_declaration, VariableDeclarationNode},
    DeclarationNode,
};

#[derive(Debug)]
pub struct ProcedureHeaderNode {
    pub identifier: String,
    pub return_type: TypeMark,
    pub parameters: Vec<VariableDeclarationNode>,
}
fn procedure_header(input: ParseInput<'_>) -> ParseResult<ProcedureHeaderNode> {
    let (input, _) = not_partial!(token!(input, Token::KwProcedure))?;
    let (input, (procedure_identifier, return_type)) =
        separated_pair(identifier, colon, type_mark)(input)?;
    let (input, parameters) = delimited(
        left_parenthesis,
        separated_list0(comma, variable_declaration),
        right_parenthesis,
    )(input)?;

    Ok((
        input,
        ProcedureHeaderNode {
            identifier: procedure_identifier,
            return_type,
            parameters,
        },
    ))
}

#[derive(Debug)]
pub struct ProcedureBodyNode {
    pub declarations: Vec<DeclarationNode>,
    pub statements: Vec<StatementNode>,
}
pub fn procedure_body(input: ParseInput<'_>) -> ParseResult<ProcedureBodyNode> {
    let (input, declarations) = many0(declaration)(input)?;
    let (input, _) = token!(input, Token::KwBegin)?;
    let (input, statements) = many0(statement)(input)?;
    let (input, _) = token!(input, Token::KwEnd)?;
    let (input, _) = token!(input, Token::KwProcedure)?;

    Ok((
        input,
        ProcedureBodyNode {
            declarations,
            statements,
        },
    ))
}

#[derive(Debug)]
pub struct ProcedureDeclarationNode {
    pub header: ProcedureHeaderNode,
    pub body: ProcedureBodyNode,
}
pub fn procedure_declaration(input: ParseInput<'_>) -> ParseResult<ProcedureDeclarationNode> {
    let (input, header) = procedure_header(input)?;
    let (input, body) = procedure_body(input)?;

    Ok((input, ProcedureDeclarationNode { header, body }))
}
