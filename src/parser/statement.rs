use crate::{
    parser::{
        general::index,
        util::{not_partial, opt, parse_error},
    },
    token::Token,
};

use super::{
    expression::{expression, ExpressionNode},
    general::{else_kw, identifier, left_parenthesis, right_parenthesis, semi_colon},
    util::{delimited, many0, peek, preceeded, separated_pair, token, ParseInput, ParseResult},
};

#[derive(Debug)]
pub struct AssignmentStatementDetails {
    pub identifier: String,
    pub expression: ExpressionNode,
}
#[derive(Debug)]
pub enum AssignmentStatementNode {
    Indexed(AssignmentStatementDetails, ExpressionNode),
    NotIndexed(AssignmentStatementDetails),
}
pub fn assignment_statement(input: ParseInput<'_>) -> ParseResult<AssignmentStatementNode> {
    let (input, ident) = identifier(input)?;
    let (input, idx) = opt(index)(input)?;
    let (input, _) = token!(input, Token::AssignmentOperator)?;
    let (input, expr) = expression(input)?;

    let details = AssignmentStatementDetails {
        identifier: ident,
        expression: expr,
    };

    Ok((
        input,
        match idx {
            Some(idx) => AssignmentStatementNode::Indexed(details, idx),
            None => AssignmentStatementNode::NotIndexed(details),
        },
    ))
}

#[derive(Debug)]
pub struct IfStatementNode {
    pub condition: ExpressionNode,
    pub then_block: Vec<StatementNode>,
    pub else_block: Option<Vec<StatementNode>>,
}
pub fn if_statement(input: ParseInput<'_>) -> ParseResult<IfStatementNode> {
    let (input, _) = token!(input, Token::KwIf)?;
    let (input, condition) = delimited(left_parenthesis, expression, right_parenthesis)(input)?;
    let (input, _) = token!(input, Token::KwThen)?;
    let (input, then_block) = many0(statement)(input)?;
    let (input, else_block) = opt(preceeded(else_kw, many0(statement)))(input)?;
    let (input, _) = token!(input, Token::KwEnd)?;
    let (input, _) = token!(input, Token::KwIf)?;

    Ok((
        input,
        IfStatementNode {
            condition,
            then_block,
            else_block,
        },
    ))
}

#[derive(Debug)]
pub struct LoopStatementNode {
    pub assignment: AssignmentStatementNode,
    pub condition: ExpressionNode,
    pub statements: Vec<StatementNode>,
}
pub fn loop_statement(input: ParseInput<'_>) -> ParseResult<LoopStatementNode> {
    let (input, _) = token!(input, Token::KwFor)?;
    let (input, (assignment, condition)) = delimited(
        left_parenthesis,
        separated_pair(assignment_statement, semi_colon, expression),
        right_parenthesis,
    )(input)?;
    let (input, statements) = many0(statement)(input)?;
    let (input, _) = token!(input, Token::KwEnd)?;
    let (input, _) = token!(input, Token::KwFor)?;

    Ok((
        input,
        LoopStatementNode {
            assignment,
            condition,
            statements,
        },
    ))
}

#[derive(Debug)]
pub struct ReturnStatementNode(ExpressionNode);
pub fn return_statement(input: ParseInput<'_>) -> ParseResult<ReturnStatementNode> {
    let (input, _) = token!(input, Token::KwReturn)?;
    let (input, expr) = expression(input)?;

    Ok((input, ReturnStatementNode(expr)))
}

#[derive(Debug)]
pub enum StatementNode {
    Assignment(AssignmentStatementNode),
    If(IfStatementNode),
    Loop(LoopStatementNode),
    Return(ReturnStatementNode),
}
pub fn statement(input: ParseInput<'_>) -> ParseResult<StatementNode> {
    let (input, next_item) = peek(input)?;

    let (input, declaration) = match next_item.token {
        Token::Identifier(_) => {
            let (i, result) = assignment_statement(input)?;
            (i, StatementNode::Assignment(result))
        }
        Token::KwIf => {
            let (i, result) = if_statement(input)?;
            (i, StatementNode::If(result))
        }
        Token::KwFor => {
            let (i, result) = loop_statement(input)?;
            (i, StatementNode::Loop(result))
        }
        Token::KwReturn => {
            let (i, result) = return_statement(input)?;
            (i, StatementNode::Return(result))
        }
        _ => return not_partial!(parse_error!("Expected statement.", next_item)),
    };

    let (input, _) = token!(input, Token::SemiColon)?;

    Ok((input, declaration))
}
