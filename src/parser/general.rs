use crate::{
    parser::util::{not_partial, token},
    token::Token,
};

use super::{
    expression::{expression, ExpressionNode},
    util::{ParseInput, ParseResult},
};

pub fn comma(input: ParseInput<'_>) -> ParseResult<Token> {
    not_partial!(token!(input, Token::Comma))
}

pub fn colon(input: ParseInput<'_>) -> ParseResult<Token> {
    token!(input, Token::Colon)
}

pub fn semi_colon(input: ParseInput<'_>) -> ParseResult<Token> {
    token!(input, Token::SemiColon)
}

pub fn global(input: ParseInput<'_>) -> ParseResult<Token> {
    not_partial!(token!(input, Token::KwGlobal))
}

pub fn left_parenthesis(input: ParseInput<'_>) -> ParseResult<Token> {
    token!(input, Token::LeftParenthesis)
}

pub fn right_parenthesis(input: ParseInput<'_>) -> ParseResult<Token> {
    token!(input, Token::RightParenthesis)
}

pub fn else_kw(input: ParseInput<'_>) -> ParseResult<Token> {
    not_partial!(token!(input, Token::KwElse))
}

pub fn identifier(input: ParseInput<'_>) -> ParseResult<String> {
    token!(input, Token::Identifier(x) => x)
}

#[derive(Debug)]
pub enum NumberNode {
    IntegerLiteral(i64),
    FloatLiteral(f64),
}
pub fn number(input: ParseInput<'_>) -> ParseResult<NumberNode> {
    let (input, token) = token!(input, Token::Integer(_) | Token::Float(_))?;

    Ok((
        input,
        match token {
            Token::Integer(x) => NumberNode::IntegerLiteral(x),
            Token::Float(x) => NumberNode::FloatLiteral(x),
            _ => panic!("Invalid number matched, this should not be possible."),
        },
    ))
}

pub fn bound(input: ParseInput<'_>) -> ParseResult<NumberNode> {
    let (input, _) = not_partial!(token!(input, Token::LeftBracket))?;
    let (input, number) = number(input)?;
    let (input, _) = token!(input, Token::RightBracket)?;

    Ok((input, number))
}

pub fn index(input: ParseInput<'_>) -> ParseResult<ExpressionNode> {
    let (input, _) = not_partial!(token!(input, Token::LeftBracket))?;
    let (input, expr) = expression(input)?;
    let (input, _) = token!(input, Token::RightBracket)?;

    Ok((input, expr))
}

#[derive(Debug)]
pub enum TypeMark {
    Integer,
    Float,
    String,
    Bool,
}
pub fn type_mark(input: ParseInput<'_>) -> ParseResult<TypeMark> {
    let (input, mark) = token!(
        input,
        Token::KwInteger | Token::KwFloat | Token::KwString | Token::KwBool
    )?;

    Ok((
        input,
        match mark {
            Token::KwInteger => TypeMark::Integer,
            Token::KwFloat => TypeMark::Float,
            Token::KwString => TypeMark::String,
            Token::KwBool => TypeMark::Bool,
            _ => panic!("Invalid type mark matched, this should not be possible."),
        },
    ))
}
