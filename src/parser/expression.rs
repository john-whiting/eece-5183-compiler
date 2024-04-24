use crate::{
    parser::{procedure::procedure_call, util::token},
    token::Token,
};

use super::{
    general::{identifier, index, left_parenthesis, not, number, right_parenthesis, NumberNode},
    procedure::ProcedureCallNode,
    util::{delimited, opt, parse_error, peek, ParseInput, ParseResult},
};

/*
 * -------------------
 * Original Grammar:
 * expression := expression & arithmetic
 *             | expression | arithmetic
 *             | [not] arithmetic
 *
 * -------------------
 * Non-Left-Recursive Grammar:
 * expression       := [not] arithmetic expression_prime
 * expression_prime := & arithmetic expression_prime
 *                   | | arithmetic expression_prime
 *                   | epsilon
 */

#[derive(Debug)]
pub enum ExpressionData {
    Not(ArithmeticNode),
    Nop(ArithmeticNode),
}

#[derive(Debug)]
pub enum ExpressionNode {
    Start(ExpressionData, Box<ExpressionNode>),
    And(ArithmeticNode, Box<ExpressionNode>),
    Or(ArithmeticNode, Box<ExpressionNode>),
    Nop,
}
pub fn expression(input: ParseInput<'_>) -> ParseResult<ExpressionNode> {
    let (input, not_token) = opt(not)(input)?;
    let arithmetic_result = arithmetic(input.clone());

    let (input, left) = if let Ok((input, left)) = arithmetic_result {
        if not_token.is_some() {
            (input, ExpressionData::Not(left))
        } else {
            (input, ExpressionData::Nop(left))
        }
    } else {
        let (_, item) = peek(input)?;
        let token = item.token.clone();
        return parse_error!("Expected expression, found {token}", item);
    };

    let (input, right) = expression_prime(input)?;

    Ok((input, ExpressionNode::Start(left, Box::new(right))))
}

macro_rules! expression_match {
    ($input:ident, $token:pat, $node:path) => {{
        let (input, _) = token!($input, $token)?;
        let (input, left) = arithmetic(input)?;
        let (input, prime) = expression_prime(input)?;
        (input, $node(left, Box::new(prime)))
    }};
}

fn expression_prime(input: ParseInput<'_>) -> ParseResult<ExpressionNode> {
    let (input, item) = peek(input)?;

    let (input, node) = match item.token {
        Token::BitAnd => expression_match!(input, Token::BitAnd, ExpressionNode::And),
        Token::BitOr => expression_match!(input, Token::BitOr, ExpressionNode::Or),
        _ => (input, ExpressionNode::Nop),
    };

    Ok((input, node))
}

/*
 * -------------------
 * Original Grammar:
 * arithmetic := arithmetic + relation
 *             | arithmetic - relation
 *             | relation
 *
 * -------------------
 * Non-Left-Recursive Grammar:
 * arithmetic       := relation arithmetic_prime
 * arithmetic_prime := + relation
 *                   | - relation
 *                   | epsilon
 */

#[derive(Debug)]
pub enum ArithmeticNode {
    Start(RelationNode, Box<ArithmeticNode>),
    Add(RelationNode, Box<ArithmeticNode>),
    Sub(RelationNode, Box<ArithmeticNode>),
    Nop,
}
pub fn arithmetic(input: ParseInput<'_>) -> ParseResult<ArithmeticNode> {
    let (input, left) = relation(input)?;
    let (input, right) = arithmetic_prime(input)?;

    Ok((input, ArithmeticNode::Start(left, Box::new(right))))
}

macro_rules! arithmetic_match {
    ($input:ident, $token:pat, $node:path) => {{
        let (input, _) = token!($input, $token)?;
        let (input, left) = relation(input)?;
        let (input, prime) = arithmetic_prime(input)?;
        (input, $node(left, Box::new(prime)))
    }};
}

fn arithmetic_prime(input: ParseInput<'_>) -> ParseResult<ArithmeticNode> {
    let (input, item) = peek(input)?;

    let (input, node) = match item.token {
        Token::Plus => arithmetic_match!(input, Token::Plus, ArithmeticNode::Add),
        Token::Minus => arithmetic_match!(input, Token::Minus, ArithmeticNode::Sub),
        _ => (input, ArithmeticNode::Nop),
    };

    Ok((input, node))
}

/*
 * -------------------
 * Original Grammar:
 * relation := relation < term
 *           | relation <= term
 *           | relation > term
 *           | relation >= term
 *           | relation == term
 *           | relation != term
 *           | term
 *
 * -------------------
 * Non-Left-Recursive Grammar:
 * relation       := term relation_prime
 * relation_prime := < term
 *                 | <= term
 *                 | > term
 *                 | >= term
 *                 | == term
 *                 | != term
 *                 | epsilon
 */

#[derive(Debug)]
pub enum RelationNode {
    Start(TermNode, Box<RelationNode>),
    LessThan(TermNode, Box<RelationNode>),
    LessThanEqual(TermNode, Box<RelationNode>),
    GreaterThan(TermNode, Box<RelationNode>),
    GreaterThanEqual(TermNode, Box<RelationNode>),
    Equal(TermNode, Box<RelationNode>),
    NotEqual(TermNode, Box<RelationNode>),
    Nop,
}
pub fn relation(input: ParseInput<'_>) -> ParseResult<RelationNode> {
    let (input, left) = term(input)?;
    let (input, right) = relation_prime(input)?;

    Ok((input, RelationNode::Start(left, Box::new(right))))
}

macro_rules! relation_match {
    ($input:ident, $token:pat, $node:path) => {{
        let (input, _) = token!($input, $token)?;
        let (input, left) = term(input)?;
        let (input, prime) = relation_prime(input)?;
        (input, $node(left, Box::new(prime)))
    }};
}

fn relation_prime(input: ParseInput<'_>) -> ParseResult<RelationNode> {
    let (input, item) = peek(input)?;

    let (input, node) = match item.token {
        Token::LessThan => relation_match!(input, Token::LessThan, RelationNode::LessThan),
        Token::LessThanEqual => {
            relation_match!(input, Token::LessThanEqual, RelationNode::LessThanEqual)
        }
        Token::GreaterThan => relation_match!(input, Token::GreaterThan, RelationNode::GreaterThan),
        Token::GreaterThanEqual => relation_match!(
            input,
            Token::GreaterThanEqual,
            RelationNode::GreaterThanEqual
        ),
        Token::Equal => relation_match!(input, Token::Equal, RelationNode::Equal),
        Token::NotEqual => relation_match!(input, Token::NotEqual, RelationNode::NotEqual),
        _ => (input, RelationNode::Nop),
    };

    Ok((input, node))
}

/*
 * -------------------
 * Original Grammar:
 * term := term * factor
 *       | term / factor
 *       | factor
 *
 * -------------------
 * Non-Left-Recursive Grammar:
 * term       := factor term_prime
 * term_prime := * factor
 *             | / factor
 *             | epsilon
 */

#[derive(Debug)]
pub enum TermNode {
    Start(FactorNode, Box<TermNode>),
    Multiply(FactorNode, Box<TermNode>),
    Divide(FactorNode, Box<TermNode>),
    Nop,
}
pub fn term(input: ParseInput<'_>) -> ParseResult<TermNode> {
    let (input, left) = factor(input)?;
    let (input, right) = term_prime(input)?;

    Ok((input, TermNode::Start(left, Box::new(right))))
}

macro_rules! term_match {
    ($input:ident, $token:pat, $node:path) => {{
        let (input, _) = token!($input, $token)?;
        let (input, left) = factor(input)?;
        let (input, prime) = term_prime(input)?;
        (input, $node(left, Box::new(prime)))
    }};
}

fn term_prime(input: ParseInput<'_>) -> ParseResult<TermNode> {
    let (input, item) = peek(input)?;

    let (input, node) = match item.token {
        Token::Multiply => term_match!(input, Token::Multiply, TermNode::Multiply),
        Token::Divide => term_match!(input, Token::Divide, TermNode::Divide),
        _ => (input, TermNode::Nop),
    };

    Ok((input, node))
}

/*
 * -------------------
 * Grammar:
 * factor := ( expression )
 *         | procedure_call
 *         | [ - ] name
 *         | [ - ] number
 *         | string
 *         | true
 *         | false
 *
 */

#[derive(Debug)]
pub enum FactorNode {
    Expression(Box<ExpressionNode>),
    ProcedureCall(ProcedureCallNode),
    Name {
        identifier: String,
        negated: bool,
        index_of: Option<Box<ExpressionNode>>,
    },
    Number(NumberNode),
    String(String),
    True,
    False,
}
pub fn factor(input: ParseInput<'_>) -> ParseResult<FactorNode> {
    let (input, item) = peek(input)?;

    let (input, node) = match item.token {
        Token::LeftParenthesis => {
            let (input, expr) = delimited(left_parenthesis, expression, right_parenthesis)(input)?;
            (input, FactorNode::Expression(Box::new(expr)))
        }
        Token::Identifier(_) => {
            let procedure_call_result = procedure_call(input.clone());
            if let Ok((input, node)) = procedure_call_result {
                (input, FactorNode::ProcedureCall(node))
            } else {
                let (input, ident) = identifier(input)?;
                let (input, index_of) = opt(index)(input)?;
                let index_of = match index_of {
                    Some(expr) => Some(Box::new(expr)),
                    None => None,
                };
                (
                    input,
                    FactorNode::Name {
                        identifier: ident,
                        negated: false,
                        index_of,
                    },
                )
            }
        }
        Token::Minus => {
            let (input, _) = token!(input, Token::Minus)?;
            let number_result = number(input.clone());
            if let Ok((input, node)) = number_result {
                (
                    input,
                    // Multiply the literals by -1
                    FactorNode::Number(match node {
                        NumberNode::IntegerLiteral(i) => NumberNode::IntegerLiteral(-i),
                        NumberNode::FloatLiteral(f) => NumberNode::FloatLiteral(-f),
                    }),
                )
            } else {
                let (input, ident) = identifier(input)?;
                let (input, index_of) = opt(index)(input)?;
                let index_of = match index_of {
                    Some(expr) => Some(Box::new(expr)),
                    None => None,
                };
                (
                    input,
                    FactorNode::Name {
                        identifier: ident,
                        negated: true,
                        index_of,
                    },
                )
            }
        }
        Token::Integer(_) | Token::Float(_) => {
            let (input, number_node) = number(input)?;
            (input, FactorNode::Number(number_node))
        }
        Token::String(s) => {
            let (input, _) = token!(input, Token::String(_))?;
            (input, FactorNode::String(s))
        }
        Token::KwTrue => {
            let (input, _) = token!(input, Token::KwTrue)?;
            (input, FactorNode::True)
        }
        Token::KwFalse => {
            let (input, _) = token!(input, Token::KwFalse)?;
            (input, FactorNode::False)
        }
        _ => return parse_error!("Expected factor.", item),
    };

    Ok((input, node))
}
