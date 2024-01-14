use super::util::{ParseInput, ParseResult};

#[derive(Debug)]
pub enum ExpressionNode {
    Todo,
}
pub fn expression(input: ParseInput<'_>) -> ParseResult<ExpressionNode> {
    Ok((input, ExpressionNode::Todo))
}
