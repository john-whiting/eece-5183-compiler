use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Declaration Prefixes
    KwProgram,
    KwProcedure,
    KwVariable,

    // Scope Modifiers
    KwGlobal,

    // Block Markers
    KwBegin,
    KwEnd,

    // Type Names
    KwInteger,
    KwFloat,
    KwString,
    KwBool,
    KwTrue,
    KwFalse,

    // Control Flow
    KwIf,
    KwThen,
    KwElse,
    KwFor,
    KwReturn,

    // Miscellaneous Keywords
    KwIs,
    KwNot,

    // Structure Symbols
    LeftParenthesis,
    RightParenthesis,
    LeftBracket,
    RightBracket,
    Comma,
    Colon,
    SemiColon,
    Period,

    // Math Symbols
    BitAnd,
    BitOr,
    Plus,
    Minus,
    Multiply,
    Divide,

    // Comparison Operators
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Equal,
    NotEqual,

    // Variables
    Identifier(String),
    String(String),
    Integer(i64),
    Float(f64),
    AssignmentOperator,

    // EOF
    EOF,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
