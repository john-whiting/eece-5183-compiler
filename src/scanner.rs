use std::{collections::HashMap, iter::Peekable, str::Chars};

use crate::token::Token;

macro_rules! try_pop_next {
    ($s: ident, $x:expr) => {
        if $x {
            $s.pop();
            true
        } else {
            false
        }
    };
}

#[derive(Debug, Clone, PartialEq)]
pub struct ScannerItem {
    pub token: Token,
    pub line_count: usize,
    pub column_count: usize,
}

#[derive(Debug, Clone)]
pub struct Scanner<'a> {
    // TODO: Optimize for memory (Allow for &str and &Token)
    identifiers: HashMap<String, Token>,
    string_constants: HashMap<String, Token>,

    input: Peekable<Chars<'a>>,
    line_count: usize,
    column_count: usize,

    eof: bool,
}

impl<'a> Scanner<'a> {
    pub fn new(input: &'a str) -> Self {
        let identifiers = HashMap::from([
            // Declaration Prefixes
            ("program".to_string(), Token::KwProgram),
            ("procedure".to_string(), Token::KwProcedure),
            ("variable".to_string(), Token::KwVariable),
            // Scope Modifiers
            ("global".to_string(), Token::KwGlobal),
            // Block Markers
            ("begin".to_string(), Token::KwBegin),
            ("end".to_string(), Token::KwEnd),
            // Type Names
            ("integer".to_string(), Token::KwInteger),
            ("float".to_string(), Token::KwFloat),
            ("string".to_string(), Token::KwString),
            ("bool".to_string(), Token::KwBool),
            ("true".to_string(), Token::KwTrue),
            ("false".to_string(), Token::KwFalse),
            // Control Flow
            ("if".to_string(), Token::KwIf),
            ("then".to_string(), Token::KwThen),
            ("else".to_string(), Token::KwElse),
            ("for".to_string(), Token::KwFor),
            ("return".to_string(), Token::KwReturn),
            // Miscellaneous Keywords
            ("is".to_string(), Token::KwIs),
            ("not".to_string(), Token::KwNot),
        ]);

        Self {
            identifiers,
            string_constants: HashMap::new(),
            input: input.chars().peekable(),
            line_count: 1,
            column_count: 0,
            eof: false,
        }
    }

    fn pop(&mut self) -> Option<char> {
        self.column_count += 1;
        self.input.next()
    }

    fn pop_next_valid(&mut self) -> Option<char> {
        let mut c = self.pop()?;

        let mut is_comment = false;
        let mut multiline_comment_count = 0;

        while match c {
            // Always ignore newline whitespace and reset is_comment
            '\n' => {
                is_comment = false;
                self.line_count += 1;
                self.column_count = 0;
                true
            }
            '\r' if try_pop_next!(self, self.input.peek() == Some(&'\n')) => {
                is_comment = false;
                self.line_count += 1;
                self.column_count = 0;
                true
            }
            // Start comment
            '/' if self.input.peek() == Some(&'/') && multiline_comment_count == 0 => {
                is_comment = true;
                true
            }
            // Start multiline comment
            '/' if self.input.peek() == Some(&'*') => {
                multiline_comment_count += 1;
                true
            }
            // End multiline comment
            '*' if try_pop_next!(
                self,
                multiline_comment_count > 0 && self.input.peek() == Some(&'/')
            ) =>
            {
                multiline_comment_count -= 1;
                true
            }
            // Ignore anything if we are inside a comment or if it is whitespace
            _ if is_comment || multiline_comment_count > 0 || c.is_whitespace() => true,
            // Nothing else to ignore
            _ => false,
        } {
            c = self.pop()?;
        }

        Some(c)
    }
}

impl Iterator for Scanner<'_> {
    type Item = ScannerItem;

    fn next(&mut self) -> Option<Self::Item> {
        let c = self.pop_next_valid();

        let mut line_count = self.line_count;
        let mut column_count = self.column_count;

        if c.is_none() && !self.eof {
            self.eof = true;
            return Some(ScannerItem {
                token: Token::EOF,
                line_count,
                column_count,
            });
        }

        let c = c?;

        let next_is_eq = self.input.peek() == Some(&'=');

        let token = match c {
            // Structure Symbols
            '(' => Token::LeftParenthesis,
            ')' => Token::RightParenthesis,
            '[' => Token::LeftBracket,
            ']' => Token::RightBracket,
            ',' => Token::Comma,
            ';' => Token::SemiColon,
            '.' => Token::Period,

            // Math Symbols
            '&' => Token::BitAnd,
            '|' => Token::BitOr,
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Multiply,
            '/' => Token::Divide,

            // Comparison Operators
            '<' if try_pop_next!(self, next_is_eq) => Token::LessThanEqual,
            '<' => Token::LessThan,
            '>' if try_pop_next!(self, next_is_eq) => Token::GreaterThanEqual,
            '>' => Token::GreaterThan,
            '=' if try_pop_next!(self, next_is_eq) => Token::Equal,
            '!' if try_pop_next!(self, next_is_eq) => Token::NotEqual,

            // Assignment Operator
            ':' if try_pop_next!(self, next_is_eq) => Token::AssignmentOperator,

            // Colon
            ':' => Token::Colon,

            // Numbers
            '0'..='9' => {
                let mut num: String = c.to_string();

                // Match against beginning part of the number
                while matches!(self.input.peek(), Some('0'..='9')) {
                    num.push(self.pop().unwrap())
                }

                // Catch Floats
                if self.input.peek() == Some(&'.') {
                    num.push(self.pop().unwrap());
                    // Match against the rest of the number
                    while matches!(self.input.peek(), Some('0'..='9')) {
                        num.push(self.pop().unwrap())
                    }
                    Token::Float(num.parse().unwrap())
                } else {
                    Token::Integer(num.parse().unwrap())
                }
            }

            // Strings
            '"' => {
                let mut s = String::new();

                while !matches!(self.input.peek(), Some('"') | None) {
                    s.push(self.pop().unwrap())
                }

                // This is either a Quote or the end of the file...
                // Either way, we can pop it
                self.pop();

                self.string_constants
                    .entry(s.clone())
                    .or_insert(Token::String(s))
                    .clone()
            }

            // Identifiers and Keywords
            'a'..='z' | 'A'..='Z' => {
                let mut identifier = c.to_string();

                while matches!(
                    self.input.peek(),
                    Some('_') | Some('a'..='z') | Some('A'..='Z') | Some('0'..='9')
                ) {
                    identifier.push(self.pop().unwrap());
                }

                identifier = identifier.to_lowercase();

                self.identifiers
                    .entry(identifier.clone())
                    .or_insert(Token::Identifier(identifier))
                    .clone()
            }

            // Illegal Characters
            _ => {
                let next = self.next()?;
                line_count = next.line_count;
                column_count = next.column_count;
                next.token
            }
        };

        Some(ScannerItem {
            token,
            line_count,
            column_count,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest]
    #[case("program", Token::KwProgram)]
    #[case("procedure", Token::KwProcedure)]
    #[case("variable", Token::KwVariable)]
    #[case("global", Token::KwGlobal)]
    #[case("begin", Token::KwBegin)]
    #[case("end", Token::KwEnd)]
    #[case("integer", Token::KwInteger)]
    #[case("float", Token::KwFloat)]
    #[case("string", Token::KwString)]
    #[case("bool", Token::KwBool)]
    #[case("true", Token::KwTrue)]
    #[case("false", Token::KwFalse)]
    #[case("if", Token::KwIf)]
    #[case("If", Token::KwIf)]
    #[case("iF", Token::KwIf)]
    #[case("then", Token::KwThen)]
    #[case("else", Token::KwElse)]
    #[case("for", Token::KwFor)]
    #[case("return", Token::KwReturn)]
    #[case("is", Token::KwIs)]
    #[case("not", Token::KwNot)]
    #[case("(", Token::LeftParenthesis)]
    #[case(")", Token::RightParenthesis)]
    #[case("[", Token::LeftBracket)]
    #[case("]", Token::RightBracket)]
    #[case(",", Token::Comma)]
    #[case(":", Token::Colon)]
    #[case(";", Token::SemiColon)]
    #[case(".", Token::Period)]
    #[case("&", Token::BitAnd)]
    #[case("|", Token::BitOr)]
    #[case("+", Token::Plus)]
    #[case("-", Token::Minus)]
    #[case("*", Token::Multiply)]
    #[case("/", Token::Divide)]
    #[case("<", Token::LessThan)]
    #[case("<=", Token::LessThanEqual)]
    #[case(">", Token::GreaterThan)]
    #[case(">=", Token::GreaterThanEqual)]
    #[case("==", Token::Equal)]
    #[case("!=", Token::NotEqual)]
    #[case("hello", Token::Identifier("hello".to_string()))]
    #[case("hElLO", Token::Identifier("hello".to_string()))]
    #[case("\"hello\"", Token::String("hello".to_string()))]
    #[case("8675309", Token::Integer(8675309))]
    #[case("1.23", Token::Float(1.23))]
    #[case(":=", Token::AssignmentOperator)]
    fn correct_tokens(#[case] input: String, #[case] token: Token) {
        let scanner = Scanner::new(&input);

        let item = ScannerItem {
            token,
            line_count: 1,
            column_count: 1,
        };

        let eof = ScannerItem {
            token: Token::EOF,
            line_count: 1,
            column_count: input.len() + 1,
        };

        assert_eq!(scanner.collect::<Vec<_>>(), vec![item, eof]);
    }

    #[rstest]
    #[case("\n  >", Token::GreaterThan, 2, 3)]
    #[case("\t>", Token::GreaterThan, 1, 2)]
    #[case("// Single Line Comment\n>", Token::GreaterThan, 2, 1)]
    #[case("/* Multi\nLine\nComment\n*/\n>", Token::GreaterThan, 5, 1)]
    #[case("/* Multi\r\nLine\r\nComment\r\n*/\r\n>", Token::GreaterThan, 5, 1)]
    #[case("/* BC with // double slashes */>", Token::GreaterThan, 1, 32)]
    #[case("/* /* BC inside of BC */ */>", Token::GreaterThan, 1, 28)]
    fn ignorables(
        #[case] input: String,
        #[case] token: Token,
        #[case] line_count: usize,
        #[case] column_count: usize,
    ) {
        let scanner = Scanner::new(&input);

        let item = ScannerItem {
            token,
            line_count,
            column_count,
        };
        let eof = ScannerItem {
            token: Token::EOF,
            line_count,
            column_count: column_count + 1,
        };

        assert_eq!(scanner.collect::<Vec<_>>(), vec![item, eof]);
    }
}
