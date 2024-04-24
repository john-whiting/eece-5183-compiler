use crate::token::Token;

use self::{
    general::{global, identifier},
    procedure::{procedure_declaration, ProcedureDeclarationNode},
    statement::{statement, StatementNode},
    util::{many0, not_partial, opt, parse_error, peek, token, ParseInput, ParseResult},
    variable::{variable_declaration, VariableDeclarationNode},
};

pub mod expression;
pub mod general;
pub mod procedure;
pub mod statement;
pub mod variable;

mod util;

#[derive(Debug)]
pub enum DeclarationType {
    Procedure(ProcedureDeclarationNode),
    Variable(VariableDeclarationNode),
}
#[derive(Debug)]
pub enum DeclarationNode {
    Global(DeclarationType),
    Local(DeclarationType),
}

fn declaration(input: ParseInput<'_>) -> ParseResult<DeclarationNode> {
    let (input, is_global) = opt(global)(input)?;
    let is_global = is_global.is_some();

    let (input, next_item) = peek(input)?;

    let (input, declaration) = match next_item.token {
        Token::KwProcedure => {
            let (i, result) = procedure_declaration(input)?;
            (i, DeclarationType::Procedure(result))
        }
        Token::KwVariable => {
            let (i, result) = variable_declaration(input)?;
            (i, DeclarationType::Variable(result))
        }
        _ if is_global => {
            return parse_error!("Expected declaration following \"global\".", next_item)
        }
        _ => return not_partial!(parse_error!("Expected declaration.", next_item)),
    };

    let (input, _) = token!(input, Token::SemiColon)?;

    Ok((
        input,
        match is_global {
            true => DeclarationNode::Global(declaration),
            false => DeclarationNode::Local(declaration),
        },
    ))
}

#[derive(Debug)]
pub struct ProgramHeaderNode(String);
fn program_header(input: ParseInput<'_>) -> ParseResult<ProgramHeaderNode> {
    let (input, _) = token!(input, Token::KwProgram)?;
    let (input, program_identifier) = identifier(input)?;
    let (input, _) = token!(input, Token::KwIs)?;
    Ok((input, ProgramHeaderNode(program_identifier)))
}

#[derive(Debug)]
pub struct ProgramBodyNode {
    pub declarations: Vec<DeclarationNode>,
    pub statements: Vec<StatementNode>,
}
fn program_body(input: ParseInput<'_>) -> ParseResult<ProgramBodyNode> {
    let (input, declarations) = many0(declaration)(input)?;
    let (input, _) = token!(input, Token::KwBegin)?;
    let (input, statements) = many0(statement)(input)?;
    let (input, _) = token!(input, Token::KwEnd)?;
    let (input, _) = token!(input, Token::KwProgram)?;

    // NOTE: LANGUAGE SEMANTICS | RULE #3
    // ALL DECLARATIONS IN THE PROGRAM SCOPE ARE GLOBAL
    let declarations = declarations
        .into_iter()
        .map(|declaration| match declaration {
            DeclarationNode::Local(x) => DeclarationNode::Global(x),
            _ => declaration,
        })
        .collect();

    Ok((
        input,
        ProgramBodyNode {
            declarations,
            statements,
        },
    ))
}

#[derive(Debug)]
pub struct ProgramNode(pub ProgramHeaderNode, pub ProgramBodyNode);

pub fn program(input: ParseInput<'_>) -> ParseResult<ProgramNode> {
    let (input, header) = program_header(input)?;
    let (input, body) = program_body(input)?;
    let (input, _) = token!(input, Token::Period)?;
    let (input, _) = token!(input, Token::EOF)?;

    Ok((input, ProgramNode(header, body)))
}

#[cfg(test)]
mod tests {
    use crate::scanner::Scanner;

    use super::*;

    #[test]
    fn basic_program() {
        let input = r#"
            program HelloWorld is
           
            global variable my_var : integer;
            variable my_var2 : bool [3];

            procedure test : float (variable t1 : integer, variable t2 : string, variable t3 : bool)
                variable inside_var : string;
            begin end procedure;

            global procedure test2 : string ()
            begin end procedure;

            begin
                // Assignment Statements
                my_var := my_var2 + 5 * 10.4;
                my_var2 [my_var] := 5 < 10;

                // If statements
                If (not my_var2) then end if;
                if (my_var + 4 - 2 > -2) then
                else end if;

                // Loop Statements
                for (my_var := 0; my_var < 10)
                    my_var := my_var + 1 + my_var2[10];
                end for;

                // Return Statements
                return test(5, "hello", true);
            end program.
            "#;

        let scanner = Scanner::new(input);

        let parsed = program(scanner);

        let (_, program_node) = parsed.unwrap();

        dbg!(program_node);
    }
}
