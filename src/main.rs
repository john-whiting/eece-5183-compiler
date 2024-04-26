use std::{path::Path, rc::Rc};

use eece_5183_compiler::{generator::{CodeGeneratorContext, OwnedCodeGenerator}, parser::{program, util::ParseError}, scanner::Scanner};
use inkwell::context::Context;
use thiserror::Error;

#[derive(Error, Debug)]
enum ParserError {
    #[error("{0:?}")]
    Error(ParseError)
}

fn compile_program(context: Rc<CodeGeneratorContext<'_>>, program_raw: String) -> anyhow::Result<()> {
    let scanner = Scanner::new(&program_raw);
    let (_, parsed) = program(scanner).map_err(|err| ParserError::Error(err))?;

    parsed.generate_code(context, None)?;

    Ok(())
}

fn main() {
    let outside_context = Context::create();
    let _context = CodeGeneratorContext::new(&outside_context);
    let context = Rc::new(_context);
    
    let basic_program = include_str!("../spec/basic.src").to_string();

    compile_program(Rc::clone(&context), basic_program).expect("Encounted error when compiling.");

    context.module.print_to_stderr();
    context.module.write_bitcode_to_path(Path::new("./spec/target/basic.bc"));

    println!("Hello, world!");
}
