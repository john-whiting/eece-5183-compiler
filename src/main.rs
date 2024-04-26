use std::{
    fs,
    io::{self, Write},
    path::PathBuf,
    process::{exit, Command},
    rc::Rc,
};

use clap::Parser;
use eece_5183_compiler::{
    generator::{CodeGeneratorContext, OwnedCodeGenerator},
    parser::{program, util::ParseError},
    scanner::Scanner,
};
use inkwell::context::Context;
use thiserror::Error;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// The path to clang-17
    clang_path: PathBuf,

    /// The path to the src code file
    src_path: PathBuf,

    /// The path to output the executable file
    #[arg(short = 'o', long)]
    output_path: Option<PathBuf>,

    /// Emits the LLVM IR ASM representation
    #[arg(short = 'a', long)]
    emit_asm: bool,

    /// Emits the LLVM IR bitcode representation
    #[arg(short = 'b', long)]
    emit_bc: bool,
}

#[derive(Error, Debug)]
enum ParserError {
    #[error("{0:?}")]
    Error(ParseError),
}

fn compile_program(
    context: Rc<CodeGeneratorContext<'_>>,
    program_raw: String,
) -> anyhow::Result<()> {
    let scanner = Scanner::new(&program_raw);
    let (_, parsed) = program(scanner).map_err(|err| ParserError::Error(err))?;

    parsed.generate_code(context, None)?;

    Ok(())
}

fn main() {
    let args = Args::parse();

    let clang_path = args.clang_path;
    let clang_path_str = clang_path.as_os_str().to_string_lossy();

    if !clang_path.exists() {
        println!("No executable could be found at {clang_path_str}. Expected clang-17.");
        exit(1);
    }
    if clang_path.is_dir() {
        println!("Found a directory at {clang_path_str}. Expected clang-17.");
        exit(1);
    }

    let src_path = args.src_path;
    let src_path_str = src_path.as_os_str().to_string_lossy();
    let src_path_base_file_name = src_path
        .file_name()
        .expect("Src path should have a filename.")
        .to_string_lossy()
        .split(".")
        .next()
        .expect("There should be a filename.")
        .to_string();

    if !src_path.exists() {
        println!("No source file could be found at {src_path_str}. Expected a source file.");
        exit(1);
    }
    if src_path.is_dir() {
        println!("Found a directory at {src_path_str}. Expected a single source file.");
        exit(1);
    }

    let outside_context = Context::create();
    let _context = CodeGeneratorContext::new(&outside_context);
    let context = Rc::new(_context);

    let basic_program = if let Ok(file_data) = fs::read_to_string(&src_path) {
        file_data
    } else {
        println!("Unable to read {src_path_str}");
        exit(1);
    };

    if let Err(err) = compile_program(Rc::clone(&context), basic_program) {
        println!("An error occurred when compiling the program!");
        println!("{err}");
        exit(1);
    }

    let output_path = args
        .output_path
        .unwrap_or(src_path.with_file_name(src_path_base_file_name));
    let output_path_str = output_path.as_os_str().to_string_lossy();
    let output_path_base_file_name = output_path
        .file_name()
        .expect("Output path should have a filename.")
        .to_string_lossy()
        .split(".")
        .next()
        .expect("There should be a filename.")
        .to_string();

    let asm_path = output_path.with_file_name(format!("{output_path_base_file_name}.ll"));
    let bc_path = output_path.with_file_name(format!("{output_path_base_file_name}.bc"));

    if args.emit_asm {
        if let Err(err) = context.module.print_to_file(asm_path) {
            println!("Failed to output LLVM IR ASM.\n{err}");
            exit(1);
        }
    }

    context.module.write_bitcode_to_path(&bc_path);

    let clang_result = Command::new(clang_path)
        .arg(&bc_path)
        .arg("-o")
        .arg(&output_path)
        .output();

    match clang_result {
        Ok(output) => {
            if !output.status.success() {
                println!("Clang failed to run to completion.");
                io::stderr().write_all(&output.stderr).unwrap();
                exit(2);
            }
        }
        Err(err) => {
            println!("Clang failed to run to completion.\n{err}");
            exit(2);
        }
    }

    if !args.emit_bc {
        fs::remove_file(bc_path)
            .expect("The bitcode file was created, it should also be able to be removed.");
    }

    println!("Successfully compiled {src_path_str} and outputted the result to {output_path_str}!");
}
