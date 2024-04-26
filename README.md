# EECE 5183 Compiler

## Requirements

- The Rust Toolchain
    - Everything is built in Rust, so some packages, Cargo, and the Rust compiler are used.
- LLVM-17
    - There is an `install-llvm.sh` script for installing all the required tools on Ubuntu
- Clang-17
    - This is used to simplify linking the LLVM IR Bitcode to libc

## Usage

The usage of the executable can be seen below.

```
Usage: eece-5183-compiler [OPTIONS] <CLANG_PATH> <SRC_PATH>

Arguments:
  <CLANG_PATH>  The path to clang-17
  <SRC_PATH>    The path to the src code file

Options:
  -o, --output-path <OUTPUT_PATH>  The path to output the executable file
  -a, --emit-asm                   Emits the LLVM IR ASM representation
  -b, --emit-bc                    Emits the LLVM IR bitcode representation
  -h, --help                       Print help
  -V, --version                    Print version
```

NOTE: The clang-17 path is required for the compiler to work. On Linux, it is recommended to use a command like the following when running the compiler:

`./target/release/eece-5183-compiler $(which clang-17) ./my_program.src`

## Building the Compiler

1. Cargo crates must be installed for the build, if they have not been installed, please run `cargo install`
2. Run `cargo build --release`
3. The executable can be found in `./target/release/eece-5183-compiler` relative to the project root

## Remaining Tasks

- Implement mass-array operations (like `a = b + 1` where `a` and `b` are same-sized arrays)
- Add line/column numbers to code generator errors.
- Add Resync capabilities when an error is met
