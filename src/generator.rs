use inkwell::{builder::Builder, context::Context, module::Module};

pub struct CodeGeneratorContext<'a> {
    pub context: &'a Context,
    pub module: Module<'a>,
    pub builder: Builder<'a>,
}

mod expression;
mod util;

impl<'a> CodeGeneratorContext<'a> {
    pub fn new(context: &'a Context) -> Self {
        let module = context.create_module(env!("CARGO_PKG_NAME"));
        let builder = context.create_builder();

        Self {
            context,
            module,
            builder,
        }
    }
}

pub trait CodeGenerator<'a> {
    type Item;

    fn generate_code(self, context: &'a CodeGeneratorContext) -> anyhow::Result<Self::Item>;
}
