use kl::checker::{Compiler, Context, Scope, Stack};
use kl::eval::evaluate_module;

fn main() {
    let mut compiler = Compiler::new();
    let code = "hi = \"Hello World!\"";
    compiler.add_file("code.kl".to_string(), code.to_string());

    let mut context = Context::new();
    let mut scope = Scope::new();

    let _ = compiler
        .get_or_compile_module(&mut context, &mut scope, "code.kl")
        .and_then(|module_id| {
            let mut stack = Stack::new();
            let module = context.get_module(module_id);
            evaluate_module(&mut stack, &context, &module)
        });
}
