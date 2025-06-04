use kl::checker::{Compiler, Context, Scope, Stack};
use kl::eval::initialize_stack;

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
            initialize_stack(&mut stack, &mut context)?;
            let module = context.get_module(module_id);

            for const_id in &module.consts {
                println!(
                    "{} = {:?}",
                    context.get_const(*const_id).name,
                    stack.read_abs(*const_id)
                );
            }

            Ok(())
        });
}
