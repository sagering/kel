use kl::checker::{Compiler, Context, Scope, Stack};
use kl::eval::evaluate_module;
use kl::format_error;
use kl::modules::lists;
use kl::modules::math;
use kl::modules::strings;
use std::fs;
use std::{env, path};

fn add_file(file_path: &str, compiler: &mut Compiler) -> String {
    let abs_path = path::PathBuf::from(file_path)
        .canonicalize()
        .expect("Unable to resolve path")
        .to_str()
        .expect("File paths should be convertible to str")
        .to_string();

    let contents = fs::read_to_string(abs_path.clone()).expect("Unable to read file");
    compiler.add_file(abs_path.clone(), contents);

    abs_path
}

fn run(file_path: &str, include: &[&str]) -> String {
    println!(
        "Current working directory: {}",
        env::current_dir().unwrap().display()
    );

    let mut compiler = Compiler::new();
    let mut context = Context::new();

    let file_path = add_file(file_path, &mut compiler);

    math::add_module(&mut context);
    strings::add_module(&mut context);
    lists::add_module(&mut context);

    let mut scope = Scope::new();

    for include_path in include {
        add_file(include_path, &mut compiler);
    }

    let result = compiler
        .get_or_compile_module(&mut context, &mut scope, &file_path)
        .and_then(|module_id| {
            let mut stack = Stack::new();
            let module = context.get_module(module_id);
            evaluate_module(&mut stack, &context, &module)
        });

    match result {
        Err(e) => {
            let error_lines = format_error(&compiler, &e);
            return error_lines.join("\n");
        }
        _ => {}
    }

    "".to_string()
}

#[test]
fn basic_types() {
    run("./tests/basic_types.kl", &[]);
}

#[test]
fn calls() {
    assert_eq!(run("./tests/calls.kl", &[]), "");
}

#[test]
fn imports() {
    assert_eq!(
        run(
            "./tests/imports.kl",
            &["./tests/basic_types.kl", "./tests/custom_types.kl"]
        ),
        ""
    );
}

#[test]
fn functions() {
    assert_eq!(run("./tests/functions.kl", &[]), "");
}

#[test]
fn lists() {
    assert_eq!(run("./tests/lists.kl", &[]), "");
}

#[test]
fn oneofs() {
    assert_eq!(run("./tests/oneofs.kl", &[]), "");
}

#[test]
fn optionals() {
    assert_eq!(run("./tests/optionals.kl", &[]), "");
}

#[test]
fn strings() {
    assert_eq!(run("./tests/strings.kl", &[]), "");
}

#[test]
fn structs() {
    assert_eq!(run("./tests/structs.kl", &[]), "");
}

#[test]
fn templates() {
    assert_eq!(run("./tests/templates.kl", &[]), "");
}

#[test]
fn type_cast() {
    assert_eq!(run("./tests/type_cast.kl", &[]), "");
}

#[test]
fn type_test() {
    assert_eq!(run("./tests/type_test.kl", &[]), "");
}
