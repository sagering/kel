use clap::Parser;
use kl::checker::{Compiler, Context, Scope, Stack};
use kl::eval::evaluate_module;
use kl::format_error;
use kl::modules::lists;
use kl::modules::math;
use kl::modules::strings;
use std::fs;
use std::time::Instant;
use std::{env, path};

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[clap(short, long, value_parser, num_args = 1.., value_delimiter = ' ')]
    files: Vec<String>,

    #[arg(short = 'e', long = "eval", required = true)]
    eval: String,
}

fn main() {
    match env::current_dir() {
        Ok(cwd) => println!("Working Directory: {:?}", cwd),
        Err(err) => {
            eprintln!("Error getting current directory: {}", err);
            return;
        }
    }

    let args = Args::parse();

    let mut files: Vec<(String, String)> = Vec::new();

    for file in &args.files {
        let mut pb = path::PathBuf::new();
        pb.push(file);

        match pb.canonicalize() {
            Ok(abs_path) => {
                if abs_path.exists() {
                    match fs::read_to_string(&abs_path) {
                        Ok(contents) => {
                            files.push((abs_path.to_str().unwrap().to_string(), contents));
                        }
                        Err(err) => {
                            eprintln!("Error reading {:?}: {}", abs_path, err);
                        }
                    }
                } else {
                    eprintln!("Error: File {:?} does not exist!", abs_path);
                }
            }
            Err(_) => {
                eprintln!("Error: Cannot resolve path {:?}", file);
            }
        }
    }

    // validate eval string
    let eval_path = path::PathBuf::from(&args.eval);
    let eval_path = match eval_path.canonicalize() {
        Ok(abs_path) => {
            if !abs_path.exists() {
                eprintln!("Error: Eval file {:?} does not exist!", abs_path);
                return;
            }
            abs_path.to_str().unwrap().to_string()
        }
        Err(_) => {
            eprintln!("Error: Cannot resolve path {:?}", args.eval);
            return;
        }
    };

    let start_time = Instant::now();

    let mut compiler = Compiler::new();

    for (path, contents) in files.into_iter() {
        compiler.add_file(path, contents);
    }

    let mut context = Context::new();

    math::add_module(&mut context);
    strings::add_module(&mut context);
    lists::add_module(&mut context);

    let mut scope = Scope::new();

    let result = compiler
        .get_or_compile_module(&mut context, &mut scope, &eval_path)
        .and_then(|module_id| {
            let mut stack = Stack::new();
            let module = context.get_module(module_id);
            evaluate_module(&mut stack, &context, &module)
        });

    let elapsed_time = start_time.elapsed(); // Calculate elapsed time

    println!("Elapsed time: {:.2?}", elapsed_time);
    println!("\n");

    match result {
        Err(e) => {
            let error_lines = format_error(&compiler, &e);
            for line in error_lines {
                eprint!("{}", line);
            }
        }
        _ => {}
    }
}
