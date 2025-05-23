pub mod checker;
pub mod common;
pub mod eval;
pub mod lexer;
pub mod modules;
pub mod parser;

use checker::{Compiler, Context, Expression, Function, Stack, Type, TypeId, Value};
use common::{Error, Span};
use std::rc::Rc;

fn highlight_error_line(compiler: &Compiler, span: Span) -> (String, Vec<String>) {
    let (file_path, content) = compiler.resolve_file_id(span.file_id);

    let mut offset = 0;
    let mut line_no = 1;
    let mut column_start = 0;
    let mut column_end = 0;
    let mut parts = Vec::new();

    for line in content.lines() {
        let line_len = line.len() + 1; // +1 for newline

        if span.start >= offset && span.start < offset + line_len {
            column_start = span.start - offset + 1;
            column_end = (span.end - offset + 1).min(offset + line_len);
            parts.push(line.to_string());
            break;
        }

        offset += line_len;
        line_no += 1;
    }

    // If the span is on a single line, provide column information.
    let location = format!("{}:{}", file_path, line_no);

    // Create an indicator line that points to the specific error.
    if column_start > 0 {
        let indicator = format!(
            "{}{}",
            " ".repeat(column_start - 1),
            "^".repeat(if column_end > column_start {
                column_end - column_start
            } else {
                1
            })
        );
        parts.push(indicator);
    }

    (location, parts)
}

pub fn format_error(compiler: &Compiler, error: &Error) -> Vec<String> {
    let mut buffer = Vec::new();

    match error {
        Error::ParserError(msg, span)
        | Error::LexerError(msg, span)
        | Error::CheckerError(msg, span) => {
            let (location, highlight) = highlight_error_line(compiler, *span);

            buffer.push(format!("\x1b[1;31mError\x1b[0m: {}\n", msg));
            buffer.push(format!("  --> {}\n", location));

            // If we have line information from the span resolver
            if highlight.len() >= 1 {
                // Print the line with the error
                buffer.push(format!("      | {}\n", highlight[0]));

                // If we have an indicator line
                if highlight.len() == 2 {
                    buffer.push(format!("      | \x1b[1;31m{}\x1b[0m\n", highlight[1]));
                }
            } else {
                // Fallback to reading the file directly if we don't have detailed span information
                if let Some(file_path) = location.split(':').next() {
                    if let Ok(contents) = std::fs::read_to_string(file_path) {
                        if let Some(line_str) = location.split(':').nth(1) {
                            if let Ok(line_no) = line_str.parse::<usize>() {
                                let lines: Vec<&str> = contents.lines().collect();
                                let line_index = line_no - 1;

                                if line_index < lines.len() {
                                    buffer.push(format!("      | {}\n", lines[line_index]));

                                    // Get column information if available
                                    if let Some(col_range) = location.split(':').nth(2) {
                                        if let Some(col_start_str) = col_range.split('-').next() {
                                            if let Ok(col_start) = col_start_str.parse::<usize>() {
                                                let col_end = col_range
                                                    .split('-')
                                                    .nth(1)
                                                    .and_then(|s| s.parse::<usize>().ok())
                                                    .unwrap_or(col_start + 1);
                                                let indicator = format!(
                                                    "{}{}",
                                                    " ".repeat(col_start - 1),
                                                    "^".repeat(col_end - col_start)
                                                );
                                                buffer.push(format!(
                                                    "      | \x1b[1;31m{}\x1b[0m\n",
                                                    indicator
                                                ));
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        Error::CompilerError(msg) => {
            buffer.push(format!("\x1b[1;31mCompilerError\x1b[0m: {:?}\n", msg));
        }
        Error::EvaluationError(msg) => {
            buffer.push(format!("\x1b[1;31mEvaluationError\x1b[0m: {:?}\n", msg));
        }
    }

    buffer
}

pub fn value_to_string(value: &Value) -> String {
    match value {
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => s.clone(),
        Value::Bool(b) => b.to_string(),
        Value::List(l, ..) => {
            let mut result = String::from("[");
            for (i, v) in l.iter().enumerate() {
                if i > 0 {
                    result.push_str(", ");
                }
                result.push_str(&value_to_string(v));
            }
            result.push(']');
            result
        }
        Value::Object(o, ..) => {
            let mut result = String::from("{");
            for (i, (k, v)) in o.iter().enumerate() {
                if i > 0 {
                    result.push_str(", ");
                }
                result.push_str(&format!("\"{}\": {}", k, value_to_string(v)));
            }
            result.push('}');
            result
        }
        Value::Range(start, end) => format!("{}..{}", start, end),
        Value::Null => "null".to_string(),
        Value::Type(typ) => format!("Type({})", typ),
        Value::Function(_, _) => "Function".to_string(),
        Value::Template(_, _) => "Template".to_string(),
    }
}

pub fn make_function(
    context: &mut Context,
    name: &str,
    pars: Vec<(String, TypeId)>,
    return_type: TypeId,
    implementation: Rc<dyn Fn(&Stack, &[Function]) -> Value>,
) -> Function {
    let type_id = context.add_type(Type::Function {
        pars: pars.iter().map(|(_, t)| *t).collect(),
        return_type,
    });

    Function {
        name: name.to_string(),
        pars,
        return_type,
        type_id,
        expr: Expression::BlockExpression {
            func: implementation,
            typ: return_type,
        },
    }
}
