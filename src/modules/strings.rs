use crate::checker::{Context, Module, Stack, Value, TYPE_ID_BOOL, TYPE_ID_STRING};
use crate::make_function;
use std::rc::Rc;

pub fn add_module(context: &mut Context) {
    let mut module = Module {
        identifier: "strings".to_string(),
        consts: vec![],
        types: vec![],
        templates: vec![],
        functions: vec![],
    };

    let contains = make_function(
        context,
        "contains",
        vec![
            ("string".to_string(), TYPE_ID_STRING),
            ("substring".to_string(), TYPE_ID_STRING),
        ],
        TYPE_ID_BOOL,
        Rc::new(|stack: &Stack, _| {
            let string = stack
                .read_rel(1)
                .as_string()
                .expect("this should not have passed type checking");
            let substring = stack
                .read_rel(0)
                .as_string()
                .expect("this should not have passed type checking");
            Value::Bool(string.contains(substring))
        }),
    );

    module.functions.push(context.add_function(contains));
    context.add_module(module);
}
