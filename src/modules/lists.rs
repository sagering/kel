use crate::{
    checker::{Context, Module, Stack, Type, Value, TYPE_ID_ANY, TYPE_ID_INT},
    make_function,
};
use std::rc::Rc;

pub fn add_module(context: &mut Context) {
    let mut module = Module {
        identifier: "lists".to_string(),
        consts: vec![],
        types: vec![],
        templates: vec![],
        functions: vec![],
    };

    let list_of_any = context.add_type(Type::List { inner: TYPE_ID_ANY });

    let contains = make_function(
        context,
        "len",
        vec![("list".to_string(), list_of_any)],
        TYPE_ID_INT,
        Rc::new(|stack: &Stack, _| {
            let list = stack
                .read_rel(0)
                .as_list()
                .expect("this should not have passed type checking");
            Value::Int(list.len() as i64)
        }),
    );

    module.functions.push(context.add_function(contains));
    context.add_module(module);
}
