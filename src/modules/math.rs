use crate::checker::{
    Context, Module, Stack, Type, TypeVarConstraint, Value, TYPE_ID_FLOAT, TYPE_ID_INT,
};
use crate::make_function;
use std::rc::Rc;

pub fn add_module(context: &mut Context) {
    let mut module = Module {
        identifier: "math".to_string(),
        consts: vec![],
        types: vec![],
        templates: vec![],
        functions: vec![],
    };

    let number = context.add_type(Type::TypeVar {
        name: "Number".to_string(),
        constraint: Some(TypeVarConstraint::new_union(vec![
            TYPE_ID_FLOAT,
            TYPE_ID_INT,
        ])),
    });

    let max = make_function(
        context,
        "min",
        vec![
            ("first".to_string(), number),
            ("second".to_string(), number),
        ],
        number,
        Rc::new(|stack: &Stack, _| {
            let first = stack.read_rel(1);
            let second = stack.read_rel(0);
            match (first, second) {
                (Value::Float(first), Value::Float(second)) => {
                    let m = (*first).max(*second);
                    return Value::Float(m);
                }
                (Value::Int(first), Value::Int(second)) => {
                    return Value::Int(*first.max(second));
                }
                _ => {}
            }
            unreachable!()
        }),
    );

    let min = make_function(
        context,
        "min",
        vec![
            ("first".to_string(), number),
            ("second".to_string(), number),
        ],
        number,
        Rc::new(|stack: &Stack, _| {
            let first = stack.read_rel(1);
            let second = stack.read_rel(0);
            match (first, second) {
                (Value::Float(first), Value::Float(second)) => {
                    let m = (*first).min(*second);
                    return Value::Float(m);
                }
                (Value::Int(first), Value::Int(second)) => {
                    return Value::Int(*first.min(second));
                }
                _ => {}
            }
            unreachable!()
        }),
    );

    module.functions.push(context.add_function(max));
    module.functions.push(context.add_function(min));

    context.add_module(module);
}
