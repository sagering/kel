use crate::{
    checker::{
        Context, Expression, FunctionId, ListElement, ModuleId, Stack, StringInterpolationPart,
        Type, TypeId, Value, TYPE_ID_BOOL, TYPE_ID_FLOAT, TYPE_ID_INT, TYPE_ID_RANGE,
        TYPE_ID_STRING, TYPE_ID_TYPE, TYPE_ID_UNKNOWN,
    },
    common::{Error, EvaluationError, Result},
    parser::Operator,
};
use std::collections::HashMap;

enum ValueIterator {
    List(std::vec::IntoIter<Value>),
    Range(i64, i64),
}

impl Iterator for ValueIterator {
    type Item = Value;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ValueIterator::List(iter) => iter.next(),
            ValueIterator::Range(start, end) => {
                if *start < *end {
                    let value = Value::Int(*start);
                    *start += 1;
                    Some(value)
                } else {
                    None
                }
            }
        }
    }
}

impl Value {
    pub fn add(&self, other: &Value) -> Result<Value> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs + rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs + rhs)),
            _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                format!("Cannot add {:?} and {:?}", self, other),
            ))),
        }
    }

    pub fn subtract(&self, other: &Value) -> Result<Value> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs - rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs - rhs)),
            _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                format!("Cannot subtract {:?} and {:?}", self, other),
            ))),
        }
    }

    pub fn multiply(&self, other: &Value) -> Result<Value> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs * rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs * rhs)),
            _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                format!("Cannot multiply {:?} and {:?}", self, other),
            ))),
        }
    }

    pub fn negate(&self) -> Result<Value> {
        match self {
            Value::Int(i) => Ok(Value::Int(-i)),
            Value::Float(f) => Ok(Value::Float(-f)),
            _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                format!("Cannot negate {:?}", self),
            ))),
        }
    }

    pub fn divide(&self, other: &Value) -> Result<Value> {
        match other {
            Value::Int(0) => return Err(Error::EvaluationError(EvaluationError::DivisionByZero)),
            Value::Float(0.0) => {
                return Err(Error::EvaluationError(EvaluationError::DivisionByZero))
            }
            _ => {}
        }
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs / rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs / rhs)),
            _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                format!("Cannot divide {:?} and {:?}", self, other),
            ))),
        }
    }

    pub fn modulo(&self, other: &Value) -> Result<Value> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs % rhs)),
            _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                format!("Cannot modulo {:?} and {:?}", self, other),
            ))),
        }
    }

    pub fn equal(&self, other: &Value) -> Result<Value> {
        Ok(Value::Bool(self == other))
    }

    pub fn not_equal(&self, other: &Value) -> Result<Value> {
        Ok(Value::Bool(self != other))
    }

    pub fn less_than(&self, other: &Value) -> Result<Value> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs < rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs < rhs)),
            _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                format!("Cannot compare {:?} and {:?}", self, other),
            ))),
        }
    }

    pub fn less_than_equals(&self, other: &Value) -> Result<Value> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs <= rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs <= rhs)),
            _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                format!("Cannot compare {:?} and {:?}", self, other),
            ))),
        }
    }

    pub fn greater_than(&self, other: &Value) -> Result<Value> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs > rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs > rhs)),
            _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                format!("Cannot compare {:?} and {:?}", self, other),
            ))),
        }
    }

    pub fn greater_than_equals(&self, other: &Value) -> Result<Value> {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs >= rhs)),
            (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs >= rhs)),
            _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                format!("Cannot compare {:?} and {:?}", self, other),
            ))),
        }
    }

    pub fn logical_and(&self, other: &Value) -> Result<Value> {
        match (self, other) {
            (Value::Bool(lhs), Value::Bool(rhs)) => Ok(Value::Bool(*lhs && *rhs)),
            _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                format!("Cannot logical and {:?} and {:?}", self, other),
            ))),
        }
    }

    pub fn logical_or(&self, other: &Value) -> Result<Value> {
        match (self, other) {
            (Value::Bool(lhs), Value::Bool(rhs)) => Ok(Value::Bool(*lhs || *rhs)),
            _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                format!("Cannot logical or {:?} and {:?}", self, other),
            ))),
        }
    }

    pub fn logical_not(&self) -> Result<Value> {
        match self {
            Value::Bool(b) => Ok(Value::Bool(!b)),
            _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                format!("Cannot logical not {:?}", self),
            ))),
        }
    }

    pub fn subscript(&self, index: &Value) -> Result<Value> {
        match (self, index) {
            (Value::List(list, ..), Value::Int(i)) => {
                if let Some(value) = list.get(*i as usize) {
                    Ok(value.clone())
                } else {
                    Err(Error::EvaluationError(EvaluationError::IndexOutOfRange))
                }
            }
            (Value::Object(obj, ..), Value::String(i)) => {
                if let Some(value) = obj.get(i) {
                    Ok(value.clone())
                } else {
                    Err(Error::EvaluationError(EvaluationError::ItemNotFound(
                        format!("Cannot subscript {:?} with {:?}", self, index),
                    )))
                }
            }
            _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                format!("Cannot subscript {:?} with {:?}", self, index),
            ))),
        }
    }

    pub fn field(&self, field_name: &str) -> Result<Value> {
        match self {
            Value::Object(map, ..) => {
                if let Some(value) = map.get(field_name) {
                    Ok(value.clone())
                } else {
                    Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                        format!("Field {:?} not found in {:?}", field_name, self),
                    )))
                }
            }
            _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                format!("Cannot access field {:?} in {:?}", field_name, self),
            ))),
        }
    }

    pub fn type_id(&self) -> TypeId {
        match self {
            Value::Bool(..) => TYPE_ID_BOOL,
            Value::Float(..) => TYPE_ID_FLOAT,
            Value::Int(..) => TYPE_ID_INT,
            Value::List(_, typ) => *typ,
            Value::Type(..) => TYPE_ID_TYPE,
            Value::Range(..) => TYPE_ID_RANGE,
            Value::Object(.., typ) => *typ,
            Value::String(..) => TYPE_ID_STRING,
            Value::Null => TYPE_ID_UNKNOWN,
            Value::Function(.., typ) => *typ,
            Value::Template(.., typ) => *typ,
        }
    }

    fn into_iter(&self) -> Result<ValueIterator> {
        match self {
            Value::List(list, ..) => Ok(ValueIterator::List(list.clone().into_iter())),
            Value::Range(start, end) => Ok(ValueIterator::Range(*start, *end)),
            _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                format!("Cannot iterate over {:?}", self),
            ))),
        }
    }

    fn as_int(&self) -> Result<i64> {
        match self {
            Value::Int(i) => Ok(*i),
            _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                format!("Cannot convert {:?} to int", self),
            ))),
        }
    }

    pub fn as_function(&self) -> Result<FunctionId> {
        match self {
            Value::Function(function_id, _) => Ok(*function_id),
            _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                format!("Cannot convert {:?} to function", self),
            ))),
        }
    }

    fn as_bool(&self) -> Result<bool> {
        match self {
            Value::Bool(b) => Ok(*b),
            _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                format!("Cannot convert {:?} to bool", self),
            ))),
        }
    }

    pub fn as_string(&self) -> Result<&String> {
        match self {
            Value::String(s) => Ok(s),
            _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                format!("Cannot convert {:?} to String", self),
            ))),
        }
    }

    pub fn as_list(&self) -> Result<&Vec<Value>> {
        match self {
            Value::List(list, ..) => Ok(list),
            _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                format!("Cannot convert {:?} to List", self),
            ))),
        }
    }

    pub fn as_type(&self) -> Result<TypeId> {
        match self {
            Value::Type(typ) => Ok(*typ),
            _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                format!("Cannot convert {:?} to type", self),
            ))),
        }
    }

    pub fn as_object(&self) -> Result<&HashMap<String, Value>> {
        match self {
            Value::Object(obj, ..) => Ok(obj),
            _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                format!("Cannot convert {:?} to object", self),
            ))),
        }
    }

    pub fn as_mut_object(&mut self) -> Result<&mut HashMap<String, Value>> {
        match self {
            Value::Object(obj, ..) => Ok(obj),
            _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                format!("Cannot convert {:?} to object", self),
            ))),
        }
    }

    pub fn is_null(&self) -> bool {
        matches!(self, Value::Null)
    }
}

pub fn initialize_stack(stack: &mut Stack, context: &Context) -> Result<()> {
    for konst in context.consts() {
        let value = evaluate_expression(context, stack, &konst.value)?;
        stack.push(1);
        stack.write(0, value)?;
    }

    Ok(())
}

pub fn call_function(
    context: &Context,
    module_id: ModuleId,
    function_name: &str,
    stack: &mut Stack,
    args: Vec<Value>,
) -> Result<Value> {
    let module = context.get_module(module_id);

    for function_id in &module.functions {
        let function = context.get_function(*function_id);
        if function.name != function_name {
            continue;
        }

        let arg_count = args.len();
        for arg in args.into_iter() {
            stack.push(1);
            stack.write(0, arg)?;
        }
        let val = evaluate_expression(context, stack, &function.expr)?;
        stack.pop(arg_count);
        return Ok(val);
    }

    Err(Error::EvaluationError(EvaluationError::FunctionNotFound(
        function_name.to_string(),
    )))
}

pub fn evaluate_list_element(
    context: &Context,
    stack: &mut Stack,
    list_element: &ListElement,
) -> Result<Vec<Value>> {
    let mut result = Vec::new();

    match list_element {
        ListElement::Expr(expr) => {
            let el = evaluate_expression(context, stack, expr)?;
            result.push(el);
        }
        ListElement::ForElement(for_element) => {
            let iterable = evaluate_expression(context, stack, &for_element.iterable)?;
            let iterator = iterable.into_iter()?;

            stack.push(1);

            for val in iterator {
                stack.write(0, val)?;
                for el in &for_element.body {
                    let mut el = evaluate_list_element(context, stack, &el)?;
                    result.append(&mut el);
                }
            }

            stack.pop(1);
        }
        ListElement::IfElement(if_element) => {
            let condition = evaluate_expression(context, stack, &if_element.condition)?;
            if condition.as_bool()? {
                for el in &if_element.body {
                    let mut el = evaluate_list_element(context, stack, &el)?;
                    result.append(&mut el);
                }
            }
        }
    }

    Ok(result)
}

pub fn evaluate_expression(
    context: &Context,
    stack: &mut Stack,
    expression: &Expression,
) -> Result<Value> {
    match expression {
        Expression::Amend {
            operand, values, ..
        } => {
            let mut operand = evaluate_expression(context, stack, &operand)?;
            // mutating to avoid an extra clone
            let obj = operand.as_mut_object()?;

            for (name, v) in values {
                obj.insert(name.clone(), evaluate_expression(context, stack, v)?);
            }

            Ok(operand)
        }
        Expression::BinaryOp { lhs, rhs, op, .. } => {
            let lhs = evaluate_expression(context, stack, lhs)?;

            if op == &Operator::TernaryCondition {
                let cond = lhs.as_bool()?;
                match &**rhs {
                    Expression::BinaryOp { lhs, rhs, .. } => {
                        if cond {
                            return evaluate_expression(context, stack, &*lhs);
                        } else {
                            return evaluate_expression(context, stack, &*rhs);
                        }
                    }
                    _ => panic!("Ternary condition must be followed by a choice"),
                }
            }

            let rhs = evaluate_expression(context, stack, rhs)?;

            match op {
                Operator::Add => lhs.add(&rhs),
                Operator::Divide => lhs.divide(&rhs),
                Operator::Equal => lhs.equal(&rhs),
                Operator::LessThan => lhs.less_than(&rhs),
                Operator::LessThanOrEqual => lhs.less_than_equals(&rhs),
                Operator::GreaterThan => lhs.greater_than(&rhs),
                Operator::GreaterThanOrEqual => lhs.greater_than_equals(&rhs),
                Operator::LogicalAnd => lhs.logical_and(&rhs),
                Operator::LogicalNot => lhs.logical_not(),
                Operator::LogicalOr => lhs.logical_or(&rhs),
                Operator::Multiply => lhs.multiply(&rhs),
                Operator::NotEqual => lhs.not_equal(&rhs),
                Operator::Subtract => lhs.subtract(&rhs),
                Operator::TypeTest => Ok(Value::Bool(lhs.type_id() == rhs.as_type()?)),
                Operator::TypeCast => {
                    let type_id = rhs.as_type()?;
                    rt_checked_typecast(context, &lhs, type_id)
                }
                Operator::NullCoalescing => {
                    if !lhs.is_null() {
                        Ok(lhs.clone())
                    } else {
                        Ok(rhs.clone())
                    }
                }
                Operator::TernaryCondition => {
                    panic!("Unexpected ternary condition, this case should be handled above")
                }
                Operator::TernaryChoice => {
                    panic!("Unexpected ternary choice, this case should be handled above")
                }
            }
        }
        Expression::UnaryOp { operand, op, .. } => {
            let operand = evaluate_expression(context, stack, operand)?;
            match op {
                Operator::LogicalNot => operand.logical_not(),
                Operator::Subtract => operand.negate(),
                _ => Err(Error::EvaluationError(EvaluationError::InvalidOperation(
                    format!("Cannot apply {:?} to {:?}", op, operand),
                ))),
            }
        }
        Expression::Value { value, .. } => Ok(value.clone()),
        Expression::StringInterpolation { parts, .. } => {
            let mut result = String::new();
            for part in parts {
                match part {
                    StringInterpolationPart::Text(text) => result.push_str(text),
                    StringInterpolationPart::Expression(expr) => {
                        let expr = evaluate_expression(context, stack, expr)?;
                        let expr = rt_checked_typecast(context, &expr, TYPE_ID_STRING)?;
                        match expr {
                            Value::String(s) => result.push_str(&s),
                            _ => panic!("String interpolation must be a string"),
                        }
                    }
                }
            }
            Ok(Value::String(result))
        }
        Expression::IndexedExpression { operand, index, .. } => {
            let operand = evaluate_expression(context, stack, operand)?;
            let index = evaluate_expression(context, stack, index)?;
            operand.subscript(&index)
        }
        Expression::Initializer { values, typ } => {
            let mut map = HashMap::<String, Value>::new();
            for (name, value) in values {
                let value = evaluate_expression(context, stack, value)?;
                map.insert(name.to_string(), value);
            }
            Ok(Value::Object(map, *typ))
        }
        Expression::IndexedStruct {
            operand,
            field_name,
            optional,
            ..
        } => {
            let operand = evaluate_expression(context, stack, operand)?;

            if *optional {
                if operand.is_null() {
                    return Ok(Value::Null);
                }
            }

            operand.field(&field_name.value)
        }
        Expression::ListLiteral {
            values: _values,
            typ,
        } => {
            let mut values = Vec::new();

            for val in _values {
                let mut el = evaluate_list_element(context, stack, val)?;
                values.append(&mut el);
            }

            Ok(Value::List(values, *typ))
        }
        Expression::OptionalNull { .. } => Ok(Value::Null),
        Expression::Range { start, end, .. } => {
            let start = evaluate_expression(context, stack, start)?;
            let end = evaluate_expression(context, stack, end)?;
            Ok(Value::Range(start.as_int()?, end.as_int()?))
        }
        Expression::Var { offset, .. } => {
            let val = stack.read_rel(*offset);
            Ok(val.clone())
        }
        Expression::Const { offset, .. } => Ok(stack.read_abs(*offset).clone()),
        Expression::TemplateInitializer {
            values, template, ..
        } => {
            let template = context.get_template(*template);
            let mut evaluated_values = Vec::new();

            // First evaluate, then push onto stack.
            for (_, expr) in values {
                let value = evaluate_expression(context, stack, expr)?;
                evaluated_values.push(value);
            }

            for value in evaluated_values {
                stack.push(1);
                stack.write(0, value)?;
            }

            let ret = evaluate_expression(context, stack, &template.expr)?;
            stack.pop(values.len());
            Ok(ret)
        }
        Expression::FunctionCall { function, args, .. } => {
            let value = evaluate_expression(context, stack, function)?;
            let function_id = value.as_function()?;

            let mut evaluated_args = Vec::new();
            for arg in args {
                let value = evaluate_expression(context, stack, arg)?;
                evaluated_args.push(value);
            }

            for value in evaluated_args {
                stack.push(1);
                stack.write(0, value)?;
            }

            let function = context.get_function(function_id);
            let ret = evaluate_expression(context, stack, &function.expr)?;
            stack.pop(args.len());
            Ok(ret)
        }
        Expression::BlockExpression { func, .. } => Ok(func(&stack, context.functions())),
        Expression::TernaryExpression {
            condition,
            left,
            right,
            ..
        } => {
            let condition = evaluate_expression(context, stack, condition)?;
            if condition.as_bool()? {
                evaluate_expression(context, stack, left)
            } else {
                evaluate_expression(context, stack, right)
            }
        }
        _ => {
            unimplemented!()
        }
    }
}

fn rt_checked_typecast(context: &Context, value: &Value, type_id: TypeId) -> Result<Value> {
    let target = context.get_type(type_id);

    match (value, target) {
        // Accept any type if target is Any
        (_, Type::Any) => Ok(value.clone()),

        // Direct type matches
        (Value::Int(_), Type::Int) => Ok(value.clone()),
        (Value::Float(_), Type::Float) => Ok(value.clone()),
        (Value::String(_), Type::String) => Ok(value.clone()),
        (Value::Bool(_), Type::Bool) => Ok(value.clone()),
        (Value::Range(a, b), Type::Range) => Ok(Value::Range(*a, *b)),
        (Value::Null, Type::Optional { .. }) => Ok(Value::Null),

        // Numeric conversions
        (Value::Int(i), Type::Float) => Ok(Value::Float(*i as f64)),
        (Value::Float(f), Type::Int) => {
            if f.trunc() == *f {
                Ok(Value::Int(*f as i64))
            } else {
                Err(Error::EvaluationError(EvaluationError::TypeCastError(
                    format!("Float {} cannot be cast to Int", f),
                )))
            }
        }

        (Value::Int(i), Type::String) => Ok(Value::String(i.to_string())),
        (Value::Float(f), Type::String) => Ok(Value::String(f.to_string())),

        // String parsing
        (Value::String(s), Type::Int) => s.parse().map(Value::Int).map_err(|_| {
            Error::EvaluationError(EvaluationError::TypeCastError(format!(
                "Failed to parse '{}' as Int",
                s
            )))
        }),
        (Value::String(s), Type::Float) => s.parse().map(Value::Float).map_err(|_| {
            Error::EvaluationError(EvaluationError::TypeCastError(format!(
                "Failed to parse '{}' as Float",
                s
            )))
        }),
        (Value::String(s), Type::Bool) => match s.to_lowercase().as_str() {
            "true" => Ok(Value::Bool(true)),
            "false" => Ok(Value::Bool(false)),
            _ => Err(Error::EvaluationError(EvaluationError::TypeCastError(
                format!("Invalid boolean string '{}'", s),
            ))),
        },

        // Bool to numeric
        (Value::Bool(true), Type::Int) => Ok(Value::Int(1)),
        (Value::Bool(false), Type::Int) => Ok(Value::Int(0)),
        (Value::Bool(true), Type::Float) => Ok(Value::Float(1.0)),
        (Value::Bool(false), Type::Float) => Ok(Value::Float(0.0)),

        // Bool/String conversions
        (Value::Bool(b), Type::String) => Ok(Value::String(b.to_string())),
        (Value::Int(i), Type::Bool) => Ok(Value::Bool(*i != 0)),
        (Value::Float(f), Type::Bool) => Ok(Value::Bool(*f != 0.0)),

        // List conversions
        (Value::List(elems, ..), Type::List { inner }) => {
            let mut casted = Vec::with_capacity(elems.len());
            for elem in elems {
                casted.push(rt_checked_typecast(context, elem, *inner)?);
            }
            Ok(Value::List(casted, *inner))
        }

        // Object to Struct
        (Value::Object(obj, ..), Type::Struct(strukt)) => {
            let mut casted = HashMap::new();
            // Check required fields
            for (field, type_id, _) in strukt.fields.iter() {
                let value = obj.get(field).ok_or(Error::EvaluationError(
                    EvaluationError::TypeCastError(format!("Missing required field '{}'", field)),
                ))?;
                casted.insert(
                    field.clone(),
                    rt_checked_typecast(context, value, *type_id)?,
                );
            }

            Ok(Value::Object(casted, type_id))
        }

        // Object to Map
        (Value::Object(obj, ..), Type::Map { key, value }) => {
            if *key != TYPE_ID_STRING {
                return Err(Error::EvaluationError(EvaluationError::TypeCastError(
                    "Map key must be String".into(),
                )));
            }
            let mut casted = HashMap::new();
            for (k, v) in obj {
                casted.insert(k.clone(), rt_checked_typecast(context, v, *value)?);
            }
            Ok(Value::Object(casted, type_id))
        }

        // Optional handling (non-null case)
        (v, Type::Optional { inner }) => {
            let casted = rt_checked_typecast(context, v, *inner)?;
            Ok(casted)
        }

        // Oneof type handling
        (v, Type::Oneof(oneof)) => {
            for type_id in &oneof.types {
                if let Ok(result) = rt_checked_typecast(context, v, *type_id) {
                    return Ok(result);
                }
            }
            Err(Error::EvaluationError(EvaluationError::TypeCastError(
                "No compatible type in Oneof".into(),
            )))
        }

        // Handle errors for unsupported casts
        _ => Err(Error::EvaluationError(EvaluationError::TypeCastError(
            "Invalid cast".to_string(),
        ))),
    }
}
