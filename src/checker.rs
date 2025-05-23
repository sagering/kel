use crate::common::{Error, Result, Span};
use crate::lexer::NumericConstant;
use crate::parser::{
    Operator, ParsedConst, ParsedExpression, ParsedForElement, ParsedFromImport, ParsedFunction,
    ParsedImport, ParsedListElement, ParsedModule, ParsedName, ParsedOneof, ParsedOperator,
    ParsedStatement, ParsedStringInterpolationPart, ParsedStruct, ParsedTemplate,
};
use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

macro_rules! error {
    ($msg:expr, $span:expr) => {
        Error::CheckerError(format!($msg), $span)
    };
    ($msg:expr, $arg1:expr, $span:expr) => {
        Error::CheckerError(format!($msg, $arg1), $span)
    };
    ($msg:expr, $arg1:expr, $arg2:expr, $span:expr) => {
        Error::CheckerError(format!($msg, $arg1, $arg2), $span)
    };
    ($msg:expr, $arg1:expr, $arg2:expr, $arg3:expr, $span:expr) => {
        Error::CheckerError(format!($msg, $arg1, $arg2, $arg3), $span)
    };
    ($msg:expr, $arg1:expr, $arg2:expr, $arg3:expr, $arg4:expr, $span:expr) => {
        Error::CheckerError(format!($msg, $arg1, $arg2, $arg3, $arg4), $span)
    };
}

pub struct Struct {
    pub module_id: ModuleId,
    pub name: String,
    pub fields: Vec<(String, TypeId, Option<Expression>)>,
}

pub struct Template {
    pub name: String,
    pub pars: Vec<(String, TypeId)>,
    pub expr: Expression,
    pub type_id: TypeId,
}

pub struct Function {
    pub name: String,
    pub pars: Vec<(String, TypeId)>,
    pub return_type: TypeId,
    pub expr: Expression,
    pub type_id: TypeId,
}

#[derive(PartialEq, Debug, Clone, Eq, Hash)]
pub struct Oneof {
    pub module_id: ModuleId,
    pub name: String,
    pub types: Vec<TypeId>,
}

pub struct Const {
    pub name: String,
    pub typ: TypeId,
    pub value: Expression,
}

pub type FunctionId = usize;
pub type TemplateId = usize;
pub type ConstId = usize;
pub type StructId = usize;
pub type OneofId = usize;
pub type TypeId = usize;
pub type ModuleId = usize;

pub const TYPE_ID_UNKNOWN: TypeId = 0;
pub const TYPE_ID_BOOL: TypeId = 1;
pub const TYPE_ID_INT: TypeId = 2;
pub const TYPE_ID_FLOAT: TypeId = 3;
pub const TYPE_ID_STRING: TypeId = 4;
pub const TYPE_ID_RANGE: TypeId = 5;
pub const TYPE_ID_ANY: TypeId = 6;
pub const TYPE_ID_TYPE: TypeId = 7;
pub const TYPE_ID_NULL: TypeId = 8;

pub enum TypeVarConstraint {
    Union { types: Vec<TypeId> },
}

impl TypeVarConstraint {
    pub fn new_union(types: Vec<TypeId>) -> Self {
        TypeVarConstraint::Union { types }
    }

    pub fn satisfied_by(&self, context: &Context, type_id: TypeId) -> Option<TypeId> {
        match self {
            TypeVarConstraint::Union { types } => {
                // Check for direct match
                if types.contains(&type_id) {
                    return Some(type_id);
                }

                // Check for subtypes
                for t in types {
                    if type_includes(context, *t, type_id) {
                        return Some(*t);
                    }
                }

                None
            }
        }
    }

    pub fn display(&self, context: &Context) -> String {
        match self {
            TypeVarConstraint::Union { types } => {
                let mut result = String::from("Union[");
                for (i, t) in types.iter().enumerate() {
                    if i > 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&context.get_type(*t).fully_qualified_name(context));
                }
                result.push(']');
                result
            }
        }
    }
}

pub enum Type {
    Any,
    Bool,
    Function {
        pars: Vec<TypeId>,
        return_type: TypeId,
    },
    Float,
    Int,
    List {
        inner: TypeId,
    },
    Map {
        key: TypeId,
        value: TypeId,
    },
    Null,
    Oneof(Oneof),
    Optional {
        inner: TypeId,
    },
    Range,
    String,
    StringLiteral {
        value: String,
    },
    Struct(Struct),
    Template {
        pars: Vec<TypeId>,
        return_type: TypeId,
    },
    Type,
    TypeVar {
        name: String,
        constraint: Option<TypeVarConstraint>,
    },
    Unknown,
}

impl Type {
    pub fn fully_qualified_name(&self, context: &Context) -> String {
        match self {
            Type::Null => "Null".to_string(),
            Type::Type => "Type".to_string(),
            Type::Any => "Any".to_string(),
            Type::Bool => "Bool".to_string(),
            Type::Int => "Int".to_string(),
            Type::Float => "Float".to_string(),
            Type::String => "String".to_string(),
            Type::StringLiteral { value } => value.clone(),
            Type::List { inner } => format!(
                "List[{}]",
                context.get_type(*inner).fully_qualified_name(context)
            ),
            Type::Map { key, value } => format!(
                "Map[{}, {}]",
                context.get_type(*key).fully_qualified_name(context),
                context.get_type(*value).fully_qualified_name(context)
            ),
            Type::Struct(s) => format!("{}.{}", s.module_id, s.name),
            Type::Optional { inner } => format!(
                "Optional[{}]",
                context.get_type(*inner).fully_qualified_name(context)
            ),
            Type::Unknown => "Unknown".to_string(),
            Type::Range => "Range".to_string(),
            Type::Oneof(o) => {
                format!("{}.{}", o.module_id, o.name)
            }
            Type::TypeVar { name, .. } => name.clone(),
            Type::Function { pars, return_type } => format!(
                "Function[{} -> {}]",
                pars.iter()
                    .map(|t| context.get_type(*t).fully_qualified_name(context))
                    .collect::<Vec<_>>()
                    .join(", "),
                context.get_type(*return_type).fully_qualified_name(context)
            ),
            Type::Template { pars, return_type } => format!(
                "Template[{} -> {}]",
                pars.iter()
                    .map(|t| context.get_type(*t).fully_qualified_name(context))
                    .collect::<Vec<_>>()
                    .join(", "),
                context.get_type(*return_type).fully_qualified_name(context)
            ),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    List(Vec<Value>, TypeId),
    Object(HashMap<String, Value>, TypeId),
    Range(i64, i64),
    Type(TypeId),
    Template(TemplateId, TypeId),
    Null,
    Function(FunctionId, TypeId),
}

/// The context is more like a allocator for the compiler.
pub struct Context {
    templates: Vec<Template>,
    functions: Vec<Function>,
    types: Vec<Type>,
    consts: Vec<Const>,
    // This type_map is used to deduplicate types, e.g. List[String] in one module
    // and List[String] in another module should map to the same TypeId.
    // Any type will have a fully qualified name such as 'List[String]' and deduplication will be done on that.
    type_map: HashMap<String, TypeId>,
    modules: Vec<Module>,
}

impl Context {
    pub fn functions(&self) -> &[Function] {
        &self.functions
    }

    pub fn new() -> Context {
        let types = vec![
            Type::Unknown,
            Type::Bool,
            Type::Int,
            Type::Float,
            Type::String,
            Type::Range,
            Type::Any,
            Type::Type,
        ];
        let mut type_map: HashMap<String, TypeId> = HashMap::new();

        type_map.insert("Unknown".to_string(), TYPE_ID_UNKNOWN);
        type_map.insert("Bool".to_string(), TYPE_ID_BOOL);
        type_map.insert("Int".to_string(), TYPE_ID_INT);
        type_map.insert("Float".to_string(), TYPE_ID_FLOAT);
        type_map.insert("String".to_string(), TYPE_ID_STRING);
        type_map.insert("Range".to_string(), TYPE_ID_RANGE);
        type_map.insert("Any".to_string(), TYPE_ID_ANY);
        type_map.insert("Type".to_string(), TYPE_ID_TYPE);
        type_map.insert("Null".to_string(), TYPE_ID_NULL);

        Context {
            types,
            templates: Vec::new(),
            functions: Vec::new(),
            consts: Vec::new(),
            type_map,
            modules: Vec::new(),
        }
    }

    pub fn consts(&self) -> &Vec<Const> {
        &self.consts
    }

    pub fn add_const(&mut self, konst: Const) -> ConstId {
        let const_id = self.consts.len();
        self.consts.push(konst);
        const_id
    }

    pub fn get_const(&self, const_id: ConstId) -> &Const {
        &self.consts[const_id]
    }

    pub fn add_template(&mut self, template: Template) -> TemplateId {
        let template_id = self.templates.len();
        self.templates.push(template);
        template_id
    }

    pub fn add_function(&mut self, function: Function) -> FunctionId {
        let function_id = self.functions.len();
        self.functions.push(function);
        function_id
    }

    pub fn add_type(&mut self, typ: Type) -> TypeId {
        let key = typ.fully_qualified_name(self);
        if self.type_map.contains_key(&key) {
            return self.type_map[&key];
        }
        let type_id = self.types.len();
        self.type_map.insert(key, type_id);
        self.types.push(typ);
        type_id
    }

    pub fn add_module(&mut self, module: Module) -> ModuleId {
        let module_id = self.modules.len();
        self.modules.push(module);
        module_id
    }

    pub fn get_module(&self, module_id: ModuleId) -> &Module {
        &self.modules[module_id]
    }

    pub fn get_template(&self, template_id: TemplateId) -> &Template {
        &self.templates[template_id]
    }

    pub fn get_function(&self, function_id: FunctionId) -> &Function {
        &self.functions[function_id]
    }

    pub fn set_template(&mut self, template_id: TemplateId, template: Template) {
        self.templates[template_id] = template;
    }

    pub fn set_function(&mut self, function_id: FunctionId, function: Function) {
        self.functions[function_id] = function;
    }

    pub fn get_type(&self, type_id: TypeId) -> &Type {
        &self.types[type_id]
    }

    pub fn set_type(&mut self, type_id: TypeId, typ: Type) {
        self.types[type_id] = typ;
    }

    fn find_module(&self, identifier: &str) -> Option<ModuleId> {
        for (id, module) in self.modules.iter().enumerate() {
            if module.identifier == identifier {
                return Some(id);
            }
        }
        None
    }
}

enum OwnedOrRef<'a, T> {
    Owned(T),
    Ref(&'a mut T),
}

impl<'a, T> Deref for OwnedOrRef<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            OwnedOrRef::Owned(ref val) => val,
            OwnedOrRef::Ref(ref val) => &**val,
        }
    }
}

impl<'a, T> DerefMut for OwnedOrRef<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            OwnedOrRef::Owned(ref mut val) => val,
            OwnedOrRef::Ref(ref mut val) => *val,
        }
    }
}

pub struct Scope<'a> {
    variables_offset: usize,
    types_offset: usize,
    templates_offset: usize,
    consts_offset: usize,
    variables: OwnedOrRef<'a, Vec<(String, TypeId)>>,
    types: OwnedOrRef<'a, Vec<TypeId>>,
    templates: OwnedOrRef<'a, Vec<TemplateId>>,
    functions: OwnedOrRef<'a, Vec<FunctionId>>,
    consts: OwnedOrRef<'a, Vec<ConstId>>,
    modules: OwnedOrRef<'a, Vec<ModuleId>>,
}

impl<'a> Scope<'a> {
    pub fn new() -> Scope<'a> {
        Scope {
            variables_offset: 0,
            types_offset: 0,
            templates_offset: 0,
            consts_offset: 0,
            variables: OwnedOrRef::Owned(Vec::new()),
            types: OwnedOrRef::Owned(Vec::new()),
            templates: OwnedOrRef::Owned(Vec::new()),
            functions: OwnedOrRef::Owned(Vec::new()),
            consts: OwnedOrRef::Owned(Vec::new()),
            modules: OwnedOrRef::Owned(Vec::new()),
        }
    }

    pub fn child(&mut self) -> Scope {
        Scope {
            variables_offset: self.variables.len(),
            types_offset: self.types.len(),
            templates_offset: self.templates.len(),
            consts_offset: self.consts.len(),
            variables: OwnedOrRef::Ref(&mut self.variables),
            templates: OwnedOrRef::Ref(&mut self.templates),
            functions: OwnedOrRef::Ref(&mut self.functions),
            types: OwnedOrRef::Ref(&mut self.types),
            consts: OwnedOrRef::Ref(&mut self.consts),
            modules: OwnedOrRef::Ref(&mut self.modules),
        }
    }

    pub fn add_variable(&mut self, name: String, type_id: TypeId) {
        self.variables.push((name, type_id));
    }

    pub fn add_type(&mut self, type_id: TypeId) {
        self.types.push(type_id);
    }

    pub fn add_template(&mut self, template_id: TemplateId) {
        self.templates.push(template_id);
    }

    pub fn add_function(&mut self, function_id: FunctionId) {
        self.functions.push(function_id);
    }

    pub fn add_const(&mut self, const_id: ConstId) {
        self.consts.push(const_id);
    }

    pub fn add_module(&mut self, module_id: ModuleId) {
        self.modules.push(module_id);
    }

    pub fn find_const(&self, name: &str, context: &Context) -> Option<ConstId> {
        for const_id in self.consts.iter().rev() {
            let konst = context.get_const(*const_id);
            if konst.name == name {
                return Some(*const_id);
            }
        }
        None
    }

    pub fn find_template(&self, name: &str, context: &Context) -> Option<TemplateId> {
        for template_id in self.templates.iter().rev() {
            let template = context.get_template(*template_id);
            if template.name == name {
                return Some(*template_id);
            }
        }
        None
    }

    pub fn find_function(&self, name: &str, context: &Context) -> Option<FunctionId> {
        for function_id in self.functions.iter().rev() {
            let function = context.get_function(*function_id);
            if function.name == name {
                return Some(*function_id);
            }
        }
        None
    }

    pub fn find_type(&self, name: &str, context: &Context) -> Option<TypeId> {
        match name {
            "Any" => return Some(TYPE_ID_ANY),
            "Bool" => return Some(TYPE_ID_BOOL),
            "Int" => return Some(TYPE_ID_INT),
            "Float" => return Some(TYPE_ID_FLOAT),
            "String" => return Some(TYPE_ID_STRING),
            _ => {}
        }

        for type_id in self.types.iter().rev() {
            let typ = context.get_type(*type_id);
            match typ {
                Type::Struct(_struct) if _struct.name == name => {
                    return Some(*type_id);
                }
                Type::Oneof(oneof) if oneof.name == name => {
                    return Some(*type_id);
                }
                _ => {}
            }
        }
        None
    }

    pub fn find_variable(&self, name: &str) -> Option<(usize, TypeId)> {
        let mut offset = 0;
        for (var_name, type_id) in self.variables.iter().rev() {
            if var_name == name {
                return Some((offset, *type_id));
            }
            offset += 1;
        }
        None
    }

    pub fn find_module(&self, name: &str, context: &Context) -> Option<ModuleId> {
        for module_id in self.modules.iter().rev() {
            let module = context.get_module(*module_id);
            if module.identifier == name {
                return Some(*module_id);
            }
        }
        None
    }

    pub fn is_name_defined(&self, name: &str, context: &Context) -> bool {
        if let Some(..) = self.find_const(name, context) {
            return true;
        }
        if let Some(..) = self.find_template(name, context) {
            return true;
        }
        if let Some(..) = self.find_function(name, context) {
            return true;
        }
        if let Some(..) = self.find_type(name, context) {
            return true;
        }
        if let Some(..) = self.find_variable(name) {
            return true;
        }
        return false;
    }
}

impl Drop for Scope<'_> {
    fn drop(&mut self) {
        self.variables.truncate(self.variables_offset);
        self.types.truncate(self.types_offset);
        self.templates.truncate(self.templates_offset);
        self.consts.truncate(self.consts_offset);
    }
}

pub struct Stack {
    values: Vec<Value>,
}

impl Stack {
    pub fn new() -> Self {
        Stack { values: Vec::new() }
    }
    pub fn push(&mut self, count: usize) {
        for _ in 0..count {
            self.values.push(Value::Null);
        }
    }
    pub fn pop(&mut self, count: usize) {
        self.values.truncate(self.values.len() - count);
    }
    pub fn write(&mut self, offset: usize, value: Value) -> Result<()> {
        let index = self.values.len() - 1 - offset;
        self.values[index] = value;
        Ok(())
    }
    pub fn read_rel(&self, offset: usize) -> &Value {
        let index = self.values.len() - 1 - offset;
        &self.values[index]
    }
    pub fn read_abs(&self, offset: usize) -> &Value {
        &self.values[offset]
    }
}

#[derive(Clone)]
pub enum Expression {
    Value {
        value: Value,
        typ: TypeId,
    },
    StringInterpolation {
        parts: Vec<StringInterpolationPart>,
        typ: TypeId,
    },
    ListLiteral {
        values: Vec<ListElement>,
        typ: TypeId,
    },
    Map {
        entries: Vec<(Expression, Expression)>,
        typ: TypeId,
    },
    IndexedExpression {
        operand: Box<Expression>,
        index: Box<Expression>,
        typ: TypeId,
    },
    UnaryOp {
        operand: Box<Expression>,
        op: Operator,
        typ: TypeId,
    },
    BinaryOp {
        lhs: Box<Expression>,
        op: Operator,
        rhs: Box<Expression>,
        typ: TypeId,
    },
    Initializer {
        values: Vec<(String, Expression)>,
        typ: TypeId,
    },
    TemplateInitializer {
        template: TemplateId,
        values: Vec<(String, Expression)>,
        typ: TypeId,
    },
    FunctionCall {
        function: Box<Expression>,
        args: Vec<Expression>,
        return_type: TypeId,
    },
    Amend {
        operand: Box<Expression>,
        values: Vec<(String, Expression)>,
        typ: TypeId,
    },
    IndexedStruct {
        operand: Box<Expression>,
        field_name: ParsedName,
        optional: bool,
        typ: TypeId,
    },
    Const {
        offset: usize,
        typ: TypeId,
    },
    Var {
        offset: usize,
        typ: TypeId,
    },
    OptionalNull {
        typ: TypeId,
    },
    Range {
        start: Box<Expression>,
        end: Box<Expression>,
        typ: TypeId,
    },
    BlockExpression {
        func: Rc<dyn Fn(&Stack, &[Function]) -> Value>,
        typ: TypeId,
    },
    TernaryExpression {
        condition: Box<Expression>,
        left: Box<Expression>,
        right: Box<Expression>,
        typ: TypeId,
    },
    Garbage,
}

#[derive(Clone)]
pub enum StringInterpolationPart {
    Text(String),
    Expression(Box<Expression>),
}

impl Expression {
    /// Get the type of this expression
    pub fn type_id(&self) -> TypeId {
        match self {
            Expression::FunctionCall { return_type, .. } => *return_type,
            Expression::Amend { typ, .. } => *typ,
            Expression::Value { typ, .. } => *typ,
            Expression::ListLiteral { typ, .. } => *typ,
            Expression::Map { typ, .. } => *typ,
            Expression::IndexedExpression { typ, .. } => *typ,
            Expression::UnaryOp { typ, .. } => *typ,
            Expression::BinaryOp { typ, .. } => *typ,
            Expression::Initializer { typ, .. } => *typ,
            Expression::TemplateInitializer { typ, .. } => *typ,
            Expression::IndexedStruct { typ, .. } => *typ,
            Expression::Const { typ, .. } => *typ,
            Expression::Var { typ, .. } => *typ,
            Expression::OptionalNull { typ } => *typ,
            Expression::Range { typ, .. } => *typ,
            Expression::Garbage => TYPE_ID_UNKNOWN,
            Expression::StringInterpolation { typ, .. } => *typ,
            Expression::TernaryExpression { typ, .. } => *typ,
            Expression::BlockExpression { typ, .. } => *typ,
        }
    }

    pub fn value(&self) -> &Value {
        match self {
            Expression::Value { value, .. } => value,
            _ => panic!("Not a value"),
        }
    }
}

pub enum Statement {
    Module(String),
    Import(String),
    Const(Const),
    Struct(Struct),
    Template(Template),
    Oneof(Oneof),
    Expression(Expression),
}

#[derive(Clone)]
pub struct ForElement {
    pub var: String,
    pub iterable: Expression,
    pub body: Vec<ListElement>,
}

#[derive(Clone)]
pub struct IfElement {
    pub condition: Expression,
    pub body: Vec<ListElement>,
}

#[derive(Clone)]
pub enum ListElement {
    Expr(Expression),
    ForElement(ForElement),
    IfElement(IfElement),
}

impl ListElement {
    fn typ(&self) -> TypeId {
        match self {
            ListElement::Expr(expr) => expr.type_id(),
            ListElement::ForElement(for_element) => for_element.body[0].typ(),
            ListElement::IfElement(for_element) => for_element.body[0].typ(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    pub identifier: String,
    pub consts: Vec<ConstId>,
    pub types: Vec<TypeId>,
    pub templates: Vec<TemplateId>,
    pub functions: Vec<FunctionId>,
}

impl Module {
    fn find_const(&self, name: &str, context: &Context) -> Option<ConstId> {
        for const_id in self.consts.iter().rev() {
            let konst = context.get_const(*const_id);
            if konst.name == name {
                return Some(*const_id);
            }
        }
        None
    }

    fn find_template(&self, name: &str, context: &Context) -> Option<TemplateId> {
        for template_id in self.templates.iter().rev() {
            let template = context.get_template(*template_id);
            if template.name == name {
                return Some(*template_id);
            }
        }
        None
    }

    fn find_function(&self, name: &str, context: &Context) -> Option<FunctionId> {
        for function_id in self.functions.iter().rev() {
            let function = context.get_function(*function_id);
            if function.name == name {
                return Some(*function_id);
            }
        }
        None
    }

    fn find_type(&self, name: &str, context: &Context) -> Option<TypeId> {
        for type_id in self.types.iter().rev() {
            let typ = context.get_type(*type_id);
            match typ {
                Type::Struct(_struct) if _struct.name == name => {
                    return Some(*type_id);
                }
                Type::Oneof(oneof) if oneof.name == name => {
                    return Some(*type_id);
                }
                _ => {}
            }
        }
        None
    }
}

fn check_binop(
    context: &mut Context,
    scope: &mut Scope,
    lhs: ParsedExpression,
    rhs: ParsedExpression,
    op: ParsedOperator,
    _: Span,
) -> Result<Expression> {
    let lhs_span = lhs.span();

    match op.operator {
        Operator::Add | Operator::Subtract | Operator::Multiply | Operator::Divide => {
            let lhs = check_expression(context, scope, lhs)?;
            let rhs = check_expression(context, scope, rhs)?;
            let lhs_type = lhs.type_id();
            if lhs.type_id() != TYPE_ID_INT && lhs.type_id() != TYPE_ID_FLOAT {
                return Err(error!(
                    "Type mismatch, expected Int of Float, got {:?}",
                    lhs.type_id(),
                    lhs_span
                ));
            }
            Ok(Expression::BinaryOp {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op: op.operator,
                typ: lhs_type,
            })
        }
        Operator::Equal | Operator::NotEqual => {
            let lhs = check_expression(context, scope, lhs)?;
            let rhs = check_expression(context, scope, rhs)?;
            check_types(context, lhs.type_id(), rhs.type_id(), lhs_span)?;
            Ok(Expression::BinaryOp {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op: op.operator,
                typ: TYPE_ID_BOOL,
            })
        }
        Operator::LessThan
        | Operator::LessThanOrEqual
        | Operator::GreaterThan
        | Operator::GreaterThanOrEqual => {
            let lhs = check_expression(context, scope, lhs)?;
            let rhs = check_expression(context, scope, rhs)?;
            if lhs.type_id() != TYPE_ID_INT && lhs.type_id() != TYPE_ID_FLOAT {
                return Err(error!(
                    "Type mismatch, expected Int of Float, got {:?}",
                    lhs.type_id(),
                    lhs_span
                ));
            }
            Ok(Expression::BinaryOp {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op: op.operator,
                typ: TYPE_ID_BOOL,
            })
        }
        Operator::LogicalAnd | Operator::LogicalOr => {
            let lhs = check_expression(context, scope, lhs)?;
            let rhs = check_expression(context, scope, rhs)?;
            if lhs.type_id() != TYPE_ID_BOOL {
                return Err(error!(
                    "Type mismatch, expected Bool, got {:?}",
                    lhs.type_id(),
                    lhs_span
                ));
            }
            Ok(Expression::BinaryOp {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                op: op.operator,
                typ: TYPE_ID_BOOL,
            })
        }
        Operator::TypeCast => {
            let lhs = check_expression(context, scope, lhs)?;
            let rhs = check_type(context, scope, rhs)?;
            Ok(Expression::BinaryOp {
                lhs: Box::new(lhs),
                rhs: Box::new(Expression::Value {
                    value: Value::Type(rhs),
                    typ: TYPE_ID_TYPE,
                }),
                op: op.operator,
                typ: rhs,
            })
        }
        Operator::TypeTest => {
            let lhs = check_expression(context, scope, lhs)?;
            let rhs = check_type(context, scope, rhs)?;
            Ok(Expression::BinaryOp {
                lhs: Box::new(lhs),
                rhs: Box::new(Expression::Value {
                    value: Value::Type(rhs),
                    typ: TYPE_ID_TYPE,
                }),
                op: op.operator,
                typ: TYPE_ID_BOOL,
            })
        }
        Operator::NullCoalescing => {
            let lhs = check_expression(context, scope, lhs)?;
            let typ = context.get_type(lhs.type_id());
            let inner = match typ {
                Type::Optional { inner } => *inner,
                _ => {
                    return Err(error!(
                        "Type mismatch, expected Optional or Null, got {:?}",
                        lhs.type_id(),
                        lhs_span
                    ));
                }
            };

            let rhs_span = rhs.span();
            let rhs = check_expression(context, scope, rhs)?;

            if lhs.type_id() == rhs.type_id() {
                return Ok(Expression::BinaryOp {
                    lhs: Box::new(lhs),
                    typ: rhs.type_id(),
                    rhs: Box::new(rhs),
                    op: op.operator,
                });
            }

            check_types(context, inner, rhs.type_id(), rhs_span)?;

            Ok(Expression::BinaryOp {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                typ: inner,
                op: op.operator,
            })
        }
        Operator::TernaryCondition => {
            let lhs = check_expression(context, scope, lhs)?;
            let rhs_span = rhs.span();
            let rhs = check_expression(context, scope, rhs)?;
            if lhs.type_id() != TYPE_ID_BOOL {
                return Err(error!(
                    "Type mismatch, expected Bool, got {:?}",
                    lhs.type_id(),
                    lhs_span
                ));
            }
            let rhs_typ = rhs.type_id();

            // Check if rhs is ternary choice binop.
            if let Expression::BinaryOp { op, .. } = &rhs {
                if op == &Operator::TernaryChoice {
                    return Ok(Expression::BinaryOp {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                        typ: rhs_typ,
                        op: Operator::TernaryCondition,
                    });
                }
            }

            Err(error!("Expected TernaryChoice", rhs_span))
        }
        Operator::TernaryChoice => {
            let lhs = check_expression(context, scope, lhs)?;
            let rhs = check_expression(context, scope, rhs)?;

            let type_id = if type_includes(context, lhs.type_id(), rhs.type_id()) {
                lhs.type_id()
            } else if type_includes(context, rhs.type_id(), lhs.type_id()) {
                rhs.type_id()
            } else {
                let types = vec![lhs.type_id(), rhs.type_id()];
                // TODO: proper name and module_id
                let oneof = Type::Oneof(Oneof {
                    module_id: lhs_span.file_id,
                    name: format!("__AnonymousOneof__"),
                    types,
                });
                context.add_type(oneof)
            };

            Ok(Expression::BinaryOp {
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                typ: type_id,
                op: op.operator,
            })
        }
        _ => unimplemented!(),
    }
}

fn check_unop(
    context: &mut Context,
    scope: &mut Scope,
    operand: ParsedExpression,
    op: ParsedOperator,
    _: Span,
) -> Result<Expression> {
    let operand_span = operand.span();
    let operand = check_expression(context, scope, operand)?;

    let typ = operand.type_id();

    match op.operator {
        Operator::LogicalNot => {
            if operand.type_id() != TYPE_ID_BOOL {
                return Err(error!(
                    "Type mismatch, expected Bool, got {:?}",
                    operand.type_id(),
                    operand_span
                ));
            }
            Ok(Expression::UnaryOp {
                operand: Box::new(operand),
                op: op.operator,
                typ: TYPE_ID_BOOL,
            })
        }
        Operator::Subtract => {
            if operand.type_id() != TYPE_ID_INT && operand.type_id() != TYPE_ID_FLOAT {
                return Err(error!(
                    "Type mismatch, expected Int or Float, got {:?}",
                    operand.type_id(),
                    operand_span
                ));
            }
            Ok(Expression::UnaryOp {
                operand: Box::new(operand),
                op: op.operator,
                typ,
            })
        }
        Operator::Add => {
            if operand.type_id() != TYPE_ID_INT && operand.type_id() != TYPE_ID_FLOAT {
                return Err(error!(
                    "Type mismatch, expected Int or Float, got {:?}",
                    operand.type_id(),
                    operand_span
                ));
            }
            Ok(Expression::UnaryOp {
                operand: Box::new(operand),
                op: op.operator,
                typ,
            })
        }
        _ => unimplemented!(),
    }
}

fn check_subscript_expr(
    context: &mut Context,
    scope: &mut Scope,
    operand: ParsedExpression,
    subscripts: Vec<ParsedExpression>,
    span: Span,
) -> Result<Expression> {
    let operand = check_expression(context, scope, operand)?;

    if subscripts.len() != 1 {
        return Err(error!(
            "Subscript expression must have exactly one index",
            span
        ));
    }

    let index = subscripts.into_iter().next().unwrap();
    let index_span = index.span();
    let index = check_expression(context, scope, index)?;
    let operand_type = context.get_type(operand.type_id());

    match operand_type {
        Type::List { inner } => {
            if index.type_id() != TYPE_ID_INT {
                return Err(error!(
                    "Index must be an Int, got {:?}",
                    index.type_id(),
                    index_span
                ));
            }
            Ok(Expression::IndexedExpression {
                operand: Box::new(operand),
                index: Box::new(index),
                typ: *inner,
            })
        }
        Type::Map { key, value } => {
            check_types(context, *key, index.type_id(), index_span)?;
            Ok(Expression::IndexedExpression {
                operand: Box::new(operand),
                index: Box::new(index),
                typ: *value,
            })
        }
        _ => Err(error!(
            "Indexed expression must be a list or a map",
            index_span
        )),
    }
}

fn check_dot_expr(
    context: &mut Context,
    scope: &mut Scope,
    operand: ParsedExpression,
    name: ParsedName,
    optional: bool,
    span: Span,
) -> Result<Expression> {
    match &operand {
        ParsedExpression::Name(operand_name) => {
            if let Some(module_id) = scope.find_module(&operand_name.value, context) {
                if optional {
                    return Err(error!("Module name cannot be optional", operand_name.span));
                }
                let module = context.get_module(module_id);

                if let Some(const_id) = module.find_const(&name.value, context) {
                    return Ok(Expression::Const {
                        offset: const_id,
                        typ: context.get_const(const_id).typ,
                    });
                } else if let Some(function_id) = module.find_function(&name.value, context) {
                    let typ = context.get_function(function_id).type_id;
                    return Ok(Expression::Value {
                        value: Value::Function(function_id, typ),
                        typ,
                    });
                } else if let Some(type_id) = module.find_type(&name.value, context) {
                    return Ok(Expression::Value {
                        value: Value::Type(type_id),
                        typ: TYPE_ID_TYPE,
                    });
                } else if let Some(_) = module.find_template(&name.value, context) {
                    return Err(error!("Not supported", span));
                } else {
                    return Err(error!(
                        "Module '{}' has no symbol '{}'",
                        module.identifier, name.value, name.span
                    ));
                }
            }
        }
        _ => {}
    };

    let operand = check_expression(context, scope, operand)?;

    let mut strukt_type = context.get_type(operand.type_id());

    if optional {
        if let Type::Optional { inner } = strukt_type {
            strukt_type = context.get_type(*inner);
        } else {
            return Err(error!(
                "Indexed expression must be an optional struct",
                span
            ));
        }
    }

    let mut field_type = match strukt_type {
        Type::Struct(st) => {
            st.fields
                .iter()
                .find(|(field_name, _, _)| *field_name == name.value)
                .ok_or_else(|| {
                    error!(
                        "Field '{}' not found in struct '{}'",
                        name.value, st.name, name.span
                    )
                })?
                .1
        }
        _ => return Err(error!("Indexed expression must be a struct", span)),
    };

    if optional {
        field_type = context.add_type(Type::Optional { inner: field_type })
    }

    Ok(Expression::IndexedStruct {
        operand: Box::new(operand),
        field_name: name,
        typ: field_type,
        optional,
    })
}

fn check_initializer(
    scope: &mut Scope,
    context: &mut Context,
    operand: ParsedExpression,
    values: Vec<(ParsedName, ParsedExpression)>,
    span: Span,
) -> Result<Expression> {
    let checked_operand = check_expression(context, scope, operand)?;
    match checked_operand {
        Expression::Value { value, .. } => match value {
            Value::Type(type_id) => check_struct_initializer(context, scope, type_id, values, span),
            Value::Template(template_id, _) => {
                check_template_initializer(scope, context, template_id, values, span)
            }
            // TOOD: Var
            _ => return Err(error!("Initializer must be used with a type", span)),
        },
        Expression::Const { offset, .. } => check_amend(scope, context, offset, values, span),
        _ => return Err(error!("Initializer must be used with a type", span)),
    }
}

fn check_template_initializer(
    scope: &mut Scope,
    context: &mut Context,
    template_id: TemplateId,
    values: Vec<(ParsedName, ParsedExpression)>,
    span: Span,
) -> Result<Expression> {
    let template = context.get_template(template_id);
    let (mut remaining_fields, template_type) = (template.pars.clone(), template.expr.type_id());
    let mut args = Vec::new();

    for (field_name, expr) in values {
        let field_index = remaining_fields
            .iter()
            .position(|(n, _)| n == &field_name.value)
            .ok_or_else(|| {
                error!(
                    "Template '{}' has no field '{}'",
                    context.get_template(template_id).name,
                    field_name.value,
                    field_name.span
                )
            })?;

        let checked_expr = check_expression(context, scope, expr)?;
        args.push((field_index, field_name.value, checked_expr));
        remaining_fields.remove(field_index);
    }

    if !remaining_fields.is_empty() {
        return Err(error!("Missing field in template initializer", span));
    }

    args.sort_by_key(|(index, _, _)| *index);

    Ok(Expression::TemplateInitializer {
        template: template_id,
        values: args
            .into_iter()
            .map(|(_, name, expr)| (name, expr))
            .collect(),
        typ: template_type,
    })
}

fn check_amend(
    scope: &mut Scope,
    context: &mut Context,
    const_id: ConstId,
    values: Vec<(ParsedName, ParsedExpression)>,
    span: Span,
) -> Result<Expression> {
    let mut checked_values = Vec::new();

    for (name, expr) in values {
        let checked_expr = check_expression(context, scope, expr)?;
        checked_values.push((name, checked_expr));
    }

    let type_id = context.get_const(const_id).typ;
    let typ = context.get_type(type_id);

    if let Type::Struct(strukt) = typ {
        for (name, expr) in &checked_values {
            let (_, field_type, _) = strukt
                .fields
                .iter()
                .find(|f| f.0 == *name.value)
                .ok_or_else(|| {
                    error!(
                        "Struct '{}' has no field '{}'",
                        strukt.name, name.value, name.span
                    )
                })?;

            check_types(context, *field_type, expr.type_id(), span)?;
        }

        Ok(Expression::Amend {
            operand: Box::new(Expression::Const {
                offset: const_id,
                typ: type_id,
            }),
            values: checked_values
                .into_iter()
                .map(|(name, expr)| (name.value, expr))
                .collect(),
            typ: type_id,
        })
    } else {
        Err(error!("Can only amend structs", span))
    }
}

fn check_struct_initializer(
    context: &mut Context,
    scope: &mut Scope,
    type_id: TypeId,
    values: Vec<(ParsedName, ParsedExpression)>,
    span: Span,
) -> Result<Expression> {
    let typ = context.get_type(type_id);
    if let Type::Struct(Struct { fields, .. }) = typ {
        let mut args = Vec::new();
        let mut remaining_fields = fields
            .iter()
            .map(|f| (f.0.clone(), f.1, f.2.clone()))
            .collect::<Vec<_>>();

        for (name, expr) in values {
            let field_index = remaining_fields
                .iter()
                .position(|(n, _, _)| n == &name.value)
                .ok_or_else(|| error!("Field '{}' not found in struct", name.value, name.span))?;

            let typ = remaining_fields[field_index].1;
            let span = expr.span();
            let checked_expr = check_expression(context, scope, expr)?;

            check_types(context, typ, checked_expr.type_id(), span)?;
            args.push((name.value, checked_expr));
            remaining_fields.remove(field_index);
        }

        for (name, _, default) in remaining_fields {
            if let Some(default) = default {
                args.push((name, default));
            } else {
                return Err(error!(
                    "Missing field '{}' in struct initializer",
                    name, span
                ));
            }
        }

        Ok(Expression::Initializer {
            values: args,
            typ: type_id,
        })
    } else {
        Err(error!(
            "Initializer must be used with a struct type, but got {:?}",
            type_id, span
        ))
    }
}

fn check_expression(
    context: &mut Context,
    scope: &mut Scope,
    expr: ParsedExpression,
) -> Result<Expression> {
    match expr {
        ParsedExpression::Boolean { value, .. } => Ok(Expression::Value {
            value: Value::Bool(value),
            typ: TYPE_ID_BOOL,
        }),
        ParsedExpression::BinaryOp { lhs, rhs, op, span } => {
            check_binop(context, scope, *lhs, *rhs, op, span)
        }
        ParsedExpression::UnaryOp { operand, op, span } => {
            check_unop(context, scope, *operand, op, span)
        }
        ParsedExpression::SubscriptExpression {
            operand,
            subscripts,
            span,
        } => check_subscript_expr(context, scope, *operand, subscripts, span),
        ParsedExpression::DotExpr {
            operand,
            name,
            optional,
            span,
        } => check_dot_expr(context, scope, *operand, name, optional, span),
        ParsedExpression::Initializer { typ, values, span } => {
            check_initializer(scope, context, *typ, values, span)
        }
        ParsedExpression::NumericConstant { value, .. } => {
            let expr = match value {
                NumericConstant::Int(value) => Expression::Value {
                    value: Value::Int(value),
                    typ: TYPE_ID_INT,
                },
                NumericConstant::Float(value) => Expression::Value {
                    value: Value::Float(value),
                    typ: TYPE_ID_FLOAT,
                },
            };
            Ok(expr)
        }
        ParsedExpression::ListLiteral { values, span } => {
            let values = check_list_elements(context, scope, values)?;

            let mut types: Vec<TypeId> = values.iter().map(|v| v.typ()).collect::<Vec<_>>();
            types.dedup();

            let inner_type = if values.is_empty() {
                TYPE_ID_UNKNOWN
            } else if types.len() == 1 {
                types[0]
            } else {
                // TODO: This is inconsistent with the ternary operator
                // TODO: proper name and module_id
                let oneof = Type::Oneof(Oneof {
                    module_id: span.file_id,
                    name: format!("__AnonymousOneof__{}", span.start),
                    types,
                });
                context.add_type(oneof)
            };

            let list_type = context.add_type(Type::List { inner: inner_type });

            Ok(Expression::ListLiteral {
                values,
                typ: list_type,
            })
        }
        ParsedExpression::StringLiteral { value, .. } => Ok(Expression::Value {
            typ: context.add_type(Type::StringLiteral {
                value: format!("\"{}\"", value),
            }),
            value: Value::String(value.clone()),
        }),
        ParsedExpression::StringInterpolation { parts, span } => {
            let mut checked_parts = Vec::new();

            for part in parts {
                match part {
                    ParsedStringInterpolationPart::Text(text) => {
                        checked_parts.push(StringInterpolationPart::Text(text.clone()));
                    }
                    ParsedStringInterpolationPart::Expression(expr) => {
                        let checked_expr = check_expression(context, scope, *expr)?;

                        // Verify the expression can be converted to string
                        let expr_type = checked_expr.type_id();

                        // Check if the expression type is compatible with string conversion
                        if !can_convert_to_string(context, expr_type) {
                            return Err(Error::CheckerError(
                                format!("Cannot convert expression of type '{}' to string in interpolation", 
                                    fqn(context, expr_type)),
                                span,
                            ));
                        }

                        checked_parts
                            .push(StringInterpolationPart::Expression(Box::new(checked_expr)));
                    }
                }
            }

            Ok(Expression::StringInterpolation {
                parts: checked_parts,
                typ: TYPE_ID_STRING,
            })
        }
        ParsedExpression::Range { start, end, .. } => {
            let start_span = start.span();
            let start = check_expression(context, scope, *start)?;
            let end = check_expression(context, scope, *end)?;

            if start.type_id() != TYPE_ID_INT || end.type_id() != TYPE_ID_INT {
                return Err(error!("Range bounds must be integers", start_span));
            }

            Ok(Expression::Range {
                start: Box::new(start),
                end: Box::new(end),
                typ: TYPE_ID_RANGE,
            })
        }
        ParsedExpression::Name(name) => {
            if name.value == "null" {
                return Ok(Expression::Value {
                    value: Value::Null,
                    typ: TYPE_ID_UNKNOWN,
                });
            }

            if let Some(type_id) = scope.find_type(&name.value, context) {
                Ok(Expression::Value {
                    value: Value::Type(type_id),
                    typ: TYPE_ID_TYPE,
                })
            } else if let Some((offset, typ)) = scope.find_variable(&name.value) {
                Ok(Expression::Var { offset, typ })
            } else if let Some(const_id) = scope.find_const(&name.value, context) {
                let konst = context.get_const(const_id);
                Ok(Expression::Const {
                    offset: const_id,
                    typ: konst.typ,
                })
            } else if let Some(function_id) = scope.find_function(&name.value, context) {
                let function = context.get_function(function_id);
                Ok(Expression::Value {
                    value: Value::Function(function_id, function.type_id),
                    typ: function.type_id,
                })
            } else if let Some(template_id) = scope.find_template(&name.value, context) {
                let template = context.get_template(template_id);
                Ok(Expression::Value {
                    value: Value::Template(template_id, template.type_id),
                    typ: template.type_id,
                })
            } else {
                Err(error!(
                    "Name '{}' not found in scope",
                    name.value, name.span
                ))
            }
        }
        ParsedExpression::Call {
            operand,
            values,
            span,
        } => {
            let operand = check_expression(context, scope, *operand)?;

            let (pars, mut return_type) = match context.get_type(operand.type_id()) {
                Type::Function { pars, return_type } => (pars.clone(), return_type.clone()),
                _ => {
                    return Err(error!("Cannot call this expression, not a function.", span));
                }
            };

            let mut args = Vec::new();

            for v in values {
                let span = v.span();
                let arg = check_expression(context, scope, v)?;
                args.push((arg, span));
            }

            let arg_types_and_spans = args
                .iter()
                .map(|(expr, span)| (expr.type_id(), span.clone()))
                .collect::<Vec<_>>();
            let substitutions = check_function_arg_types(context, pars, arg_types_and_spans, span)?;

            if type_is_generic(context, return_type) {
                return_type = substitute(context, return_type, &substitutions).expect(
                    "Invalid generic function return type, a generic return type has to depend on generic function parameter types",
                );
            }

            Ok(Expression::FunctionCall {
                function: Box::new(operand),
                args: args.into_iter().map(|(expr, _)| expr).collect(),
                return_type,
            })
        }
        _ => Err(error!("Not implemented {:?}", expr, expr.span())),
    }
}

pub fn check_list_elements(
    context: &mut Context,
    scope: &mut Scope,
    elements: Vec<ParsedListElement>,
) -> Result<Vec<ListElement>> {
    if elements.is_empty() {
        return Ok(Vec::new());
    }

    let mut values = Vec::new();

    for el in elements {
        let val = check_list_element(context, scope, el)?;
        values.push(val);
    }

    Ok(values)
}

pub fn check_list_element(
    context: &mut Context,
    scope: &mut Scope,
    el: ParsedListElement,
) -> Result<ListElement> {
    match el {
        ParsedListElement::Expr(expr) => {
            let expr = check_expression(context, scope, expr)?;
            Ok(ListElement::Expr(expr))
        }
        ParsedListElement::ForElement(compr) => {
            let ParsedForElement {
                var,
                iterable,
                body,
                ..
            } = compr;
            let iterable_span = iterable.span();
            let iterable = check_expression(context, scope, iterable)?;

            let var_type_id = if let Some(var_type_id) = type_iterable(context, iterable.type_id())
            {
                var_type_id
            } else {
                return Err(error!(
                    "ForElement iterable must be a Range or List",
                    iterable_span
                ));
            };

            let mut scope = scope.child();

            scope.add_variable(var.value.clone(), var_type_id);

            let values = check_list_elements(context, &mut scope, body)?;
            if values.is_empty() {
                return Err(error!("ForElement must not be empty", compr.span));
            }
            Ok(ListElement::ForElement(ForElement {
                var: var.value,
                iterable,
                body: values,
            }))
        }
        ParsedListElement::IfElement(if_element) => {
            let span = if_element.condition.span();
            let condition = check_expression(context, scope, if_element.condition)?;
            check_types(context, TYPE_ID_BOOL, condition.type_id(), span)?;
            let values = check_list_elements(context, scope, if_element.body)?;
            if values.is_empty() {
                return Err(error!("IfElement must not be empty", if_element.span));
            }
            Ok(ListElement::IfElement(IfElement {
                condition,
                body: values,
            }))
        }
    }
}

pub fn check_struct(
    context: &mut Context,
    scope: &mut Scope,
    strukt: ParsedStruct,
) -> Result<Struct> {
    let mut scope = scope.child();

    let fields = strukt
        .fields
        .into_iter()
        .map(|(name, pt, default)| {
            let typ = check_type(context, &mut scope, pt)?;
            let default = if let Some(default) = default {
                let default_span = default.span();
                let checked_default = check_expression(context, &mut scope, default)?;
                check_types(context, typ, checked_default.type_id(), default_span)?;
                Some(checked_default)
            } else {
                None
            };
            Ok((name.value.clone(), typ, default))
        })
        .collect::<Result<Vec<_>>>()?;

    Ok(Struct {
        module_id: 0, // is set later
        name: strukt.name.value.clone(),
        fields,
    })
}

pub fn type_includes(context: &Context, target: TypeId, source: TypeId) -> bool {
    let target_type = context.get_type(target);
    let source_type = context.get_type(source);

    match (target_type, source_type) {
        (Type::Any, _) => true,
        (_, Type::Unknown) => true,
        (Type::List { inner: inner1 }, Type::List { inner: inner2 }) => {
            type_includes(context, *inner1, *inner2)
        }
        (_, Type::Oneof(oneof)) => oneof
            .types
            .iter()
            .all(|t| type_includes(context, target, *t)),
        (Type::String, Type::StringLiteral { .. }) => true,
        (Type::Null, _) => false,
        (Type::Optional { .. }, Type::Null) => true,
        (Type::Optional { inner: inner1 }, Type::Optional { inner: inner2 }) => {
            type_includes(context, *inner1, *inner2)
        }
        (Type::Optional { inner: inner1 }, _) => type_includes(context, *inner1, source),
        (Type::Oneof(oneof), _) => oneof
            .types
            .iter()
            .any(|t| type_includes(context, *t, source)),
        _ => target == source,
    }
}

pub fn type_iterable(context: &Context, type_id: TypeId) -> Option<TypeId> {
    let typ = context.get_type(type_id);

    match typ {
        Type::List { inner } => Some(*inner),
        Type::Range => Some(TYPE_ID_INT),
        _ => None,
    }
}

pub fn fqn(context: &Context, type_id: TypeId) -> String {
    let typ = context.get_type(type_id);
    typ.fully_qualified_name(context)
}

pub fn type_is_generic(context: &Context, type_id: TypeId) -> bool {
    match context.get_type(type_id) {
        Type::TypeVar { .. } => true,
        Type::List { inner } => type_is_generic(context, *inner),
        Type::Map { key, value } => {
            type_is_generic(context, *key) || type_is_generic(context, *value)
        }
        _ => false,
    }
}

// Tries to match a generic tartget type (TypeVar) to a non-generic source type.
pub fn match_generic_type(
    context: &Context,
    generic_target: TypeId,
    source: TypeId,
    span: Span,
) -> Result<Vec<(TypeId, TypeId)>> {
    assert!(type_is_generic(context, generic_target));

    let generic_type = context.get_type(generic_target);
    let source_type = context.get_type(source);

    match generic_type {
        Type::TypeVar { constraint, .. } => {
            if let Some(constraint) = constraint {
                // Check if the source type satisfies the constraint.
                // The satisfying type might be super type (e.g. StringLiteral -> String).
                if let Some(satisfying_type) = constraint.satisfied_by(context, source) {
                    return Ok(vec![(generic_target, satisfying_type)]);
                } else {
                    return Err(error!(
                        "Type '{}' does not satisfy constraint '{}'.",
                        fqn(context, source),
                        constraint.display(context),
                        span
                    ));
                }
            }
            return Ok(vec![(generic_target, source)]);
        }
        Type::List { inner: g_inner } => {
            if let Type::List { inner } = source_type {
                return match_generic_type(context, *g_inner, *inner, span);
            }
            return Err(error!(
                "Type mismatch: expected List, got {}",
                fqn(context, source),
                span
            ));
        }
        _ => {
            unimplemented!()
        }
    }
}

pub fn check_function_arg_types(
    context: &mut Context,
    target: Vec<TypeId>,
    source: Vec<(TypeId, Span)>,
    span: Span,
) -> Result<Vec<(TypeId, TypeId)>> {
    if target.len() != source.len() {
        return Err(error!(
            "Function argument count mismatch: expected {} but got {}.",
            target.len(),
            source.len(),
            span
        ));
    }

    // Type matching goes from left to right and accumulates substitutions.
    let mut substitutions = Vec::new();

    for (target, source) in target.iter().zip(source.iter()) {
        // Replace the target type with its substitution if it exists.
        let target = if let Some(substitute) = substitute(context, *target, &substitutions) {
            substitute
        } else {
            *target
        };

        // If target type is still generic (has not been substituted yet), try to match it with the source type.
        if type_is_generic(context, target) {
            let subs = match_generic_type(context, target, source.0, source.1)?;
            substitutions.extend(subs);

            // Now try to substitute the target again, if this fails, type matching failed.
            if substitute(context, target, &substitutions).is_none() {
                Err(error!(
                    "Type mismatch: expected '{}' but got '{}'.",
                    fqn(context, target),
                    fqn(context, source.0),
                    source.1
                ))?
            };
        // Otherwise check if target is compatible with source.
        } else {
            check_types(context, target, source.0, source.1)?;
        }
    }

    // Return the substitutions, because might be required to substitute the return type of the function.
    Ok(substitutions)
}

pub fn substitute(
    context: &mut Context,
    target: TypeId,
    subsitutions: &Vec<(TypeId, TypeId)>,
) -> Option<TypeId> {
    if !type_is_generic(context, target) {
        return None;
    }

    match context.get_type(target) {
        Type::TypeVar { .. } => {
            // direct substitution
            if let Some(substitution) = subsitutions.iter().find(|(t, _)| *t == target) {
                return Some(substitution.1);
            }
        }
        Type::List { inner } => {
            if let Some(substitution) = substitute(context, *inner, subsitutions) {
                return Some(context.add_type(Type::List {
                    inner: substitution,
                }));
            }
        }
        Type::Map { .. } => {
            unimplemented!()
        }

        _ => {}
    }

    None
}

pub fn check_types(context: &Context, target: TypeId, source: TypeId, span: Span) -> Result<()> {
    if !type_includes(context, target, source) {
        return Err(error!(
            "Type mismatch: expected '{}' but got '{}'.",
            fqn(context, target),
            fqn(context, source),
            span
        ));
    }
    Ok(())
}

pub fn check_template(
    context: &mut Context,
    scope: &mut Scope,
    template: ParsedTemplate,
) -> Result<Template> {
    let pars = template
        .fields
        .into_iter()
        .map(|(name, pt)| {
            let typ = check_type(context, scope, pt)?;
            Ok((name.value.clone(), typ))
        })
        .collect::<Result<Vec<_>>>()?;

    let mut scope = scope.child();

    for (par_name, par_type) in &pars {
        scope.add_variable(par_name.clone(), *par_type);
    }

    let expr = check_expression(context, &mut scope, template.expr)?;

    let type_id = context.add_type(Type::Template {
        pars: pars.iter().map(|it| it.1).collect(),
        return_type: expr.type_id(),
    });

    Ok(Template {
        name: template.name.value.clone(),
        pars,
        expr,
        type_id,
    })
}

pub fn check_function(
    context: &mut Context,
    scope: &mut Scope,
    function: ParsedFunction,
) -> Result<Function> {
    let pars = function
        .pars
        .into_iter()
        .map(|(name, pt)| {
            let typ = check_type(context, scope, pt)?;
            Ok((name.value.clone(), typ))
        })
        .collect::<Result<Vec<_>>>()?;

    let mut scope = scope.child();

    for (par_name, par_type) in &pars {
        scope.add_variable(par_name.clone(), *par_type);
    }

    let expr = check_expression(context, &mut scope, function.expr)?;

    let type_id = context.add_type(Type::Function {
        pars: pars.iter().map(|it| it.1).collect(),
        return_type: expr.type_id(),
    });

    Ok(Function {
        name: function.name.value.clone(),
        pars,
        return_type: expr.type_id(),
        expr,
        type_id,
    })
}
pub fn check_oneof(context: &mut Context, scope: &mut Scope, oneof: ParsedOneof) -> Result<Oneof> {
    let types = oneof
        .types
        .into_iter()
        .map(|pt| {
            let typ = check_type(context, scope, pt)?;
            Ok(typ)
        })
        .collect::<Result<Vec<_>>>()?;

    Ok(Oneof {
        module_id: 0, // is set later
        name: oneof.name.value.clone(),
        types,
    })
}

fn check_name_available(context: &Context, scope: &Scope, name: &str, span: Span) -> Result<()> {
    if scope.is_name_defined(name, context) {
        return Err(error!(
            "Name '{}' already defined in this scope.",
            name, span
        ));
    }
    Ok(())
}

pub fn check_const(context: &mut Context, scope: &mut Scope, konst: ParsedConst) -> Result<Const> {
    check_name_available(context, scope, &konst.name.value, konst.span)?;

    let value_span = konst.value.span();
    let value = check_expression(context, scope, konst.value)?;

    let typ = if let Some(typ) = konst.typ {
        let typ = check_type(context, scope, typ)?;
        check_types(context, typ, value.type_id(), value_span)?;
        typ
    } else {
        value.type_id()
    };

    Ok(Const {
        name: konst.name.value.clone(),
        typ,
        value,
    })
}

fn check_generic_type(
    context: &mut Context,
    scope: &mut Scope,
    name: ParsedExpression,
    args: Vec<ParsedExpression>,
    span: Span,
) -> Result<TypeId> {
    let name = match name {
        ParsedExpression::Name(name) => name,
        _ => {
            return Err(error!("Expected type name for generic type.", span));
        }
    };

    match name.value.as_str() {
        "List" => {
            if args.len() != 1 {
                return Err(error!(
                    "Expected exactly 1 type argument for generic List.",
                    span
                ));
            }
            let inner = check_type(context, scope, args.into_iter().next().unwrap())?;
            Ok(context.add_type(Type::List { inner }))
        }
        "Map" => {
            if args.len() != 2 {
                return Err(error!(
                    "Expected exactly 2 type arguments for generic Map.",
                    span
                ));
            }
            let mut iter = args.into_iter();
            let key = check_type(context, scope, iter.next().unwrap())?;
            let value = check_type(context, scope, iter.next().unwrap())?;
            Ok(context.add_type(Type::Map { key, value }))
        }
        _ => Err(error!(
            "Generic type with name '{}' not defined.",
            name.value, name.span
        )),
    }
}

fn check_named_type(context: &Context, scope: &Scope, name: ParsedName) -> Result<TypeId> {
    if let Some(type_id) = scope.find_type(&name.value, &context) {
        Ok(type_id)
    } else {
        Err(error!(
            "No type with name '{}' defined in scope.",
            name.value, name.span
        ))
    }
}

fn check_type(context: &mut Context, scope: &mut Scope, typ: ParsedExpression) -> Result<TypeId> {
    match typ {
        ParsedExpression::Name(name) => check_named_type(context, scope, name),
        ParsedExpression::StringLiteral { value, .. } => {
            Ok(context.add_type(Type::StringLiteral {
                value: format!("\"{}\"", value),
            }))
        }
        ParsedExpression::SubscriptExpression {
            operand,
            subscripts,
            span,
        } => check_generic_type(context, scope, *operand, subscripts, span),
        ParsedExpression::Optional { value, .. } => {
            let inner = check_type(context, scope, *value)?;
            Ok(context.add_type(Type::Optional { inner }))
        }
        _ => Err(error!("Expression is not a valid type.", typ.span())),
    }
}

pub struct Compiler {
    files: Vec<(String, String)>,
    file_stack: Vec<FileId>,
}

pub type FileId = usize;

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            files: Vec::new(),
            file_stack: Vec::new(),
        }
    }

    pub fn add_file(&mut self, abs_path: String, contents: String) -> FileId {
        self.files.push((abs_path, contents));
        self.files.len() - 1
    }

    pub fn find_file(&self, abs_path: &str) -> Option<FileId> {
        for (i, (path, _)) in self.files.iter().enumerate() {
            if path == abs_path {
                return Some(i);
            }
        }
        None
    }

    pub fn get_or_compile_module(
        &mut self,
        context: &mut Context,
        scope: &mut Scope,
        module_identifier: &str,
    ) -> Result<ModuleId> {
        if let Some(module_id) = context.find_module(module_identifier) {
            return Ok(module_id);
        }

        let file_id = match self.find_file(module_identifier) {
            Some(id) => id,
            None => {
                return Err(Error::CompilerError(format!(
                    "File '{}' not found",
                    module_identifier
                )))
            }
        };

        if self.file_stack.contains(&file_id) {
            return Err(Error::CompilerError(format!(
                "Circular import detected: {}",
                module_identifier
            )));
        }

        self.file_stack.push(file_id);

        let (_, contents) = &self.files[file_id];

        let tokens = crate::lexer::lex(file_id, contents)?;
        let token_stream = crate::parser::TokenStream::new(tokens);
        let parsed_module = crate::parser::parse_module(&token_stream)?;
        let module = check_module(
            self,
            context,
            scope,
            parsed_module,
            module_identifier.to_string(),
        )?;

        self.file_stack.pop();

        Ok(context.add_module(module))
    }

    pub fn resolve_file_id(&self, file_id: FileId) -> (&str, &str) {
        let (file_path, content) = &self.files[file_id];
        (file_path, content)
    }
}

pub fn check_import_path(current_path: &str, path: &str, span: Span) -> Result<String> {
    let mut path_ = path.to_string();

    // First check named module (not a file module).
    let pattern = r"^[A-Za-z_][A-Za-z0-9_]*$";
    let re = Regex::new(pattern).expect("should be valid regex");

    if re.is_match(path_.as_str()) {
        return Ok(path_.to_string());
    }

    if path_.is_empty() {
        return Err(error!("Empty import path", span));
    }

    let invalid_chars = ['\0', '\r', '\n', '\\'];

    if path_.contains(|c| invalid_chars.contains(&c)) {
        return Err(error!("Invalid character in import path", span));
    }

    if path_.ends_with('/') {
        return Err(error!("Import path must not end with '/'", span));
    }

    if !path_.starts_with('/') {
        // The returned module path is assumbed to be an absolute path with file name.
        let base = current_path.to_string();
        // Strip off the file name including the last '/'.
        let base = match base.rfind('/') {
            Some(index) => &base[..index],
            _ => unreachable!(), // This should never happen
        };
        path_ = format!("{}/{}", base, path_);
    }

    let mut parts = Vec::new();

    for part in path_.split('/') {
        match part {
            "" | "." => {}
            ".." => {
                if parts.is_empty() {
                    return Err(error!("Invalid import path", span));
                }
                parts.pop();
            }
            _ => parts.push(part),
        }
    }

    Ok(format!("/{}", parts.join("/")))
}

pub fn check_module(
    compiler: &mut Compiler,
    context: &mut Context,
    scope: &mut Scope,
    module: ParsedModule,
    path: String,
) -> Result<Module> {
    let mut consts = Vec::new();
    let mut types = Vec::new();
    let mut templates = Vec::new();
    let mut functions = Vec::new();

    let mut scope = scope.child();

    let mut parsed_oneofs = Vec::new();
    let mut parsed_statements = Vec::new();

    let mut import_cnt = 0;

    for stmt in &module.statements {
        match stmt {
            ParsedStatement::Import(import) => {
                let ParsedImport {
                    path: import_path,
                    span,
                } = import;
                let path = check_import_path(&path, import_path, *span)?;
                let module_id =
                    compiler.get_or_compile_module(context, &mut Scope::new(), &path)?;
                scope.add_module(module_id);
                import_cnt += 1;
            }
            ParsedStatement::FromImport(import) => {
                let ParsedFromImport {
                    path: import_path,
                    names,
                    span,
                } = import;
                let path = check_import_path(&path, import_path, *span)?;
                let module_id =
                    compiler.get_or_compile_module(context, &mut Scope::new(), &path)?;
                let module = context.get_module(module_id);

                let import_list = names
                    .iter()
                    .map(|n| n.value.clone())
                    .collect::<HashSet<_>>();

                let mut remaining_imports = import_list.clone();

                for import_name in names {
                    for const_id in &module.consts {
                        let konst = context.get_const(*const_id);
                        if import_name.value == konst.name {
                            check_name_available(
                                context,
                                &mut scope,
                                &import_name.value,
                                import_name.span,
                            )?;
                            remaining_imports.remove(&konst.name);
                            scope.add_const(*const_id);
                            consts.push(*const_id);
                        }
                    }

                    for type_id in &module.types {
                        let typ = context.get_type(*type_id);
                        let type_name = match typ {
                            Type::Struct(s) => s.name.clone(),
                            Type::Oneof(o) => o.name.clone(),
                            _ => continue, // Only structs and oneofs can be imported
                        };

                        if import_name.value == type_name {
                            check_name_available(
                                context,
                                &mut scope,
                                &import_name.value,
                                import_name.span,
                            )?;
                            remaining_imports.remove(type_name.as_str());
                            scope.add_type(*type_id);
                            types.push(*type_id);
                        }
                    }

                    for template_id in &module.templates {
                        let template = context.get_template(*template_id);
                        if import_name.value == template.name {
                            check_name_available(
                                context,
                                &mut scope,
                                &import_name.value,
                                import_name.span,
                            )?;
                            remaining_imports.remove(&template.name);
                            scope.add_template(*template_id);
                            templates.push(*template_id);
                        }
                    }

                    for function_id in &module.functions {
                        let function = context.get_function(*function_id);
                        if import_name.value == function.name {
                            check_name_available(
                                context,
                                &mut scope,
                                &import_name.value,
                                import_name.span,
                            )?;
                            remaining_imports.remove(&function.name);
                            scope.add_function(*function_id);
                            templates.push(*function_id);
                        }
                    }
                }

                if !remaining_imports.is_empty() {
                    return Err(error!(
                        "Imported names not found in module '{}': {:?}.",
                        module.identifier, remaining_imports, import.span
                    ));
                }

                import_cnt += 1;
            }
            _ => break,
        }
    }

    // NOTE: This assumes that the module is added to the context right after this function returns and no other
    // module is added before that.
    let this_module_id = context.modules.len();

    for stmt in module.statements.into_iter().skip(import_cnt) {
        match stmt {
            ParsedStatement::FromImport(..) | ParsedStatement::Import(..) => {
                return Err(error!(
                    "Import statements must be at the beginning of the module",
                    stmt.span()
                ));
            }
            ParsedStatement::Struct(strukt) => {
                check_name_available(context, &mut scope, &strukt.name.value, strukt.name.span)?;
                let type_id = context.add_type(Type::Struct(Struct {
                    module_id: this_module_id,
                    name: strukt.name.value.clone(),
                    fields: Vec::new(),
                }));
                scope.add_type(type_id);
                parsed_statements.push(ParsedStatement::Struct(strukt));
            }
            ParsedStatement::Oneof(oneof) => {
                check_name_available(context, &mut scope, &oneof.name.value, oneof.name.span)?;
                let type_id = context.add_type(Type::Oneof(Oneof {
                    module_id: this_module_id,
                    name: oneof.name.value.clone(),
                    types: Vec::new(),
                }));
                scope.add_type(type_id);
                parsed_oneofs.push(oneof);
            }
            ParsedStatement::Template(template) => {
                check_name_available(
                    context,
                    &mut scope,
                    &template.name.value,
                    template.name.span,
                )?;
                let template_id = context.add_template(Template {
                    name: template.name.value.clone(),
                    pars: Vec::new(),
                    expr: Expression::Garbage,
                    type_id: TYPE_ID_UNKNOWN,
                });
                scope.add_template(template_id);
                parsed_statements.push(ParsedStatement::Template(template));
            }
            ParsedStatement::Function(function) => {
                check_name_available(
                    context,
                    &mut scope,
                    &function.name.value,
                    function.name.span,
                )?;
                let function_id = context.add_function(Function {
                    name: function.name.value.clone(),
                    pars: Vec::new(),
                    return_type: TYPE_ID_UNKNOWN,
                    expr: Expression::Garbage,
                    type_id: TYPE_ID_UNKNOWN,
                });
                scope.add_function(function_id);
                parsed_statements.push(ParsedStatement::Function(function));
            }
            ParsedStatement::Const(konst) => {
                parsed_statements.push(ParsedStatement::Const(konst));
            }
        }
    }

    // Oneofs go before struct because oneof only refernces types or struct names.
    for oneof in parsed_oneofs {
        let mut oneof = check_oneof(context, &mut scope, oneof)?;
        let type_id = scope
            .find_type(&oneof.name, &context)
            .expect("oneof declaration should have been added");
        oneof.module_id = this_module_id;
        let typ = Type::Oneof(oneof);
        context.set_type(type_id, typ);
        types.push(type_id);
    }

    // There is no clear order of const / template / struct:
    // - consts can contain structs / templates
    // - templates can contain structs / consts
    // - structs can contain consts / templates
    // => Require that they have to be declared in the order that they are used.
    for stmt in parsed_statements {
        match stmt {
            ParsedStatement::Struct(strukt) => {
                let mut strukt = check_struct(context, &mut scope, strukt)?;
                let type_id = scope
                    .find_type(&strukt.name, &context)
                    .expect("struct declaration should have been added");
                strukt.module_id = this_module_id;
                context.set_type(type_id, Type::Struct(strukt));
                types.push(type_id);
            }
            ParsedStatement::Template(template) => {
                let template = check_template(context, &mut scope, template)?;
                let template_id = scope
                    .find_template(&template.name, &context)
                    .expect("template declaration should have been added");
                context.set_template(template_id, template);
                templates.push(template_id);
            }
            ParsedStatement::Function(function) => {
                let function = check_function(context, &mut scope, function)?;
                let function_id = scope
                    .find_function(&function.name, &context)
                    .expect("function declaration should have been added");
                context.set_function(function_id, function);
                functions.push(function_id);
            }
            ParsedStatement::Const(konst) => {
                let konst = check_const(context, &mut scope, konst)?;
                let const_id = context.add_const(konst);
                scope.add_const(const_id);
                consts.push(const_id);
            }
            _ => {
                unreachable!()
            }
        }
    }

    Ok(Module {
        identifier: path,
        consts,
        types,
        templates,
        functions,
    })
}

// Helper function to determine if a type can be converted to string in interpolation
fn can_convert_to_string(context: &Context, type_id: TypeId) -> bool {
    let typ = context.get_type(type_id);

    match typ {
        // Basic types that can be converted to string
        Type::String | Type::StringLiteral { .. } => true,
        Type::Int | Type::Float | Type::Bool => true,

        // Optional types - check if their inner type can be converted
        Type::Optional { inner } => can_convert_to_string(context, *inner),

        // For any type, we'll allow string representation
        Type::Any => true,

        // Add other types that can be naturally converted to string
        // For example, you might want to allow Range, Null, etc.
        Type::Range | Type::Null => true,

        Type::Oneof(oneof) => {
            for typ in &oneof.types {
                if !can_convert_to_string(context, *typ) {
                    return false;
                }
            }
            true
        }

        // For all other types, we could potentially add custom formatting rules
        // but for now, let's be conservative
        _ => false,
    }
}
