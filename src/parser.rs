use crate::common::{Error, Result, Span};
use crate::lexer::{NumericConstant, StringInterpolationPart, Token, TokenKind};
use core::str;
use std::cell::Cell;

macro_rules! error {
    ($msg:expr, $span:expr) => {
        Error::ParserError(format!($msg), $span)
    };
    ($msg:expr, $arg1:expr, $span:expr) => {
        Error::ParserError(format!($msg, $arg1), $span)
    };
    ($msg:expr, $arg1:expr, $arg2:expr, $span:expr) => {
        Error::ParserError(format!($msg, $arg1, $arg2), $span)
    };
    ($msg:expr, $arg1:expr, $arg2:expr, $arg3:expr, $span:expr) => {
        Error::ParserError(format!($msg, $arg1, $arg2, $arg3), $span)
    };
}

#[derive(Clone, Debug, PartialEq)]
pub enum Operator {
    Add,
    Subtract,

    Multiply,
    Divide,

    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,

    LogicalAnd,
    LogicalOr,

    LogicalNot,

    NullCoalescing,
    TypeCast,
    TypeTest,

    TernaryCondition,
    TernaryChoice,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ParsedOperator {
    pub operator: Operator,
    pub span: Span,
}

impl ParsedOperator {
    fn precedence(&self) -> usize {
        match self.operator {
            // TODO: not really sure what precedence this should get
            Operator::LogicalNot => 5,

            Operator::LogicalOr => 15,
            Operator::LogicalAnd => 20,

            Operator::Equal => 25,
            Operator::NotEqual => 25,
            Operator::LessThan => 25,
            Operator::GreaterThan => 25,
            Operator::LessThanOrEqual => 25,
            Operator::GreaterThanOrEqual => 25,
            Operator::TypeTest => 25,

            Operator::Add => 35,
            Operator::Subtract => 40,

            Operator::Multiply => 45,
            Operator::Divide => 50,

            Operator::TypeCast => 55,
            Operator::NullCoalescing => 55,

            Operator::TernaryCondition => 1,
            Operator::TernaryChoice => 1,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct ParsedName {
    pub value: String,
    pub span: Span,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ParsedForElement {
    pub var: ParsedName,
    pub iterable: ParsedExpression,
    pub body: Vec<ParsedListElement>,
    pub span: Span,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ParsedIfElement {
    pub condition: ParsedExpression,
    pub body: Vec<ParsedListElement>,
    pub span: Span,
}

#[derive(PartialEq, Debug, Clone)]
pub enum ParsedListElement {
    Expr(ParsedExpression),
    ForElement(ParsedForElement),
    IfElement(ParsedIfElement),
}

impl ParsedListElement {
    pub fn span(&self) -> Span {
        match self {
            ParsedListElement::Expr(expr) => expr.span(),
            ParsedListElement::ForElement(for_element) => for_element.span,
            ParsedListElement::IfElement(if_element) => if_element.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParsedStringInterpolationPart {
    Text(String),
    Expression(Box<ParsedExpression>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum ParsedExpression {
    // Standalone
    BinaryOp {
        lhs: Box<ParsedExpression>,
        op: ParsedOperator,
        rhs: Box<ParsedExpression>,
        span: Span,
    },
    Boolean {
        value: bool,
        span: Span,
    },
    Call {
        operand: Box<ParsedExpression>,
        values: Vec<ParsedExpression>,
        span: Span,
    },
    Garbage {
        span: Span,
    },
    DotExpr {
        operand: Box<ParsedExpression>,
        name: ParsedName,
        optional: bool,
        span: Span,
    },
    Initializer {
        typ: Box<ParsedExpression>,
        values: Vec<(ParsedName, ParsedExpression)>,
        span: Span,
    },
    ListLiteral {
        values: Vec<ParsedListElement>,
        span: Span,
    },
    Map {
        entries: Vec<(ParsedExpression, ParsedExpression)>,
        span: Span,
    },
    Name(ParsedName),
    NumericConstant {
        value: NumericConstant,
        span: Span,
    },
    Optional {
        value: Box<ParsedExpression>,
        span: Span,
    },
    OptionalNull {
        span: Span,
    },
    Range {
        start: Box<ParsedExpression>,
        end: Box<ParsedExpression>,
        span: Span,
    },
    StringInterpolation {
        parts: Vec<ParsedStringInterpolationPart>,
        span: Span,
    },
    StringLiteral {
        value: String,
        span: Span,
    },
    SubscriptExpression {
        operand: Box<ParsedExpression>,
        subscripts: Vec<ParsedExpression>,
        span: Span,
    },
    // TernaryExpression {
    //     condition: Box<ParsedExpression>,
    //     lhs: Box<ParsedExpression>,
    //     rhs: Box<ParsedExpression>,
    //     span: Span,
    // },
    UnaryOp {
        operand: Box<ParsedExpression>,
        op: ParsedOperator,
        span: Span,
    },
}

impl ParsedExpression {
    pub fn span(&self) -> Span {
        match self {
            ParsedExpression::BinaryOp { span, .. } => *span,
            ParsedExpression::Boolean { span, .. } => *span,
            ParsedExpression::Call { span, .. } => *span,
            ParsedExpression::DotExpr { span, .. } => *span,
            ParsedExpression::Initializer { span, .. } => *span,
            ParsedExpression::Garbage { span } => *span,
            ParsedExpression::ListLiteral { span, .. } => *span,
            ParsedExpression::Map { span, .. } => *span,
            ParsedExpression::Name(name) => name.span,
            ParsedExpression::NumericConstant { span, .. } => *span,
            ParsedExpression::Optional { span, .. } => *span,
            ParsedExpression::OptionalNull { span } => *span,
            ParsedExpression::Range { span, .. } => *span,
            ParsedExpression::StringInterpolation { span, .. } => *span,
            ParsedExpression::StringLiteral { span, .. } => *span,
            ParsedExpression::SubscriptExpression { span, .. } => *span,
            // ParsedExpression::TernaryExpression { span, .. } => *span,
            ParsedExpression::UnaryOp { span, .. } => *span,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct ParsedStruct {
    pub name: ParsedName,
    pub fields: Vec<(ParsedName, ParsedExpression, Option<ParsedExpression>)>,
    pub span: Span,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ParsedTemplate {
    pub name: ParsedName,
    pub fields: Vec<(ParsedName, ParsedExpression)>,
    pub expr: ParsedExpression,
    pub span: Span,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ParsedFunction {
    pub name: ParsedName,
    pub pars: Vec<(ParsedName, ParsedExpression)>,
    pub expr: ParsedExpression,
    pub span: Span,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ParsedConst {
    pub name: ParsedName,
    pub typ: Option<ParsedExpression>,
    pub value: ParsedExpression,
    pub span: Span,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ParsedFromImport {
    pub path: String,
    pub names: Vec<ParsedName>,
    pub span: Span,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ParsedImport {
    pub path: String,
    pub span: Span,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ParsedOneof {
    pub name: ParsedName,
    pub types: Vec<ParsedExpression>,
    pub span: Span,
}

#[derive(PartialEq, Debug, Clone)]
pub enum ParsedStatement {
    FromImport(ParsedFromImport),
    Import(ParsedImport),
    Const(ParsedConst),
    Struct(ParsedStruct),
    Template(ParsedTemplate),
    Function(ParsedFunction),
    Oneof(ParsedOneof),
}

impl ParsedStatement {
    pub fn span(&self) -> Span {
        match self {
            ParsedStatement::Const(c) => c.span,
            ParsedStatement::FromImport(i) => i.span,
            ParsedStatement::Import(i) => i.span,
            ParsedStatement::Struct(s) => s.span,
            ParsedStatement::Template(t) => t.span,
            ParsedStatement::Function(f) => f.span,
            ParsedStatement::Oneof(o) => o.span,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct ParsedModule {
    pub statements: Vec<ParsedStatement>,
    pub span: Span,
}

pub struct TokenStream {
    tokens: Vec<Token>,
    index: Cell<usize>,
}

impl TokenStream {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            index: Cell::new(0),
        }
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.index.get()).unwrap_or(&Token {
            kind: TokenKind::Eof,
            span: Span {
                file_id: 0,
                start: 0,
                end: 0,
            },
        })
    }

    fn advance(&self) {
        self.index.set(self.index.get() + 1);
    }

    fn consume_name(&self, name: &str, context: &str) -> Result<Span> {
        let token = self.peek();
        match token.kind {
            TokenKind::Name(_) => {
                self.advance();
                Ok(token.span)
            }
            _ => Err(error!(
                "Expected name '{}' in '{}', found {:?}",
                name, context, token.kind, token.span
            )),
        }
    }

    fn current_span(&self) -> Span {
        self.tokens[self.index.get()].span
    }
}

macro_rules! consume_token {
    // Variant with an inner value and context
    ($stream:expr, TokenKind::$expected_variant:ident($inner:ident), $context:expr) => {{
        let token = $stream.peek();
        match &token.kind {
            TokenKind::$expected_variant($inner) => {
                $stream.advance();
                Ok(($inner.clone(), token.span))
            }
            _ => Err(error!(
                "Unexpected token in {}: expected {}, found {:?}",
                $context,
                stringify!($expected_variant),
                token.kind,
                token.span
            )),
        }
    }};
    // Variant with no inner value and context
    ($stream:expr, TokenKind::$expected_variant:ident, $context:expr) => {{
        let token = $stream.peek();
        match token.kind {
            TokenKind::$expected_variant => {
                $stream.advance();
                Ok(token.span)
            }
            _ => Err(error!(
                "Unexpected token in {}: expected {}, found {:?}",
                $context,
                stringify!($expected_variant),
                token.kind,
                token.span
            )),
        }
    }};
    // Variant with no inner value and context
    ($stream:expr, TokenCKind::$expected_variant:ident, $context:expr) => {{
        let token = $stream.peek();
        match token.kind {
            TokenKind::$expected_variant => {
                $stream.advance();
                Ok(token.span)
            }
            _ => Err(error!(
                "Unexpected token in {}: expected {}, found {:?}",
                $context,
                stringify!($expected_variant),
                token.kind,
                token.span
            )),
        }
    }};
}

pub fn parse_operator(token_stream: &TokenStream) -> Result<ParsedOperator> {
    let token = token_stream.peek();

    let operator = match &token.kind {
        TokenKind::DoubleEqual => Operator::Equal,
        TokenKind::NotEqual => Operator::NotEqual,
        TokenKind::Plus => Operator::Add,
        TokenKind::Minus => Operator::Subtract,
        TokenKind::ForwardSlash => Operator::Divide,
        TokenKind::Asterisk => Operator::Multiply,
        TokenKind::LessThan => Operator::LessThan,
        TokenKind::LessThanOrEqual => Operator::LessThanOrEqual,
        TokenKind::GreaterThan => Operator::GreaterThan,
        TokenKind::GreaterThanOrEqual => Operator::GreaterThanOrEqual,
        TokenKind::ExclamationMark => Operator::LogicalNot,
        TokenKind::LogicalAnd => Operator::LogicalAnd,
        TokenKind::LogicalOr => Operator::LogicalOr,
        TokenKind::QuestionColon => Operator::TernaryCondition,
        TokenKind::Colon => Operator::TernaryChoice,
        TokenKind::QuestionMarkQuestionMark => Operator::NullCoalescing,
        TokenKind::Name(name) if name == "as" => Operator::TypeCast,
        TokenKind::Name(name) if name == "is" => Operator::TypeTest,
        _ => {
            return Err(error!(
                "Failed to parse operator, unexpected token {:?}",
                token.kind, token.span
            ));
        }
    };

    token_stream.advance();

    Ok(ParsedOperator {
        operator,
        span: token.span,
    })
}

pub fn parse_operand_expr(
    token_stream: &TokenStream,
    allow_initializer: bool,
) -> Result<ParsedExpression> {
    let token = token_stream.peek();

    match token.kind {
        TokenKind::LParen => {
            token_stream.advance();
            let expr = parse_expr(token_stream, allow_initializer)?;
            consume_token!(token_stream, TokenKind::RParen, "parenthesized expression")?;
            parse_primary_expr(token_stream, Some(expr), allow_initializer)
        }
        TokenKind::Plus | TokenKind::Minus | TokenKind::ExclamationMark => {
            let operator = parse_operator(token_stream)?;
            let operand = parse_operand_expr(token_stream, allow_initializer)?;
            let file_id = operator.span.file_id;
            let start = operator.span.start;
            let end = operand.span().end;
            Ok(ParsedExpression::UnaryOp {
                operand: Box::new(operand),
                op: operator,
                span: Span {
                    file_id,
                    start,
                    end,
                },
            })
        }
        TokenKind::StringLiteral(ref value) => {
            token_stream.advance();
            Ok(ParsedExpression::StringLiteral {
                value: value.clone(),
                span: token.span,
            })
        }
        TokenKind::StringInterpolation(ref parts) => {
            token_stream.advance();
            let mut parsed_parts = Vec::new();

            for part in parts {
                match part {
                    StringInterpolationPart::Text(text) => {
                        parsed_parts.push(ParsedStringInterpolationPart::Text(text.clone()));
                    }
                    StringInterpolationPart::Expression(tokens) => {
                        // Create a new token stream from the expression tokens
                        let expression_token_stream = TokenStream::new(tokens.clone());

                        // Parse the expression
                        let expression = parse_expr(&expression_token_stream, true)?;
                        parsed_parts.push(ParsedStringInterpolationPart::Expression(Box::new(
                            expression,
                        )));
                    }
                }
            }

            Ok(ParsedExpression::StringInterpolation {
                parts: parsed_parts,
                span: token.span,
            })
        }
        _ => parse_primary_expr(token_stream, None, allow_initializer),
    }
}

pub fn parse_primary_expr(
    token_stream: &TokenStream,
    left: Option<ParsedExpression>,
    allow_initializer: bool,
) -> Result<ParsedExpression> {
    let token = token_stream.peek();

    if let Some(left) = left {
        match &token.kind {
            TokenKind::Dot => {
                let expr = parse_dot_expr(token_stream, left, false)?;
                parse_primary_expr(token_stream, Some(expr), allow_initializer)
            }
            TokenKind::QuestionMarkDot => {
                let expr = parse_dot_expr(token_stream, left, true)?;
                parse_primary_expr(token_stream, Some(expr), allow_initializer)
            }
            TokenKind::LSquare => {
                let expr = parse_subscript_expr(token_stream, left)?;
                parse_primary_expr(token_stream, Some(expr), allow_initializer)
            }
            TokenKind::DotDotDot => {
                let expr = parse_range(token_stream, left, allow_initializer)?;
                parse_primary_expr(token_stream, Some(expr), allow_initializer)
            }
            TokenKind::QuestionMark => {
                token_stream.advance();
                let expr = ParsedExpression::Optional {
                    value: Box::new(left),
                    span: token.span,
                };
                parse_primary_expr(token_stream, Some(expr), allow_initializer)
            }
            // TokenKind::QuestionColon => parse_ternary_expr(token_stream, left),
            TokenKind::LCurly if allow_initializer => {
                let expr = parse_initializer_expr(token_stream, left)?;
                parse_primary_expr(token_stream, Some(expr), allow_initializer)
            }
            TokenKind::LParen => {
                let expr = parse_call_expr(token_stream, left)?;
                parse_primary_expr(token_stream, Some(expr), allow_initializer)
            }
            _ => Ok(left),
        }
    } else {
        match &token.kind {
            // Float / Int
            TokenKind::Number(value) => {
                token_stream.advance();
                let expr = ParsedExpression::NumericConstant {
                    value: value.clone(),
                    span: token.span,
                };
                parse_primary_expr(token_stream, Some(expr), allow_initializer)
            }
            // String
            TokenKind::StringLiteral(ref value) => {
                token_stream.advance();
                let expr = ParsedExpression::StringLiteral {
                    value: value.clone(),
                    span: token.span,
                };
                parse_primary_expr(token_stream, Some(expr), allow_initializer)
            }
            // Bool, Initializer or Var
            TokenKind::Name(value) => {
                let expr = if matches!(value.as_str(), "true" | "false") {
                    token_stream.advance();
                    ParsedExpression::Boolean {
                        value: value == "true",
                        span: token.span,
                    }
                } else {
                    ParsedExpression::Name(parse_name(token_stream)?)
                };

                parse_primary_expr(token_stream, Some(expr), allow_initializer)
            }
            // List
            TokenKind::LSquare => {
                let expr = parse_list_literal(token_stream)?;
                parse_primary_expr(token_stream, Some(expr), allow_initializer)
            }
            _ => {
                return Err(error!(
                    "Failed to parse primary expression, unmatched token {:?}",
                    token.kind, token.span
                ));
            }
        }
    }
}

pub fn parse_call_expr(
    token_stream: &TokenStream,
    operand: ParsedExpression,
) -> Result<ParsedExpression> {
    let _ = consume_token!(token_stream, TokenKind::LParen, "call expression")?;
    let _ = consume_token!(token_stream, TokenKind::Newline, "call expression");

    let mut args = Vec::new();

    loop {
        let token = token_stream.peek();

        match &token.kind {
            TokenKind::RParen => {
                token_stream.advance();
                let file_id = operand.span().file_id;
                let start = operand.span().start;
                return Ok(ParsedExpression::Call {
                    operand: Box::new(operand),
                    values: args,
                    span: Span {
                        file_id,
                        start,
                        end: token.span.end,
                    },
                });
            }
            _ => {
                let arg = parse_expr(token_stream, true)?;

                args.push(arg);

                parse_list_seperator(token_stream, TokenKind::RParen, "call expression")?;
            }
        }
    }
}

pub fn parse_initializer_expr(
    token_stream: &TokenStream,
    type_expr: ParsedExpression,
) -> Result<ParsedExpression> {
    let _ = consume_token!(token_stream, TokenKind::LCurly, "initialization expression")?;
    let _ = consume_token!(
        token_stream,
        TokenKind::Newline,
        "initialization expression"
    );

    let mut values = Vec::new();

    loop {
        let token = token_stream.peek();

        match &token.kind {
            TokenKind::RCurly => {
                token_stream.advance();
                let file_id = type_expr.span().file_id;
                let start = type_expr.span().start;
                return Ok(ParsedExpression::Initializer {
                    typ: Box::new(type_expr),
                    values,
                    span: Span {
                        file_id,
                        start,
                        end: token.span.end,
                    },
                });
            }
            TokenKind::Name(_) => {
                let field_name = parse_name(token_stream)?;

                // FIXME: We don't get good diagnostics because both cases look exactly the same to the checker.
                if let Ok(_) =
                    consume_token!(token_stream, TokenKind::Equal, "initialization expression")
                {
                    let expr = parse_expr(token_stream, true)?;
                    values.push((field_name, expr));
                } else {
                    values.push((field_name.clone(), ParsedExpression::Name(field_name)));
                }

                parse_list_seperator(token_stream, TokenKind::RCurly, "initializer expression")?;
            }
            _ => {
                return Err(error!(
                    "Failed to parse initialization expression, unexpected token {:?}",
                    token.kind, token.span
                ));
            }
        }
    }
}

pub fn parse_dot_expr(
    token_stream: &TokenStream,
    operand: ParsedExpression,
    optional: bool,
) -> Result<ParsedExpression> {
    let operand_span = operand.span();
    if optional {
        let _ = consume_token!(
            token_stream,
            TokenKind::QuestionMarkDot,
            "indexed struct expression"
        )?;
    } else {
        let _ = consume_token!(token_stream, TokenKind::Dot, "indexed struct expression")?;
    }
    let field_name = parse_name(token_stream)?;
    let field_name_span = field_name.span;
    Ok(ParsedExpression::DotExpr {
        operand: Box::new(operand),
        name: field_name,
        optional,
        span: Span {
            file_id: operand_span.file_id,
            start: operand_span.start,
            end: field_name_span.end,
        },
    })
}

fn parse_subscript_expr(
    token_stream: &TokenStream,
    operand: ParsedExpression,
) -> Result<ParsedExpression> {
    let operand_span = operand.span();
    let _ = consume_token!(token_stream, TokenKind::LSquare, "subscript expression")?;
    let mut subscripts = Vec::new();
    loop {
        subscripts.push(parse_expr(token_stream, true)?);
        let token = token_stream.peek();
        match &token.kind {
            TokenKind::RSquare => {
                break;
            }
            _ => {}
        }

        let _ = consume_token!(token_stream, TokenKind::Comma, "subscript expression")?;
    }
    let end_span = consume_token!(token_stream, TokenKind::RSquare, "subscript expression")
        .expect("we would not have exited the loop if this failed");

    Ok(ParsedExpression::SubscriptExpression {
        operand: Box::new(operand),
        subscripts,
        span: Span {
            file_id: operand_span.file_id,
            start: operand_span.start,
            end: end_span.end,
        },
    })
}

fn parse_range(
    token_stream: &TokenStream,
    start: ParsedExpression,
    allow_initializer: bool,
) -> Result<ParsedExpression> {
    let start_span = start.span();
    let _ = consume_token!(token_stream, TokenKind::DotDotDot, "range expression")?;
    let end = parse_expr(token_stream, allow_initializer)?;
    let end_span = end.span();
    Ok(ParsedExpression::Range {
        start: Box::new(start),
        end: Box::new(end),
        span: Span {
            file_id: start_span.file_id,
            start: start_span.start,
            end: end_span.end,
        },
    })
}

enum PrecendenceStackElement {
    Operand(ParsedExpression),
    Operator(ParsedOperator),
}

impl PrecendenceStackElement {
    fn operand(self) -> ParsedExpression {
        match self {
            Self::Operand(operand) => operand,
            _ => {
                panic!("not an operand")
            }
        }
    }
    fn operator(self) -> ParsedOperator {
        match self {
            Self::Operator(operator) => operator,
            _ => {
                panic!("not an operator")
            }
        }
    }
    fn operator_ref(&self) -> &ParsedOperator {
        match self {
            Self::Operator(operator) => operator,
            _ => {
                panic!("not an operator")
            }
        }
    }
}

const TERNARY_OPERATOR_PRECEDENCE: usize = 1;

fn reduce_expressions(
    stack: &mut Vec<PrecendenceStackElement>,
    precedence: usize,
    last_precedence: &mut usize,
) {
    while precedence <= *last_precedence && stack.len() > 2 {
        // The ternary operators :? and : have the same precedence, to achieve right associativity
        // e.g. expr0 ?: (expr4 : (expr5 ?: (expr8 : expr9))) we do not want to reduce.
        //
        // If did reduce, we would end up with a left associative expression
        // like (((expr0 ?: expr4) : expr5) ?: expr8) : expr9
        // which is not what we want for the ternary operator.
        if precedence == *last_precedence && precedence == TERNARY_OPERATOR_PRECEDENCE {
            break;
        }

        let rhs = stack.pop().unwrap().operand();
        let op = stack.pop().unwrap().operator();
        let lhs = stack.pop().unwrap().operand();

        let span = Span {
            file_id: lhs.span().file_id,
            start: lhs.span().start,
            end: rhs.span().end,
        };
        let binop = PrecendenceStackElement::Operand(ParsedExpression::BinaryOp {
            lhs: Box::new(lhs),
            op,
            rhs: Box::new(rhs),
            span,
        });

        *last_precedence = if stack.len() > 0 {
            stack.last().unwrap().operator_ref().precedence()
        } else {
            std::usize::MAX
        };

        stack.push(binop);
    }
}

pub fn parse_expr(token_stream: &TokenStream, allow_initializer: bool) -> Result<ParsedExpression> {
    let mut stack = Vec::new();
    let mut last_precedence = 0;
    let span = token_stream.current_span();

    loop {
        match parse_operand_expr(token_stream, allow_initializer) {
            Err(err) => {
                if stack.is_empty() {
                    return Err(err);
                } else {
                    break;
                }
            }
            Ok(operand) => stack.push(PrecendenceStackElement::Operand(operand)),
        }

        let operator = match parse_operator(token_stream) {
            Err(_) => break,
            Ok(operator) => operator,
        };

        let precedence = operator.precedence();
        reduce_expressions(&mut stack, precedence, &mut last_precedence);
        stack.push(PrecendenceStackElement::Operator(operator));
        last_precedence = precedence;
    }

    if stack.len() & 1 == 1 {
        reduce_expressions(&mut stack, 0, &mut last_precedence);
    }

    if stack.len() != 1 {
        Err(error!("Failed to parse expression.", span))
    } else {
        Ok(stack.pop().unwrap().operand())
    }
}

pub fn parse_statement(token_stream: &TokenStream) -> Result<ParsedStatement> {
    let token = token_stream.peek();

    match &token.kind {
        TokenKind::Name(name) if name == "from" => parse_from_import(token_stream),
        TokenKind::Name(name) if name == "import" => parse_import(token_stream),
        TokenKind::Name(name) if name == "struct" => parse_struct(token_stream),
        TokenKind::Name(name) if name == "template" => parse_template(token_stream),
        TokenKind::Name(name) if name == "oneof" => parse_oneof(token_stream),
        TokenKind::Name(name) if name == "func" => parse_func(token_stream),
        _ => parse_const(token_stream),
    }
}

pub fn parse_name(token_stream: &TokenStream) -> Result<ParsedName> {
    let token = token_stream.peek();

    if let TokenKind::Name(name) = &token.kind {
        let span = token.span.clone();
        token_stream.advance();
        Ok(ParsedName {
            value: name.clone(),
            span,
        })
    } else {
        Err(Error::ParserError(
            format!("Expected a name but found {:?}", token.kind),
            token.span.clone(),
        ))
    }
}

fn parse_list_seperator(
    token_stream: &TokenStream,
    list_terminator: TokenKind,
    context: &str,
) -> Result<()> {
    if let Err(_) = consume_token!(token_stream, TokenKind::Comma, context) {
        if let Err(_) = consume_token!(token_stream, TokenKind::Newline, context) {
            if token_stream.peek().kind != list_terminator {
                return Err(error!(
                    "Expected comma or newline in {}.",
                    context,
                    token_stream.current_span()
                ));
            }
        }
    } else {
        // consume optional newline
        let _ = consume_token!(token_stream, TokenKind::Newline, context);
    }

    Ok(())
}

pub fn parse_struct(token_stream: &TokenStream) -> Result<ParsedStatement> {
    let start = token_stream.consume_name("struct", "struct declaration")?;
    let name = parse_name(token_stream)?;
    let _ = consume_token!(token_stream, TokenKind::LCurly, "struct declaration")?;
    let _ = consume_token!(token_stream, TokenKind::Newline, "struct declaration");
    let mut fields = Vec::new();

    loop {
        let token = token_stream.peek();

        match &token.kind {
            TokenKind::RCurly => {
                token_stream.advance();
                let _ = consume_token!(token_stream, TokenKind::Newline, "struct declaration");
                return Ok(ParsedStatement::Struct(ParsedStruct {
                    name,
                    fields,
                    span: Span {
                        file_id: start.file_id,
                        start: start.start,
                        end: token.span.end,
                    },
                }));
            }
            TokenKind::Name(_) => {
                let field_name = parse_name(token_stream)?;
                let _ = consume_token!(token_stream, TokenKind::Colon, "struct declaration")?;
                let typ = parse_expr(token_stream, true)?;

                let default = match token_stream.peek().kind {
                    TokenKind::Equal => {
                        token_stream.advance();
                        Some(parse_expr(token_stream, true)?)
                    }
                    _ => None,
                };

                fields.push((field_name, typ, default));
                parse_list_seperator(token_stream, TokenKind::RCurly, "struct declaration")?;
            }
            _ => {
                return Err(error!(
                    "Failed to parse struct, unexpected token {:?}",
                    &token.kind, token.span
                ));
            }
        }
    }
}

fn parse_template(token_stream: &TokenStream) -> Result<ParsedStatement> {
    let start_span = token_stream.consume_name("template", "template definition")?;
    let name = parse_name(token_stream)?;

    let _ = consume_token!(token_stream, TokenKind::LCurly, "template definition")?;
    let _ = consume_token!(token_stream, TokenKind::Newline, "template definition");
    let mut fields = Vec::new();

    loop {
        let token = token_stream.peek();

        match &token.kind {
            TokenKind::RCurly => {
                token_stream.advance();
                break;
            }
            TokenKind::Name(_) => {
                let field_name = parse_name(token_stream)?;
                let _ = consume_token!(token_stream, TokenKind::Colon, "template definition")?;
                let typ = parse_expr(token_stream, true)?;
                fields.push((field_name, typ));
                parse_list_seperator(token_stream, TokenKind::RCurly, "template definition")?;
            }
            _ => {
                return Err(error!(
                    "Failed to parse template, unexpected token {:?}",
                    &token.kind, token.span
                ));
            }
        }
    }

    let _ = consume_token!(token_stream, TokenKind::Equal, "template definition")?;
    let expr = parse_expr(token_stream, true)?;
    let end_span = expr.span();
    let _ = consume_token!(token_stream, TokenKind::Newline, "template definition");
    return Ok(ParsedStatement::Template(ParsedTemplate {
        name,
        fields,
        expr,
        span: Span {
            file_id: start_span.file_id,
            start: start_span.start,
            end: end_span.end,
        },
    }));
}

fn parse_func(token_stream: &TokenStream) -> Result<ParsedStatement> {
    let start_span = token_stream.consume_name("func", "function definition")?;
    let name = parse_name(token_stream)?;

    let _ = consume_token!(token_stream, TokenKind::LParen, "function definition")?;
    let _ = consume_token!(token_stream, TokenKind::Newline, "function definition");
    let mut pars = Vec::new();

    loop {
        let token = token_stream.peek();

        match &token.kind {
            TokenKind::RParen => {
                token_stream.advance();
                break;
            }
            TokenKind::Name(_) => {
                let field_name = parse_name(token_stream)?;
                let _ = consume_token!(token_stream, TokenKind::Colon, "function definition")?;
                let typ = parse_expr(token_stream, true)?;
                pars.push((field_name, typ));
                parse_list_seperator(token_stream, TokenKind::RParen, "func definition")?;
            }
            _ => {
                return Err(error!(
                    "Failed to parse function, unexpected token {:?}",
                    &token.kind, token.span
                ));
            }
        }
    }

    let _ = consume_token!(token_stream, TokenKind::Equal, "function definition")?;
    let expr = parse_expr(token_stream, true)?;
    let end_span = expr.span();
    let _ = consume_token!(token_stream, TokenKind::Newline, "function definition");
    return Ok(ParsedStatement::Function(ParsedFunction {
        name,
        pars,
        expr,
        span: Span {
            file_id: start_span.file_id,
            start: start_span.start,
            end: end_span.end,
        },
    }));
}

fn parse_oneof(token_stream: &TokenStream) -> Result<ParsedStatement> {
    let start_span = token_stream.consume_name("oneof", "oneof declaration")?;
    let name = parse_name(token_stream)?;

    let _ = consume_token!(token_stream, TokenKind::LCurly, "oneof declaration")?;
    let _ = consume_token!(token_stream, TokenKind::Newline, "oneof declaration");
    let mut types = Vec::new();

    loop {
        let token = token_stream.peek();
        match token.kind {
            TokenKind::RCurly => {
                token_stream.advance();
                let _ = consume_token!(token_stream, TokenKind::Newline, "oneof declaration");
                return Ok(ParsedStatement::Oneof(ParsedOneof {
                    name,
                    types,
                    span: Span {
                        file_id: start_span.file_id,
                        start: start_span.start,
                        end: token.span.end,
                    },
                }));
            }
            _ => {}
        }
        let typ = parse_expr(token_stream, true)?;
        parse_list_seperator(token_stream, TokenKind::RCurly, "oneof definition")?;
        types.push(typ);
    }
}

fn parse_const(token_stream: &TokenStream) -> Result<ParsedStatement> {
    let name = parse_name(token_stream)?;
    let start_span = name.span;

    let typ = if let Ok(_) = consume_token!(token_stream, TokenKind::Colon, "constant declaration")
    {
        Some(parse_expr(token_stream, true)?)
    } else {
        None
    };

    let _ = consume_token!(token_stream, TokenKind::Equal, "constant declaration")?;
    let value = parse_expr(token_stream, true)?;
    let _ = consume_token!(token_stream, TokenKind::Newline, "constant declaration");
    let end_span = value.span();

    Ok(ParsedStatement::Const(ParsedConst {
        name,
        typ,
        value,
        span: Span {
            file_id: start_span.file_id,
            start: start_span.start,
            end: end_span.end,
        },
    }))
}

fn parse_import(token_stream: &TokenStream) -> Result<ParsedStatement> {
    let start = token_stream.consume_name("import", "import declaration")?;
    let (path, end) = consume_token!(
        token_stream,
        TokenKind::StringLiteral(path),
        "import declaration"
    )?;
    let _ = consume_token!(token_stream, TokenKind::Newline, "import declaration");
    Ok(ParsedStatement::Import(ParsedImport {
        path,
        span: Span {
            file_id: start.file_id,
            start: start.start,
            end: end.end,
        },
    }))
}

fn parse_from_import(token_stream: &TokenStream) -> Result<ParsedStatement> {
    let start = token_stream.consume_name("from", "import declaration")?;
    let (path, end) = consume_token!(
        token_stream,
        TokenKind::StringLiteral(path),
        "import declaration"
    )?;
    let _ = token_stream.consume_name("import", "import declaration")?;
    let _ = consume_token!(token_stream, TokenKind::LCurly, "import declaration")?;
    let _ = consume_token!(token_stream, TokenKind::Newline, "import declaration");

    let mut names = Vec::new();
    loop {
        let token = token_stream.peek();

        match &token.kind {
            TokenKind::RCurly => {
                token_stream.advance();
                break;
            }
            TokenKind::Name(_) => {
                names.push(parse_name(token_stream)?);
                parse_list_seperator(token_stream, TokenKind::RCurly, "import")?;
            }
            _ => {
                return Err(error!(
                    "Failed to parse import, unmatched token {:?}",
                    &token.kind, token.span
                ));
            }
        }
    }

    let _ = consume_token!(token_stream, TokenKind::Newline, "import declaration");
    Ok(ParsedStatement::FromImport(ParsedFromImport {
        path,
        names,
        span: Span {
            file_id: start.file_id,
            start: start.start,
            end: end.end,
        },
    }))
}

fn parse_for_element(token_stream: &TokenStream) -> Result<ParsedForElement> {
    let for_span = token_stream.consume_name("for", "for loop")?;
    let var = parse_name(token_stream)?;
    let _ = token_stream.consume_name("in", "for loop")?;
    let iterable = parse_expr(token_stream, false)?;
    let _ = consume_token!(token_stream, TokenKind::LCurly, "for loop")?;
    let _ = consume_token!(token_stream, TokenKind::Newline, "for loop");

    let values = parse_list(token_stream)?;

    let rcurly_span = consume_token!(token_stream, TokenKind::RCurly, "for loop")?;

    Ok(ParsedForElement {
        var,
        iterable,
        body: values,
        span: Span {
            file_id: for_span.file_id,
            start: for_span.start,
            end: rcurly_span.end,
        },
    })
}

fn parse_if_element(token_stream: &TokenStream) -> Result<ParsedIfElement> {
    let if_span = token_stream.consume_name("if", "if statement")?;
    let condition = parse_expr(token_stream, false)?;

    let _ = consume_token!(token_stream, TokenKind::LCurly, "if statement")?;
    let _ = consume_token!(token_stream, TokenKind::Newline, "if statement");

    let values = parse_list(token_stream)?;

    let rcurly_span = consume_token!(token_stream, TokenKind::RCurly, "if statement")?;

    Ok(ParsedIfElement {
        condition,
        body: values,
        span: Span {
            file_id: if_span.file_id,
            start: if_span.start,
            end: rcurly_span.end,
        },
    })
}

fn parse_list_element(token_stream: &TokenStream) -> Result<ParsedListElement> {
    let token = token_stream.peek();
    let el = match &token.kind {
        TokenKind::Name(name) if name == "for" => {
            ParsedListElement::ForElement(parse_for_element(token_stream)?)
        }
        TokenKind::Name(name) if name == "if" => {
            ParsedListElement::IfElement(parse_if_element(token_stream)?)
        }
        _ => ParsedListElement::Expr(parse_expr(token_stream, true)?),
    };
    Ok(el)
}

fn parse_list(token_stream: &TokenStream) -> Result<Vec<ParsedListElement>> {
    let mut values = Vec::new();

    loop {
        let token = token_stream.peek();

        match &token.kind {
            TokenKind::RCurly | TokenKind::RSquare => {
                return Ok(values);
            }
            _ => {}
        }

        values.push(parse_list_element(token_stream)?);

        parse_list_seperator(token_stream, TokenKind::RSquare, "list element")?;
    }
}

fn parse_list_literal(token_stream: &TokenStream) -> Result<ParsedExpression> {
    let lsquare_span = consume_token!(token_stream, TokenKind::LSquare, "list literal")?;
    let _ = consume_token!(token_stream, TokenKind::Newline, "list literal");
    let values = parse_list(token_stream)?;
    let end = consume_token!(token_stream, TokenKind::RSquare, "list literal")?;
    Ok(ParsedExpression::ListLiteral {
        values,
        span: Span {
            file_id: lsquare_span.file_id,
            start: lsquare_span.start,
            end: end.end,
        },
    })
}

pub fn parse_module(token_stream: &TokenStream) -> Result<ParsedModule> {
    let mut statements = Vec::new();

    loop {
        let token = token_stream.peek();

        match &token.kind {
            TokenKind::Eof => {
                token_stream.advance();
                return Ok(ParsedModule {
                    statements,
                    span: Span {
                        file_id: token.span.file_id,
                        start: 0,
                        end: token.span.end,
                    },
                });
            }
            _ => {
                let statement = parse_statement(token_stream)?;
                statements.push(statement);
            }
        }
    }
}
