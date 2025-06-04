pub type FileId = usize;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Span {
    pub file_id: FileId,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(file_id: FileId, start: usize, end: usize) -> Self {
        Span {
            file_id,
            start,
            end,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Error {
    LexerError(String, Span),
    ParserError(String, Span),
    CheckerError(String, Span),
    EvaluationError(EvaluationError),
    CompilerError(String),
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, PartialEq)]
pub enum EvaluationError {
    DivisionByZero,
    InvalidOperation(String),
    IndexOutOfRange,
    ItemNotFound(String),
    TypeCastError(String),
    FunctionNotFound(String),
}
