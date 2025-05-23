use crate::common::{Error, FileId, Result, Span};

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    fn new(kind: TokenKind, file_id: FileId, start: usize, end: usize) -> Self {
        Token {
            kind: kind,
            span: Span {
                file_id,
                start,
                end,
            },
        }
    }

    fn is_newline(&self) -> bool {
        matches!(self.kind, TokenKind::Newline)
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum NumericConstant {
    Float(f64),
    Int(i64),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    StringLiteral(String),
    StringInterpolation(Vec<StringInterpolationPart>),
    Number(NumericConstant),
    Name(String),
    Colon,
    LParen,
    RParen,
    LCurly,
    RCurly,
    LSquare,
    RSquare,
    PercentSign,
    Plus,
    Minus,
    Equal,
    NotEqual,
    DoubleEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Asterisk,
    ForwardSlash,
    ExclamationMark,
    QuestionMark,
    QuestionMarkDot,
    QuestionMarkQuestionMark,
    QuestionColon,
    Comma,
    Comment,
    Dot,
    DotDotDot,
    Eof,
    LogicalAnd,
    LogicalOr,
    Newline,

    Garbage,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StringInterpolationPart {
    Text(String),
    Expression(Vec<Token>),
}

struct Chars {
    chars: Vec<char>,
    index: usize,
}

impl Chars {
    fn peek(&self, offset: usize) -> Option<char> {
        if self.index + offset >= self.chars.len() {
            return None;
        }
        Some(self.chars[self.index + offset])
    }
    fn next(&mut self) -> Option<(usize, char)> {
        if self.index >= self.chars.len() {
            return None;
        }
        let ch = self.chars[self.index];
        self.index += 1;
        Some((self.index - 1, ch))
    }
}

pub fn lex(file_id: FileId, text: &str) -> Result<Vec<Token>> {
    let mut tokens: Vec<Token> = Vec::new();

    let mut chars = Chars {
        chars: text.chars().collect(),
        index: 0,
    };

    let mut inside_comment = false;

    while let Some((idx, ch)) = chars.next() {
        let start = idx;

        // If inside a comment, ignore all characters until the end of the line.
        if inside_comment {
            match ch {
                '\r' | '\n' => {
                    inside_comment = false;
                    continue;
                }
                _ => continue,
            }
        }

        match ch {
            '\r' | '\n' => {
                // Ignore Newlines at the start of a file or consecutive Newlines
                if let Some(token) = tokens.last() {
                    if token.is_newline() {
                        continue;
                    }
                }
                tokens.push(Token::new(TokenKind::Newline, file_id, start, start + 1));
            }
            _ => {}
        }

        match ch {
            // Whitespace: skip it
            ' ' | '\t' | '\r' | '\n' => continue,

            // Single-character tokens
            ',' => tokens.push(Token::new(TokenKind::Comma, file_id, start, start + 1)),
            ':' => tokens.push(Token::new(TokenKind::Colon, file_id, start, start + 1)),
            '(' => tokens.push(Token::new(TokenKind::LParen, file_id, start, start + 1)),
            ')' => tokens.push(Token::new(TokenKind::RParen, file_id, start, start + 1)),
            '{' => tokens.push(Token::new(TokenKind::LCurly, file_id, start, start + 1)),
            '}' => tokens.push(Token::new(TokenKind::RCurly, file_id, start, start + 1)),
            '[' => tokens.push(Token::new(TokenKind::LSquare, file_id, start, start + 1)),
            ']' => tokens.push(Token::new(TokenKind::RSquare, file_id, start, start + 1)),
            '%' => tokens.push(Token::new(
                TokenKind::PercentSign,
                file_id,
                start,
                start + 1,
            )),
            '+' => tokens.push(Token::new(TokenKind::Plus, file_id, start, start + 1)),
            '-' => tokens.push(Token::new(TokenKind::Minus, file_id, start, start + 1)),
            '*' => tokens.push(Token::new(TokenKind::Asterisk, file_id, start, start + 1)),
            '/' => {
                if let Some('/') = chars.peek(0) {
                    chars.next();
                    inside_comment = true;
                } else {
                    tokens.push(Token::new(
                        TokenKind::ForwardSlash,
                        file_id,
                        start,
                        start + 1,
                    ));
                }
            }
            '.' => {
                if let Some('.') = chars.peek(0) {
                    chars.next();
                    tokens.push(Token::new(TokenKind::DotDotDot, file_id, start, start + 2));
                } else {
                    tokens.push(Token::new(TokenKind::Dot, file_id, start, start + 1));
                }
            }
            '!' => {
                if let Some('=') = chars.peek(0) {
                    chars.next();
                    tokens.push(Token::new(TokenKind::NotEqual, file_id, start, start + 2));
                } else {
                    tokens.push(Token::new(
                        TokenKind::ExclamationMark,
                        file_id,
                        start,
                        start + 1,
                    ));
                }
            }
            '=' => {
                if let Some('=') = chars.peek(0) {
                    chars.next();
                    tokens.push(Token::new(
                        TokenKind::DoubleEqual,
                        file_id,
                        start,
                        start + 2,
                    ));
                } else {
                    tokens.push(Token::new(TokenKind::Equal, file_id, start, start + 1));
                }
            }
            '<' => {
                if let Some('=') = chars.peek(0) {
                    chars.next();
                    tokens.push(Token::new(
                        TokenKind::LessThanOrEqual,
                        file_id,
                        start,
                        start + 2,
                    ));
                } else {
                    tokens.push(Token::new(TokenKind::LessThan, file_id, start, start + 1));
                }
            }
            '>' => {
                if let Some('=') = chars.peek(0) {
                    chars.next();
                    tokens.push(Token::new(
                        TokenKind::GreaterThanOrEqual,
                        file_id,
                        start,
                        start + 2,
                    ));
                } else {
                    tokens.push(Token::new(
                        TokenKind::GreaterThan,
                        file_id,
                        start,
                        start + 1,
                    ));
                }
            }
            '&' if matches!(chars.peek(0), Some('&')) => {
                chars.next();
                tokens.push(Token::new(TokenKind::LogicalAnd, file_id, start, start + 2));
            }
            '|' if matches!(chars.peek(0), Some('|')) => {
                chars.next();
                tokens.push(Token::new(TokenKind::LogicalOr, file_id, start, start + 2));
            }
            '?' => {
                if let Some('?') = chars.peek(0) {
                    chars.next();
                    tokens.push(Token::new(
                        TokenKind::QuestionMarkQuestionMark,
                        file_id,
                        start,
                        start + 2,
                    ));
                } else if let Some('.') = chars.peek(0) {
                    chars.next();
                    tokens.push(Token::new(
                        TokenKind::QuestionMarkDot,
                        file_id,
                        start,
                        start + 2,
                    ));
                } else if let Some(':') = chars.peek(0) {
                    chars.next();
                    tokens.push(Token::new(
                        TokenKind::QuestionColon,
                        file_id,
                        start,
                        start + 2,
                    ));
                } else {
                    tokens.push(Token::new(
                        TokenKind::QuestionMark,
                        file_id,
                        start,
                        start + 1,
                    ));
                }
            }
            // Numbers
            '0'..='9' => {
                let (end, number_token) = lex_number(file_id, idx, ch, &mut chars)?;
                tokens.push(Token {
                    kind: number_token,
                    span: Span {
                        file_id: file_id,
                        start,
                        end,
                    },
                });
            }

            // String Interpolation
            '$' if matches!(chars.peek(0), Some('"')) => {
                chars.next(); // Consume the quote character
                let (end, string_token) = lex_string_interpolation(file_id, idx, &mut chars)?;
                tokens.push(Token {
                    kind: string_token,
                    span: Span {
                        file_id: file_id,
                        start,
                        end,
                    },
                });
            }

            // Regular Strings
            '"' => {
                let (end, string_token) = lex_string(file_id, idx, ch, &mut chars)?;
                tokens.push(Token {
                    kind: string_token,
                    span: Span {
                        file_id: file_id,
                        start,
                        end,
                    },
                });
            }

            // Identifiers/Names
            'a'..='z' | 'A'..='Z' | '_' => {
                let (end, name) = lex_name(idx, ch, &mut chars);
                tokens.push(Token {
                    kind: TokenKind::Name(name),
                    span: Span {
                        file_id: file_id,
                        start,
                        end,
                    },
                });
            }

            // Unknown character
            _ => {
                return Err(Error::LexerError(
                    format!("Unexpected character '{}'", ch),
                    Span {
                        file_id: file_id,
                        start,
                        end: start + 1,
                    },
                ));
            }
        }
    }

    tokens.push(Token {
        kind: TokenKind::Eof,
        span: Span {
            file_id: file_id,
            start: text.len(),
            end: text.len(),
        },
    });

    Ok(tokens)
}

fn lex_number(
    file_id: FileId,
    start_idx: usize,
    first_char: char,
    chars: &mut Chars,
) -> Result<(usize, TokenKind)> {
    let mut end = start_idx;
    let mut number_str = first_char.to_string();
    let mut is_float = false;
    while let Some(ch) = chars.peek(0) {
        match ch {
            '0'..='9' => {
                number_str.push(ch);
                end += 1;
                chars.next();
            }
            '.' => {
                if let Some(next_ch) = chars.peek(1) {
                    if next_ch.is_ascii_digit() {
                        number_str.push('.');
                        number_str.push(next_ch);
                        end += 2;
                        chars.next();
                        chars.next();
                        is_float = true;
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }
            _ => break,
        }
    }

    if is_float {
        number_str
            .parse::<f64>()
            .map(|n| (end + 1, TokenKind::Number(NumericConstant::Float(n))))
            .map_err(|_| {
                Error::LexerError(
                    "Failed to parse float".to_string(),
                    Span {
                        file_id: file_id,
                        start: start_idx,
                        end,
                    },
                )
            })
    } else {
        number_str
            .parse::<i64>()
            .map(|n| (end + 1, TokenKind::Number(NumericConstant::Int(n))))
            .map_err(|_| {
                Error::LexerError(
                    format!("Failed to parse integer '{}'", number_str),
                    Span {
                        file_id: file_id,
                        start: start_idx,
                        end,
                    },
                )
            })
    }
}

fn replace_escaped_characetrs(mut s: String) -> String {
    let replacements = [
        ("\\n", "\n"),
        ("\\t", "\t"),
        ("\\r", "\r"),
        ("\\\"", "\""),
        ("\\\\", "\\"),
        ("\\'", "'"),
    ];
    for (escaped, replacement) in &replacements {
        s = s.replace(escaped, replacement);
    }
    s
}

fn lex_string(
    file_id: FileId,
    start_idx: usize,
    quote_char: char,
    chars: &mut Chars,
) -> Result<(usize, TokenKind)> {
    let mut end = start_idx;
    let mut string_content = String::new();

    while let Some((idx, ch)) = chars.next() {
        end = idx;
        if ch == quote_char {
            return Ok((
                end + 1,
                TokenKind::StringLiteral(replace_escaped_characetrs(string_content)),
            ));
        } else {
            string_content.push(ch);
        }
    }

    // If we get here, the string was not terminated
    Err(Error::LexerError(
        "Unterminated string literal".to_string(),
        Span {
            file_id: file_id,
            start: start_idx,
            end,
        },
    ))
}

fn lex_name(start_idx: usize, first_char: char, chars: &mut Chars) -> (usize, String) {
    let mut end = start_idx;
    let mut name = first_char.to_string();

    while let Some(ch) = chars.peek(0) {
        if ch.is_alphanumeric() || ch == '_' {
            name.push(ch);
            end += 1;
            chars.next();
        } else {
            break;
        }
    }

    (end + 1, name)
}

fn lex_string_interpolation(
    file_id: FileId,
    start_idx: usize,
    chars: &mut Chars,
) -> Result<(usize, TokenKind)> {
    let mut end = start_idx;
    let mut parts = Vec::new();
    let mut current_text = String::new();
    let mut in_expression = false;
    let mut brace_count = 0;

    while let Some((idx, ch)) = chars.next() {
        end = idx;

        if in_expression {
            match ch {
                '{' => {
                    // Track nested braces within expressions
                    brace_count += 1;
                    current_text.push(ch);
                }
                '}' => {
                    if brace_count == 0 {
                        // End of the embedded expression
                        in_expression = false;

                        // Parse the expression content as tokens
                        let expr_result = lex(file_id, &current_text)?;
                        parts.push(StringInterpolationPart::Expression(expr_result));
                        current_text = String::new();
                    } else {
                        // Just a nested closing brace
                        brace_count -= 1;
                        current_text.push(ch);
                    }
                }
                _ => {
                    // Add to the expression content
                    current_text.push(ch);
                }
            }
        } else {
            // Not in an expression
            match ch {
                '"' => {
                    // End of the interpolated string
                    if !current_text.is_empty() {
                        parts.push(StringInterpolationPart::Text(replace_escaped_characetrs(
                            current_text,
                        )));
                    }
                    return Ok((end + 1, TokenKind::StringInterpolation(parts)));
                }
                '{' => {
                    // Start of an embedded expression
                    if !current_text.is_empty() {
                        parts.push(StringInterpolationPart::Text(replace_escaped_characetrs(
                            current_text,
                        )));
                        current_text = String::new();
                    }
                    in_expression = true;
                }
                _ => {
                    // Regular text
                    current_text.push(ch);
                }
            }
        }
    }

    Err(Error::LexerError(
        "Unterminated string interpolation".to_string(),
        Span {
            file_id: file_id,
            start: start_idx,
            end,
        },
    ))
}
