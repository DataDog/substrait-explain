use std::fmt;

use thiserror::Error;

use super::extensions::ExtensionParseError;
use crate::extensions::registry::ExtensionError;
use crate::extensions::simple::MissingReference;

/// Context for parse errors and warnings
#[derive(Debug, Clone)]
pub struct ParseContext {
    pub line_no: i64,
    pub line: String,
}

impl ParseContext {
    pub fn new(line_no: i64, line: String) -> Self {
        Self { line_no, line }
    }
}

impl std::fmt::Display for ParseContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}: '{}'", self.line_no, self.line)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TextSpan {
    pub start: usize,
    pub end: usize,
}

impl TextSpan {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SyntaxErrorKind {
    InvalidToken,
    UnexpectedEof,
    UnexpectedToken { found: String },
    ExtraToken { found: String },
    User { message: String },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxErrorDetail {
    pub kind: SyntaxErrorKind,
    pub span: Option<TextSpan>,
    pub expected: Vec<String>,
}

impl SyntaxErrorDetail {
    pub fn new(kind: SyntaxErrorKind, span: Option<TextSpan>, expected: Vec<String>) -> Self {
        Self {
            kind,
            span,
            expected,
        }
    }

    pub fn describe_with_line(&self, line: &str) -> String {
        let mut message = self.to_string();
        if let Some(column) = self.column(line) {
            message.push_str(&format!(" at column {column}"));
            if let Some(caret) = self.caret_line(line) {
                message.push('\n');
                message.push_str(line);
                message.push('\n');
                message.push_str(&caret);
            }
        }
        message
    }

    fn column(&self, line: &str) -> Option<usize> {
        let span = self.span?;
        let idx = clamp_to_char_boundary(line, span.start);
        Some(line[..idx].chars().count() + 1)
    }

    fn caret_line(&self, line: &str) -> Option<String> {
        let span = self.span?;
        let idx = clamp_to_char_boundary(line, span.start);
        let col0 = line[..idx].chars().count();
        Some(format!("{}^", " ".repeat(col0)))
    }
}

impl fmt::Display for SyntaxErrorDetail {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            SyntaxErrorKind::InvalidToken => write!(f, "invalid token")?,
            SyntaxErrorKind::UnexpectedEof => write!(f, "unexpected end of input")?,
            SyntaxErrorKind::UnexpectedToken { found } => write!(f, "unexpected token '{found}'")?,
            SyntaxErrorKind::ExtraToken { found } => write!(f, "extra token '{found}'")?,
            SyntaxErrorKind::User { message } => write!(f, "{message}")?,
        }

        if !self.expected.is_empty() {
            write!(f, "; expected {}", Self::format_expected(&self.expected))?;
        }

        Ok(())
    }
}

impl SyntaxErrorDetail {
    fn format_expected(expected: &[String]) -> String {
        const MAX_ITEMS: usize = 6;
        if expected.len() <= MAX_ITEMS {
            expected.join(", ")
        } else {
            let mut display = expected[..MAX_ITEMS].join(", ");
            display.push_str(", ...");
            display
        }
    }
}

fn clamp_to_char_boundary(line: &str, mut idx: usize) -> usize {
    idx = idx.min(line.len());
    while idx > 0 && !line.is_char_boundary(idx) {
        idx -= 1;
    }
    idx
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    Syntax,
    InvalidValue,
    Lookup(MissingReference),
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::Syntax => write!(f, "Syntax"),
            ErrorKind::InvalidValue => write!(f, "Invalid value"),
            ErrorKind::Lookup(e) => write!(f, "Invalid reference ({e})"),
        }
    }
}

#[derive(Error, Debug, Clone)]
#[error("{kind} Error parsing {message}: {description}")]
pub struct MessageParseError {
    pub message: &'static str,
    pub kind: ErrorKind,
    pub description: String,
}

impl MessageParseError {
    pub fn syntax(message: &'static str, description: impl ToString) -> Self {
        Self::new(message, ErrorKind::Syntax, description)
    }

    pub fn invalid(message: &'static str, description: impl ToString) -> Self {
        Self::new(message, ErrorKind::InvalidValue, description)
    }

    pub fn lookup(
        message: &'static str,
        missing: MissingReference,
        description: impl ToString,
    ) -> Self {
        Self::new(message, ErrorKind::Lookup(missing), description)
    }

    pub fn new(message: &'static str, kind: ErrorKind, description: impl ToString) -> Self {
        Self {
            message,
            kind,
            description: description.to_string(),
        }
    }
}

/// Parse errors that prevent successful parsing
#[derive(Debug, Clone, Error)]
pub enum ParseError {
    #[error(
        "Syntax error parsing {target} on {context}: {message}",
        message = .detail.describe_with_line(&.context.line)
    )]
    Syntax {
        target: &'static str,
        context: ParseContext,
        detail: Box<SyntaxErrorDetail>,
    },

    #[error("Error parsing extension on {0}: {1}")]
    Extension(ParseContext, #[source] ExtensionParseError),

    #[error("Error parsing extension detail on {0}: {1}")]
    ExtensionDetail(ParseContext, #[source] ExtensionError),

    #[error("Error parsing plan on {0}: {1}")]
    Plan(ParseContext, #[source] MessageParseError),

    #[error("Unregistered extension '{name}' on {context}")]
    UnregisteredExtension { name: String, context: ParseContext },

    #[error("Failed to parse relation on {0}: {1}")]
    RelationParse(ParseContext, String),

    #[error("Error parsing section header on {0}: {1}")]
    Initial(ParseContext, #[source] MessageParseError),
}

/// Result type for the public Parser API
pub type ParseResult = Result<substrait::proto::Plan, ParseError>;
