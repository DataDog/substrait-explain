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
    #[error("Error parsing extension on {0}: {1}")]
    Extension(ParseContext, #[source] ExtensionParseError),

    #[error("Error parsing extension detail on {0}: {1}")]
    ExtensionDetail(ParseContext, #[source] ExtensionError),

    #[error("Error parsing plan on {0}: {1}")]
    Plan(ParseContext, #[source] MessageParseError),

    #[error("Expected '{expected}' on {context}")]
    Expected {
        expected: &'static str,
        context: ParseContext,
    },

    #[error("No relation found in plan")]
    NoRelationFound,

    #[error("Unregistered extension '{name}' on {context}")]
    UnregisteredExtension { name: String, context: ParseContext },

    #[error("Failed to parse relation on {0}: {1}")]
    RelationParse(ParseContext, String),

    #[error("Unknown extension relation type: {0}")]
    UnknownExtensionRelationType(String),

    #[error("Validation error: {0}")]
    ValidationError(String),

    #[error("Error parsing section header on {0}: {1}")]
    Initial(ParseContext, #[source] MessageParseError),
}

/// Result type for the public Parser API
pub type ParseResult = Result<substrait::proto::Plan, ParseError>;
