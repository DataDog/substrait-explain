use thiserror::Error;

use super::MessageParseError;
use super::extensions::ExtensionParseError;

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

/// Parse errors that prevent successful parsing
#[derive(Debug, Clone, Error)]
pub enum ParseError {
    #[error("Error parsing extension on {0}: {1}")]
    Extension(ParseContext, #[source] ExtensionParseError),

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

    #[error("Error parsing section header on line {0}: {1}")]
    Initial(ParseContext, #[source] MessageParseError),

    #[error("Error parsing relation: {0}")]
    Relation(ParseContext, #[source] MessageParseError),
}

/// Result type for the public Parser API
pub type ParseResult = Result<substrait::proto::Plan, ParseError>;
