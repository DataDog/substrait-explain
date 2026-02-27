//! Shared parser primitives.
//!
//! This module owns the generated `pest_typed` parser and the common parser
//! error type used across parser submodules.

use std::fmt;

use pest_typed::{ParsableTypedNode, TypedParser as PestTypedParser};
use pest_typed_derive::TypedParser as PestTypedDeriveParser;
use thiserror::Error;

use crate::extensions::simple::MissingReference;

#[derive(PestTypedDeriveParser)]
#[grammar = "parser/expression_grammar.pest"]
#[emit_rule_reference]
pub struct ExpressionParser;

/// Parse a typed grammar node and wrap syntax errors in [`MessageParseError`].
pub(crate) fn parse_typed<'i, T>(
    input: &'i str,
    message: &'static str,
) -> Result<T, MessageParseError>
where
    T: ParsableTypedNode<'i, Rule>,
{
    ExpressionParser::try_parse::<T>(input)
        .map_err(|error| MessageParseError::new(message, ErrorKind::Syntax, error))
}

/// Unescapes a quoted string literal, handling escape sequences.
pub fn unescape_string(input: &str, quote: char) -> String {
    let mut chars = input.chars();
    let first = chars.next().expect("Empty quoted string");
    assert_eq!(first, quote, "Expected opening quote {quote}");

    let mut result = String::new();
    while let Some(c) = chars.next() {
        match c {
            c if c == quote => {
                assert_eq!(
                    chars.next(),
                    None,
                    "Unexpected characters after closing quote"
                );
                break;
            }
            '\\' => {
                let next = chars
                    .next()
                    .expect("Incomplete escape sequence at end of string");
                match next {
                    'n' => result.push('\n'),
                    't' => result.push('\t'),
                    'r' => result.push('\r'),
                    _ => result.push(next),
                }
            }
            _ => result.push(c),
        }
    }

    result
}

/// An error that occurs when parsing a message within a specific line. Contains
/// context pointing at that specific error.
#[derive(Error, Debug, Clone)]
#[error("{kind} Error parsing {message}:\n{error}")]
pub struct MessageParseError {
    pub message: &'static str,
    pub kind: ErrorKind,
    #[source]
    pub error: Box<pest::error::Error<Rule>>,
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    /// Grammar-level parse failure.
    Syntax,
    /// Parsed shape was valid but the value was semantically invalid.
    InvalidValue,
    /// Name/anchor/URN lookup failure in extension metadata.
    Lookup(MissingReference),
}

impl MessageParseError {
    fn typed_span(span: pest_typed::Span<'_>) -> pest::Span<'_> {
        pest::Span::new(span.get_input(), span.start(), span.end())
            .expect("failed to construct span from typed span")
    }

    fn from_span(
        message: &'static str,
        kind: ErrorKind,
        span: pest_typed::Span<'_>,
        description: impl ToString,
    ) -> Self {
        let error = pest::error::Error::new_from_span(
            pest::error::ErrorVariant::CustomError {
                message: description.to_string(),
            },
            Self::typed_span(span),
        );
        Self::new(message, kind, Box::new(error))
    }

    pub fn syntax(
        message: &'static str,
        span: pest_typed::Span<'_>,
        description: impl ToString,
    ) -> Self {
        Self::from_span(message, ErrorKind::Syntax, span, description)
    }

    pub fn invalid(
        message: &'static str,
        span: pest_typed::Span<'_>,
        description: impl ToString,
    ) -> Self {
        Self::from_span(message, ErrorKind::InvalidValue, span, description)
    }

    pub fn invalid_line(message: &'static str, input: &str, description: impl ToString) -> Self {
        let span = pest_typed::Span::new(input, 0, input.len())
            .expect("failed to construct span from input");
        Self::from_span(message, ErrorKind::InvalidValue, span, description)
    }

    pub fn lookup(
        message: &'static str,
        missing: MissingReference,
        span: pest_typed::Span<'_>,
        description: impl ToString,
    ) -> Self {
        Self::from_span(message, ErrorKind::Lookup(missing), span, description)
    }

    pub fn new(
        message: &'static str,
        kind: ErrorKind,
        error: Box<pest::error::Error<Rule>>,
    ) -> Self {
        Self {
            message,
            kind,
            error,
        }
    }
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ErrorKind::Syntax => write!(f, "Syntax"),
            ErrorKind::InvalidValue => write!(f, "Invalid value"),
            ErrorKind::Lookup(e) => write!(f, "Invalid reference ({e})"),
        }
    }
}
