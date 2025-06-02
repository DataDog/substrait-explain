//! Parser for the structural part of the Substrait file format.
//!
//! This is the overall parser for parsing the text format. It is responsible
//! for tracking which section of the file we are currently parsing, and parsing
//! each line separately.

use std::fmt;

use thiserror::Error;

use crate::extensions::{ExtensionError, SimpleExtensions, simple, simple::ExtensionKind};

#[derive(Debug)]
pub enum State {
    // The initial state, before we have parsed any lines.
    Initial,
    // The extensions section, after parsing the header, before parsing any subsection headers.
    Extensions,
    // The extension URIs section, after parsing the 'URIs:' subsection header, and any URIs so far.
    ExtensionUris,
    // In a subsection, after parsing the subsection header, and any declarations so far.
    ExtensionDeclarations(ExtensionKind),
    // The plan section, after parsing the header, before parsing any plan lines.
    Plan,
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Error)]
#[error("Parse error at line {line_no}: {state}: {error}")]
pub struct ParseError<'a> {
    pub line_no: i64,
    pub line: &'a str,
    pub state: State,
    pub error: ParseErrorType,
}

#[derive(Debug, Error)]
pub enum ParseErrorType {
    #[error("Extension error: {0}")]
    ExtensionError(ExtensionError),
    #[error("Unexpected line")]
    UnexpectedLine,
}

impl<'a> ParseError<'a> {
    pub fn unexpected_line(line_no: i64, line: &'a str, state: State) -> Self {
        Self {
            line_no: 0,
            line,
            state,
            error: ParseErrorType::UnexpectedLine,
        }
    }
}

pub struct StructuralParser<'a> {
    line_no: i64,
    state: State,
    errors: Vec<ParseError<'a>>,
    extensions: SimpleExtensions,
    // plans: …,
}

impl<'a> StructuralParser<'a> {
    pub fn new() -> Self {
        Self {
            line_no: 1,
            state: State::Initial,
            errors: Vec::new(),
            extensions: SimpleExtensions::new(),
        }
    }
}
impl<'a> StructuralParser<'a> {
    // Trim ending whitespace and return the number of indents (number of leading spaces / 2)
    pub fn trim(&self, line: &'a str) -> (usize, &'a str) {
        let mut line = line.trim_end();
        let mut spaces = 0;
        for c in line.chars() {
            if c == ' ' {
                spaces += 1;
            } else {
                break;
            }
        }

        let indents = spaces / 2;

        (_, line) = line.split_at(indents * 2);

        (indents, line)
    }

    pub fn parse_initial(&mut self, line: &'a str) -> Result<(), ParseError<'a>> {
        let (indents, trimmed) = self.trim(line);
        if indents == 0 && trimmed == simple::EXTENSIONS_HEADER {
            self.state = State::ExtensionUris;
            return Ok(());
        }
        return Err(ParseError::unexpected_line(
            self.line_no,
            line,
            State::Initial,
        ));
    }

    pub fn parse_extensions(&mut self, line: &'a str) -> Result<(), ParseError<'a>> {
        let (indents, trimmed) = self.trim(line);
        // TODO: This allows for subsections to be repeated, which is kind of weird.
        let state = match (indents, trimmed) {
            (0, simple::EXTENSION_URIS_HEADER) => State::ExtensionUris,
            (0, simple::EXTENSION_FUNCTIONS_HEADER) => {
                State::ExtensionDeclarations(ExtensionKind::Function)
            }
            (0, simple::EXTENSION_TYPES_HEADER) => {
                State::ExtensionDeclarations(ExtensionKind::Type)
            }
            (0, simple::EXTENSION_TYPE_VARIATIONS_HEADER) => {
                State::ExtensionDeclarations(ExtensionKind::TypeVariation)
            }
            // Blank lines are allowed between subsections, so if we see one, we stay in the same state.
            (0, "") => State::Extensions,
            _ => {
                return Err(ParseError::unexpected_line(
                    self.line_no,
                    line,
                    State::Extensions,
                ));
            }
        };
        self.state = state;
        Ok(())
    }

    fn parse_extension_uris(&mut self, line: &'a str) -> Result<(), ParseError<'a>> {
        let (indents, trimmed) = self.trim(line);
        match (indents, trimmed) {
            (0, "") => {
                // Blank lines are allowed between subsections, so if we see one, we stay in the same state.
                self.state = State::Extensions;
                Ok(())
            }
            _ => Err(ParseError::unexpected_line(
                self.line_no,
                line,
                State::ExtensionUris,
            )),
        }
    }
}
