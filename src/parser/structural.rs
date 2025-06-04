//! Parser for the structural part of the Substrait file format.
//!
//! This is the overall parser for parsing the text format. It is responsible
//! for tracking which section of the file we are currently parsing, and parsing
//! each line separately.

use std::fmt;

use thiserror::Error;

use crate::extensions::{
    ExtensionError, SimpleExtensions,
    simple::{self, ExtensionKind},
};

use super::{FromStr, Rule};

pub const PLAN_HEADER: &str = "=== Plan";

/// Represents an input line, trimmed of leading two-space indents and final
/// whitespace. Contains the number of indents and the trimmed line.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IndentedLine<'a>(pub usize, pub &'a str);

impl<'a> From<&'a str> for IndentedLine<'a> {
    fn from(line: &'a str) -> Self {
        let line = line.trim_end();
        let mut spaces = 0;
        for c in line.chars() {
            if c == ' ' {
                spaces += 1;
            } else {
                break;
            }
        }

        let indents = spaces / 2;

        let (_, trimmed) = line.split_at(indents * 2);

        IndentedLine(indents, trimmed)
    }
}

#[derive(Debug, Clone, Error)]
pub enum ExtensionParseError {
    #[error("Unexpected line, expected {0}")]
    UnexpectedLine(ExtensionParserState),
    #[error("Extension error: {0}")]
    ExtensionError(#[from] ExtensionError),
    #[error("Error parsing line: {0}")]
    LineParseError(#[from] pest::error::Error<Rule>),
}

/// The state of the extension parser - tracking what section of extension
/// parsing we are in.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ExtensionParserState {
    // The extensions section, after parsing the 'Extensions:' header, before
    // parsing any subsection headers.
    Extensions,
    // The extension URIs section, after parsing the 'URIs:' subsection header,
    // and any URIs so far.
    ExtensionUris,
    // In a subsection, after parsing the subsection header, and any
    // declarations so far.
    ExtensionDeclarations(ExtensionKind),
}

impl fmt::Display for ExtensionParserState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExtensionParserState::Extensions => write!(f, "Subsection Header, e.g. 'URIs:'"),
            ExtensionParserState::ExtensionUris => write!(f, "Extension URIs"),
            ExtensionParserState::ExtensionDeclarations(kind) => {
                write!(f, "Extension Declaration for {}", kind)
            }
        }
    }
}

/// The parser for the extension section of the Substrait file format.
///
/// This is responsible for parsing the extension section of the file, which
/// contains the extension URIs and declarations.
///
/// Consumes input line by line,
pub struct ExtensionParser {
    state: ExtensionParserState,
    extensions: SimpleExtensions,
}

impl ExtensionParser {
    pub fn new() -> Self {
        Self {
            state: ExtensionParserState::Extensions,
            extensions: SimpleExtensions::new(),
        }
    }

    pub fn parse_line(&mut self, line: IndentedLine) -> Result<(), ExtensionParseError> {
        if let IndentedLine(_, "") = line {
            // Blank lines are allowed between subsections, so if we see
            // one, we revert out of the subsection.
            self.state = ExtensionParserState::Extensions;
            return Ok(());
        }

        match self.state {
            ExtensionParserState::Extensions => self.parse_subsection(line),
            ExtensionParserState::ExtensionUris => self.parse_extension_uris(line),
            ExtensionParserState::ExtensionDeclarations(extension_kind) => {
                self.parse_declarations(line, extension_kind)
            }
        }
    }

    fn parse_subsection(&mut self, line: IndentedLine) -> Result<(), ExtensionParseError> {
        match line {
            IndentedLine(0, simple::EXTENSION_URIS_HEADER) => {
                self.state = ExtensionParserState::ExtensionUris;
                Ok(())
            }
            IndentedLine(0, simple::EXTENSION_FUNCTIONS_HEADER) => {
                self.state = ExtensionParserState::ExtensionDeclarations(ExtensionKind::Function);
                Ok(())
            }
            IndentedLine(0, simple::EXTENSION_TYPES_HEADER) => {
                self.state = ExtensionParserState::ExtensionDeclarations(ExtensionKind::Type);
                Ok(())
            }
            IndentedLine(0, simple::EXTENSION_TYPE_VARIATIONS_HEADER) => {
                self.state =
                    ExtensionParserState::ExtensionDeclarations(ExtensionKind::TypeVariation);
                Ok(())
            }
            _ => Err(ExtensionParseError::UnexpectedLine(self.state)),
        }
    }

    fn parse_extension_uris(&mut self, line: IndentedLine) -> Result<(), ExtensionParseError> {
        match line {
            //
            IndentedLine(0, _s) => self.parse_subsection(line), // Pass the original line with 0 indent
            IndentedLine(1, s) => {
                let uri = crate::structure::URIExtensionDeclaration::from_str(s)?;
                self.extensions.add_extension_uri(uri.uri, uri.anchor)?;
                Ok(())
            }
            _ => Err(ExtensionParseError::UnexpectedLine(self.state)),
        }
    }

    fn parse_declarations(
        &mut self,
        line: IndentedLine,
        extension_kind: ExtensionKind,
    ) -> Result<(), ExtensionParseError> {
        match line {
            IndentedLine(0, _s) => self.parse_subsection(line), // Pass the original line with 0 indent
            IndentedLine(1, s) => {
                let decl = crate::structure::SimpleExtensionDeclaration::from_str(s)?;
                match extension_kind {
                    ExtensionKind::Function => self.extensions.add_extension_function(
                        decl.uri_anchor,
                        decl.anchor,
                        decl.name,
                    )?,
                    ExtensionKind::Type => self.extensions.add_extension_type(
                        decl.uri_anchor,
                        decl.anchor,
                        decl.name,
                    )?,
                    ExtensionKind::TypeVariation => self.extensions.add_extension_type_variation(
                        decl.uri_anchor,
                        decl.anchor,
                        decl.name,
                    )?,
                    ExtensionKind::Uri => {
                        // This case should ideally not be reached if state management is correct,
                        // as URIs are handled by parse_extension_uris.
                        // However, to be exhaustive:
                        return Err(ExtensionParseError::UnexpectedLine(self.state));
                    }
                }
                Ok(())
            }
            _ => Err(ExtensionParseError::UnexpectedLine(self.state)),
        }
    }
}

#[derive(Debug)]
pub enum State {
    // The initial state, before we have parsed any lines.
    Initial,
    // The extensions section, after parsing the header and any other Extension lines.
    Extensions,
    // The plan section, after parsing the header and any other Plan lines.
    Plan,
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, Error)]
pub enum ParseError {
    #[error("Error parsing extensions: {0}")]
    ExtensionError(#[from] ExtensionParseError),
}

#[derive(Debug, Clone)]
pub struct ParseContext {
    pub line_no: i64,
    pub line: String,
}

impl fmt::Display for ParseContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line {}: '{}'", self.line_no, self.line)
    }
}

#[derive(Debug, Clone, Error)]
#[error("Error parsing {context}: {error}")]
pub struct ParseErrorWithContext {
    #[source]
    error: ParseError,
    context: ParseContext,
}

/// The parser for the substrait-explain format.
///
/// This is responsible for parsing the file line by line, and tracking the
/// current state of the parser.
pub struct Parser {
    line_no: i64,
    state: State,
    extensions: ExtensionParser,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            line_no: 1,
            state: State::Initial,
            extensions: ExtensionParser::new(),
        }
    }
}
impl Parser {
    fn parse_initial(&mut self, line: IndentedLine) -> Result<(), ParseError> {
        if line == IndentedLine(0, simple::EXTENSIONS_HEADER) {
            self.state = State::Extensions;
            return Ok(());
        }
        if line == IndentedLine(0, PLAN_HEADER) {
            self.state = State::Plan;
            return Ok(());
        }

        todo!()
    }

    fn parse_extensions(&mut self, line: IndentedLine<'_>) -> Result<(), ParseError> {
        if line == IndentedLine(0, PLAN_HEADER) {
            self.state = State::Plan;
            return Ok(());
        }

        self.extensions.parse_line(line)?;
        Ok(())
    }

    fn parse_plan(&mut self, line: IndentedLine<'_>) -> Result<(), ParseError> {
        todo!()
    }

    fn add_error_context(&self, line: String, error: ParseError) -> ParseErrorWithContext {
        ParseErrorWithContext {
            error,
            context: ParseContext {
                line_no: self.line_no,
                line,
            },
        }
    }

    pub fn parse_line(&mut self, line: &str) -> Result<(), ParseErrorWithContext> {
        let (orig, line) = (line, IndentedLine::from(line));

        let result = match self.state {
            State::Initial => self.parse_initial(line),
            State::Extensions => self.parse_extensions(line),
            State::Plan => self.parse_plan(line),
        };
        if let Err(e) = result {
            return Err(self.add_error_context(orig.to_string(), e));
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::extensions::ExtensionLookup;
    use crate::extensions::simple::ExtensionKind;

    // Helper to create IndentedLine - still needed for error tests
    fn il(indent: usize, text: &str) -> IndentedLine {
        IndentedLine(indent, text)
    }

    #[test]
    fn test_parse_basic_block() {
        let mut expected_extensions = SimpleExtensions::new();
        expected_extensions
            .add_extension_uri("/uri/common".to_string(), 1)
            .unwrap();
        expected_extensions
            .add_extension_uri("/uri/specific_funcs".to_string(), 2)
            .unwrap();
        expected_extensions
            .add_extension_function(1, 10, "func_a".to_string())
            .unwrap();
        expected_extensions
            .add_extension_function(2, 11, "func_b_special".to_string())
            .unwrap();
        expected_extensions
            .add_extension_type(1, 20, "SomeType".to_string())
            .unwrap();
        expected_extensions
            .add_extension_type_variation(2, 30, "VarX".to_string())
            .unwrap();

        let mut parser = ExtensionParser::new();
        let input_block = r#"
URIs:
  @  1: /uri/common
  @  2: /uri/specific_funcs
Functions:
  # 10 @  1: func_a
  # 11 @  2: func_b_special
Types:
  # 20 @  1: SomeType
Type Variations:
  # 30 @  2: VarX
"#;

        for line_str in input_block.trim().lines() {
            parser
                .parse_line(IndentedLine::from(line_str))
                .unwrap_or_else(|e| panic!("Failed to parse line \'{}\': {:?}", line_str, e));
        }

        assert_eq!(parser.extensions, expected_extensions);

        let extensions_str = parser.extensions.to_string("  ");
        println!("{}", extensions_str);
        assert_eq!(extensions_str.trim(), input_block.trim());
        // Check final state after all lines are processed.
        // The last significant line in input_block is a TypeVariation declaration.
        assert_eq!(
            parser.state,
            ExtensionParserState::ExtensionDeclarations(ExtensionKind::TypeVariation)
        );

        // Check that a subsequent blank line correctly resets state to Extensions.
        parser.parse_line(IndentedLine(0, "")).unwrap();
        assert_eq!(parser.state, ExtensionParserState::Extensions);
    }

    /// Test that we can parse a larger extensions block and it matches the input.
    #[test]
    fn test_parse_complete_extension_block() {
        let mut parser = ExtensionParser::new();
        let input_block = r#"
URIs:
  @  1: /uri/common
  @  2: /uri/specific_funcs
  @  3: /uri/types_lib
  @  4: /uri/variations_lib
Functions:
  # 10 @  1: func_a
  # 11 @  2: func_b_special
  # 12 @  1: func_c_common
Types:
  # 20 @  1: CommonType
  # 21 @  3: LibraryType
  # 22 @  1: AnotherCommonType
Type Variations:
  # 30 @  4: VarX
  # 31 @  4: VarY
"#;

        for line_str in input_block.trim().lines() {
            parser
                .parse_line(IndentedLine::from(line_str))
                .unwrap_or_else(|e| panic!("Failed to parse line \'{}\': {:?}", line_str, e));
        }

        let extensions_str = parser.extensions.to_string("  ");
        println!("{}", extensions_str);
        assert_eq!(extensions_str.trim(), input_block.trim());
    }
}
