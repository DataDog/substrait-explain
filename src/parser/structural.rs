//! Parser for the structural part of the Substrait file format.
//!
//! This is the overall parser for parsing the text format. It is responsible
//! for tracking which section of the file we are currently parsing, and parsing
//! each line separately.

use std::fmt;
use std::str::FromStr;

use thiserror::Error;

use crate::extensions::simple::{self, ExtensionKind};
use crate::extensions::{InsertError, SimpleExtensions};
use crate::parser::relations::{Relation, RelationParseError};
use crate::parser::{MessageParseError, ScopedParse};

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
pub enum LineParseError {
    #[error("Error parsing extension on line {0}: {1}")]
    Extension(ParseContext, #[source] ExtensionParseError),
    #[error("Error parsing plan on line {0}: {1}")]
    Plan(ParseContext, #[source] MessageParseError),
    #[error("Error parsing section header on line {0}: {1}")]
    Initial(ParseContext, #[source] MessageParseError),
    #[error("Error parsing relation: {0}")]
    Relation(ParseContext, #[source] RelationParseError),
}

#[derive(Debug, Clone, Error)]
pub enum ExtensionParseError {
    #[error("Unexpected line, expected {0}")]
    UnexpectedLine(ExtensionParserState),
    #[error("Error adding extension: {0}")]
    ExtensionError(#[from] InsertError),
    #[error("Error parsing message: {0}")]
    Message(#[from] MessageParseError),
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
                write!(f, "Extension Declaration for {kind}")
            }
        }
    }
}

pub struct RelationNode {
    pub context: ParseContext,
    pub relation: Relation,
    pub children: Vec<RelationNode>,
}

impl RelationNode {
    pub fn new(context: ParseContext, relation: Relation) -> Self {
        Self {
            context,
            relation,
            children: Vec::new(),
        }
    }

    pub fn convert_to_proto(&self) -> Result<substrait::proto::Rel, LineParseError> {
        let children: Vec<substrait::proto::Rel> = self
            .children
            .iter()
            .map(|c| c.convert_to_proto())
            .collect::<Result<Vec<_>, _>>()?;
        match self.relation.convert_to_proto(children) {
            Ok(rel) => Ok(rel),
            Err(e) => Err(LineParseError::Relation(self.context.clone(), e)),
        }
    }
}

pub struct PlanParserState {
    // Trees that have been completed so far.
    completed: Vec<substrait::proto::Rel>,
    // The current relational tree being built.
    current: Option<RelationNode>,
}

impl PlanParserState {
    pub fn new() -> Self {
        Self {
            completed: Vec::new(),
            current: None,
        }
    }

    /// Find the last seen node at the given depth.
    pub fn find_parent(&mut self, depth: usize) -> Option<&mut RelationNode> {
        let mut depth = depth;
        let mut current = self.current.as_mut()?;
        while depth > 0 {
            current = current.children.last_mut()?;
            depth -= 1;
        }
        Some(current)
    }

    // Push a new relation at the given depth.
    pub fn push_relation(
        &mut self,
        depth: usize,
        context: ParseContext,
        relation: Relation,
    ) -> Result<(), LineParseError> {
        if depth == 0 {
            if let Some(current) = self.current.take() {
                self.completed.push(current.convert_to_proto()?);
            }
            self.current = Some(RelationNode::new(context, relation));
            return Ok(());
        }

        let parent = self.find_parent(depth - 1).unwrap();
        parent.children.push(RelationNode {
            context,
            relation,
            children: Vec::new(),
        });
        Ok(())
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
        if line.1.is_empty() {
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
            IndentedLine(0, _s) => self.parse_subsection(line), // Pass the original line with 0 indent
            IndentedLine(1, s) => {
                let uri = super::extensions::URIExtensionDeclaration::from_str(s)
                    .map_err(ExtensionParseError::Message)?;
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
                let decl = super::extensions::SimpleExtensionDeclaration::from_str(s)?;
                self.extensions.add_extension(
                    extension_kind,
                    decl.uri_anchor,
                    decl.anchor,
                    decl.name,
                )?;
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
        write!(f, "{self:?}")
    }
}

#[derive(Debug, Clone)]
pub struct ParseContext {
    pub line_no: i64,
    pub line: String,
}

impl fmt::Display for ParseContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line {} ('{}')", self.line_no, self.line)
    }
}

/// The parser for the substrait-explain format.
///
/// This is responsible for parsing the file line by line, and tracking the
/// current state of the parser.
pub struct Parser {
    line_no: i64,
    state: State,
    ext_parser: ExtensionParser,
    plan_state: PlanParserState,
}

impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
}

impl Parser {
    pub fn new() -> Self {
        Self {
            line_no: 1,
            state: State::Initial,
            ext_parser: ExtensionParser::new(),
            plan_state: PlanParserState::new(),
        }
    }

    /// Wrap a Result with a Plan context into a LineParseError
    pub fn with_plan_context<T>(
        &self,
        result: Result<T, MessageParseError>,
        line: &str,
    ) -> Result<T, LineParseError> {
        result.map_err(|e| {
            LineParseError::Plan(
                ParseContext {
                    line_no: self.line_no,
                    line: line.to_string(),
                },
                e,
            )
        })
    }

    /// Wrap a Result with an Initial context into a LineParseError
    pub fn with_initial_context<T>(
        &self,
        result: Result<T, MessageParseError>,
        line: &str,
    ) -> Result<T, LineParseError> {
        result.map_err(|e| {
            LineParseError::Initial(
                ParseContext {
                    line_no: self.line_no,
                    line: line.to_string(),
                },
                e,
            )
        })
    }

    /// Wrap a Result with a Relation context into a LineParseError
    pub fn with_relation_context<T>(
        &self,
        result: Result<T, RelationParseError>,
        line: &str,
    ) -> Result<T, LineParseError> {
        result.map_err(|e| {
            LineParseError::Relation(
                ParseContext {
                    line_no: self.line_no,
                    line: line.to_string(),
                },
                e,
            )
        })
    }

    fn parse_initial(&mut self, line: IndentedLine) -> Result<(), LineParseError> {
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

    fn parse_extensions(&mut self, line: IndentedLine<'_>) -> Result<(), ExtensionParseError> {
        if line == IndentedLine(0, PLAN_HEADER) {
            self.state = State::Plan;
            return Ok(());
        }

        self.ext_parser.parse_line(line)?;
        Ok(())
    }

    fn parse_plan_line(&mut self, line: IndentedLine<'_>) -> Result<(), LineParseError> {
        let r =
            self.with_plan_context(Relation::parse(&self.ext_parser.extensions, line.1), line.1)?;
        let ctx = ParseContext {
            line_no: self.line_no,
            // TODO: Reference to the original line?
            line: line.1.to_string(),
        };
        self.plan_state.push_relation(0, ctx, r)
    }

    pub fn parse_line(&mut self, line: &str) -> Result<(), LineParseError> {
        let (orig, line) = (line, IndentedLine::from(line));

        let line_no = self.line_no;
        let ctx = || ParseContext {
            line_no,
            line: orig.to_string(),
        };

        match self.state {
            State::Initial => self.parse_initial(line),
            State::Extensions => self
                .parse_extensions(line)
                .map_err(|e| LineParseError::Extension(ctx(), e)),
            State::Plan => self.parse_plan_line(line),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::extensions::simple::ExtensionKind;

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
            .add_extension(ExtensionKind::Function, 1, 10, "func_a".to_string())
            .unwrap();
        expected_extensions
            .add_extension(ExtensionKind::Function, 2, 11, "func_b_special".to_string())
            .unwrap();
        expected_extensions
            .add_extension(ExtensionKind::Type, 1, 20, "SomeType".to_string())
            .unwrap();
        expected_extensions
            .add_extension(ExtensionKind::TypeVariation, 2, 30, "VarX".to_string())
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
