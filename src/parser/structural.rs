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
use crate::parser::MessageParseError;
use crate::parser::common::ScopedParsePair;

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
    Relation(ParseContext, #[source] MessageParseError),
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
    pub relation: substrait::proto::Rel,
    pub children: Vec<RelationNode>,
}

impl RelationNode {
    pub fn new(relation: substrait::proto::Rel) -> Self {
        Self {
            relation,
            children: Vec::new(),
        }
    }

    pub fn convert_to_proto(&self) -> Result<substrait::proto::Rel, LineParseError> {
        Ok(self.relation.clone())
    }
}

/// Helper function to wire a child relation into a parent relation's input field.
/// This is the core of the "wire-as-we-go" approach - we parse parent-first but
/// wire children into parents as we encounter them, building the tree bottom-up.
fn wire_child_into_parent(parent: &mut substrait::proto::Rel, child: substrait::proto::Rel) {
    match &mut parent.rel_type {
        Some(substrait::proto::rel::RelType::Project(project_rel)) => {
            project_rel.input = Some(Box::new(child));
        }
        Some(substrait::proto::rel::RelType::Filter(filter_rel)) => {
            filter_rel.input = Some(Box::new(child));
        }
        Some(substrait::proto::rel::RelType::Read(_)) => {
            // Read relations don't have inputs, so this would be an error
            // in a well-formed plan, but we'll just ignore it for now
        }
        _ => {
            // For other relation types, we'll need to add cases as we support them
        }
    }
}

/// Helper function to get the number of input fields from a relation.
/// This is needed for Project relations to calculate output mapping indices.
fn get_input_field_count(rel: &substrait::proto::Rel) -> usize {
    match &rel.rel_type {
        Some(substrait::proto::rel::RelType::Read(read_rel)) => {
            // For Read relations, count the fields in the base schema
            read_rel
                .base_schema
                .as_ref()
                .and_then(|schema| schema.r#struct.as_ref())
                .map(|struct_| struct_.types.len())
                .unwrap_or(0)
        }
        Some(substrait::proto::rel::RelType::Filter(_)) => {
            // For Filter relations, we need to get the count from the input
            // This is a bit tricky since we're wiring bottom-up
            // For now, we'll assume a reasonable default and fix this later
            1
        }
        Some(substrait::proto::rel::RelType::Project(project_rel)) => {
            // For Project relations, we need to get the count from the input
            // This is also tricky due to bottom-up wiring
            1
        }
        _ => 0,
    }
}

pub struct PlanParserState {
    // Trees that have been completed so far.
    completed: Vec<substrait::proto::Rel>,
    // The current relational tree being built.
    // We maintain a stack of relations at different depths to support
    // the wire-as-we-go approach.
    current: Option<RelationNode>,
}

impl PlanParserState {
    pub fn new() -> Self {
        Self {
            completed: Vec::new(),
            current: None,
        }
    }

    /// Find the parent relation at the given depth.
    /// This is used in the wire-as-we-go approach to find where to wire
    /// the current relation as a child.
    pub fn find_parent(&mut self, depth: usize) -> Option<&mut RelationNode> {
        let mut current_depth = depth;
        let mut current = self.current.as_mut()?;

        // Navigate down the tree to find the node at the target depth
        while current_depth > 0 {
            current = current.children.last_mut()?;
            current_depth -= 1;
        }
        Some(current)
    }

    /// Add a new relation at the given depth using the wire-as-we-go approach.
    ///
    /// The wire-as-we-go approach works as follows:
    /// 1. Parse each relation line in order (top-down)
    /// 2. For each relation, determine if it's a root (depth 0) or child (depth > 0)
    /// 3. If it's a child, find its parent and wire it in immediately
    /// 4. This builds the tree bottom-up as we parse, avoiding the need for
    ///    post-processing or intermediate structures.
    pub fn add_relation_wire_as_we_go(
        &mut self,
        depth: usize,
        relation: substrait::proto::Rel,
    ) -> Result<(), LineParseError> {
        if depth == 0 {
            // This is a root relation - start a new tree
            if let Some(current) = self.current.take() {
                // Complete the previous tree
                self.completed.push(current.convert_to_proto()?);
            }
            self.current = Some(RelationNode::new(relation));
            return Ok(());
        }

        // This is a child relation - find its parent and wire it in
        let parent_depth = depth - 1;
        if let Some(parent) = self.find_parent(parent_depth) {
            // Wire the child into the parent's protobuf structure
            wire_child_into_parent(&mut parent.relation, relation.clone());

            // Also add it to the tree structure for future parent lookups
            parent.children.push(RelationNode::new(relation));
            Ok(())
        } else {
            // No parent found at the expected depth - this is an error
            Err(LineParseError::Plan(
                ParseContext {
                    line_no: 0, // We'll set this in the caller
                    line: "".to_string(),
                },
                MessageParseError::new(
                    "relation",
                    crate::parser::ErrorKind::InvalidValue,
                    Box::new(pest::error::Error::new_from_span(
                        pest::error::ErrorVariant::CustomError {
                            message: format!("No parent found at depth {}", parent_depth),
                        },
                        pest::Span::new("", 0, 0).unwrap(),
                    )),
                ),
            ))
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
        result: Result<T, MessageParseError>,
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
        use pest::Parser;

        use crate::parser::{ExpressionParser, Rule};
        let mut pairs = ExpressionParser::parse(Rule::relation, line.1).map_err(|e| {
            LineParseError::Plan(
                ParseContext {
                    line_no: self.line_no,
                    line: line.1.to_string(),
                },
                MessageParseError::new(
                    "relation",
                    crate::parser::ErrorKind::InvalidValue,
                    Box::new(e),
                ),
            )
        })?;
        let pair = pairs.next().unwrap();
        // Unwrap the relation rule to get the specific relation type
        let mut inner_pairs = pair.into_inner();
        let specific_pair = inner_pairs.next().unwrap();
        assert!(inner_pairs.next().is_none());

        let rel = match specific_pair.as_rule() {
            Rule::read_relation => {
                let read_rel = substrait::proto::ReadRel::parse_pair(
                    &self.ext_parser.extensions,
                    specific_pair,
                )
                .map_err(|e| {
                    LineParseError::Plan(
                        ParseContext {
                            line_no: self.line_no,
                            line: line.1.to_string(),
                        },
                        e,
                    )
                })?;
                substrait::proto::Rel {
                    rel_type: Some(substrait::proto::rel::RelType::Read(Box::new(read_rel))),
                }
            }
            Rule::filter_relation => {
                let filter_rel = substrait::proto::FilterRel::parse_pair(
                    &self.ext_parser.extensions,
                    specific_pair,
                )
                .map_err(|e| {
                    LineParseError::Plan(
                        ParseContext {
                            line_no: self.line_no,
                            line: line.1.to_string(),
                        },
                        e,
                    )
                })?;
                substrait::proto::Rel {
                    rel_type: Some(substrait::proto::rel::RelType::Filter(Box::new(filter_rel))),
                }
            }
            Rule::project_relation => {
                // For Project relations, we need to get the input field count from the parent
                // This is part of the wire-as-we-go approach - we need context from the parent
                let input_field_count = if line.0 > 0 {
                    // We're a child, so we have a parent - get the input field count from it
                    let parent_depth = line.0 - 1;
                    if let Some(parent) = self.plan_state.find_parent(parent_depth) {
                        get_input_field_count(&parent.relation)
                    } else {
                        // No parent found - this shouldn't happen in well-formed input
                        0
                    }
                } else {
                    // We're a root - no input fields
                    0
                };

                // Create a placeholder input relation for now - it will be replaced
                // when the child is wired in via the wire-as-we-go approach
                let placeholder_input = if line.0 > 0 {
                    Box::new(substrait::proto::Rel::default())
                } else {
                    Box::new(substrait::proto::Rel::default())
                };

                let project_rel = crate::parser::relations::parse_project_relation(
                    &self.ext_parser.extensions,
                    specific_pair,
                    input_field_count,
                    placeholder_input,
                )
                .map_err(|e| {
                    LineParseError::Plan(
                        ParseContext {
                            line_no: self.line_no,
                            line: line.1.to_string(),
                        },
                        e,
                    )
                })?;
                substrait::proto::Rel {
                    rel_type: Some(substrait::proto::rel::RelType::Project(Box::new(
                        project_rel,
                    ))),
                }
            }
            _ => {
                let err = pest::error::Error::new_from_span(
                    pest::error::ErrorVariant::CustomError {
                        message: format!("Unknown relation rule: {:?}", specific_pair.as_rule()),
                    },
                    specific_pair.as_span(),
                );
                return Err(LineParseError::Plan(
                    ParseContext {
                        line_no: self.line_no,
                        line: line.1.to_string(),
                    },
                    MessageParseError::new(
                        "relation",
                        crate::parser::ErrorKind::InvalidValue,
                        Box::new(err),
                    ),
                ));
            }
        };

        // Use the wire-as-we-go approach to add this relation to the tree
        // This will automatically wire it into its parent if it's a child
        self.plan_state.add_relation_wire_as_we_go(line.0, rel)
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

    #[test]
    fn test_parse_relation_tree() {
        // Example plan with a Project, a Filter, and a Read, nested by indentation
        let plan = r#"=== Plan
Project[$0, $1, 42, 84]
  Filter[$2 => $0, $1]
    Read[my.table => a:i32, b:string?, c:boolean]
"#;
        let mut parser = Parser::new();
        parser.state = State::Plan;
        parser.line_no = 1;
        for (i, line) in plan.lines().enumerate() {
            if line.trim().is_empty() || line.trim() == "=== Plan" {
                continue;
            }
            parser.line_no = (i + 1) as i64;
            parser.parse_line(line).unwrap();
        }
        // Check the tree structure
        let plan_state = &parser.plan_state;
        let root = plan_state
            .current
            .as_ref()
            .expect("Should have a root node");
        // Root should be Project
        match &root.relation.rel_type {
            Some(substrait::proto::rel::RelType::Project(p)) => {}
            other => panic!("Expected Project at root, got {:?}", other),
        }
        assert_eq!(root.children.len(), 1);
        let filter = &root.children[0];
        match &filter.relation.rel_type {
            Some(substrait::proto::rel::RelType::Filter(_)) => {}
            other => panic!("Expected Filter as child, got {:?}", other),
        }
        assert_eq!(filter.children.len(), 1);
        let read = &filter.children[0];
        match &read.relation.rel_type {
            Some(substrait::proto::rel::RelType::Read(_)) => {}
            other => panic!("Expected Read as grandchild, got {:?}", other),
        }
        assert_eq!(read.children.len(), 0);
    }
}
