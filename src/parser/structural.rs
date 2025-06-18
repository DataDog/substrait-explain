//! Parser for the structural part of the Substrait file format.
//!
//! This is the overall parser for parsing the text format. It is responsible
//! for tracking which section of the file we are currently parsing, and parsing
//! each line separately.

use std::fmt;

use thiserror::Error;

use crate::extensions::{SimpleExtensions, simple};
use crate::parser::extensions::{ExtensionParseError, ExtensionParser};
use crate::parser::{ExpressionParser, MessageParseError, RelationParsePair, Rule};

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

/// Represents a line in the tree structure before it's converted to a relation.
/// This allows us to build the tree structure first, then convert to relations
/// with proper parent-child relationships.
#[derive(Debug, Clone)]
pub struct LineNode<'a> {
    pub pair: pest::iterators::Pair<'a, Rule>,
    pub line_no: i64,
    pub children: Vec<LineNode<'a>>,
}

impl<'a> LineNode<'a> {
    pub fn new(line: &'a str, line_no: i64) -> Result<Self, LineParseError> {
        // Parse the line immediately to catch syntax errors
        let mut pairs: pest::iterators::Pairs<'a, Rule> =
            <ExpressionParser as pest::Parser<Rule>>::parse(Rule::relation, line).map_err(|e| {
                LineParseError::Plan(
                    ParseContext {
                        line_no,
                        line: line.to_string(),
                    },
                    MessageParseError::new(
                        "relation",
                        crate::parser::ErrorKind::InvalidValue,
                        Box::new(e),
                    ),
                )
            })?;

        let pair = pairs.next().unwrap();
        assert!(pairs.next().is_none()); // Should be exactly one pair

        Ok(Self {
            pair,
            line_no,
            children: Vec::new(),
        })
    }

    /// Traverse down the tree, always taking the last child at each level, until reaching the specified depth.
    pub fn get_at_depth(&mut self, depth: usize) -> &mut LineNode<'a> {
        let mut node = self;
        for _ in 0..depth {
            if node.children.is_empty() {
                break;
            }
            node = node.children.last_mut().unwrap();
        }
        node
    }

    /// Convert this line node and its children to a relation tree.
    /// This is done bottom-up: children are converted first, then parents
    /// can access their children's output schemas.
    pub fn convert_to_relation(
        &self,
        extensions: &SimpleExtensions,
    ) -> Result<substrait::proto::Rel, LineParseError> {
        // Use the stored pair to get the specific relation pair
        let mut inner_pairs = self.pair.clone().into_inner();
        let specific_pair = inner_pairs.next().unwrap();
        assert!(inner_pairs.next().is_none());

        // Convert children first to get their output schemas
        let child_relations: Vec<substrait::proto::Rel> = self
            .children
            .iter()
            .map(|child| child.convert_to_relation(extensions))
            .collect::<Result<Vec<_>, _>>()?;

        // Get the input field count from the first child (if any)
        let input_field_count = child_relations
            .first()
            .map(get_input_field_count)
            .unwrap_or(0);

        // Parse the specific relation type using the new RelationParsePair trait
        let rel = match specific_pair.as_rule() {
            Rule::read_relation => substrait::proto::ReadRel::parse_pair_with_context(
                extensions,
                specific_pair,
                &child_relations,
                input_field_count,
            )
            .map_err(|e| {
                LineParseError::Plan(
                    ParseContext {
                        line_no: self.line_no,
                        line: self.pair.as_str().to_string(),
                    },
                    e,
                )
            })?,
            Rule::filter_relation => substrait::proto::FilterRel::parse_pair_with_context(
                extensions,
                specific_pair,
                &child_relations,
                input_field_count,
            )
            .map_err(|e| {
                LineParseError::Plan(
                    ParseContext {
                        line_no: self.line_no,
                        line: self.pair.as_str().to_string(),
                    },
                    e,
                )
            })?,
            Rule::project_relation => crate::parser::ProjectRelWrapper::parse_pair_with_context(
                extensions,
                specific_pair,
                &child_relations,
                input_field_count,
            )
            .map_err(|e| {
                LineParseError::Plan(
                    ParseContext {
                        line_no: self.line_no,
                        line: self.pair.as_str().to_string(),
                    },
                    e,
                )
            })?,
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
                        line: self.pair.as_str().to_string(),
                    },
                    MessageParseError::new(
                        "relation",
                        crate::parser::ErrorKind::InvalidValue,
                        Box::new(err),
                    ),
                ));
            }
        };
        Ok(rel)
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
        Some(substrait::proto::rel::RelType::Filter(filter_rel)) => {
            // For Filter relations, get the count from the input
            filter_rel
                .input
                .as_ref()
                .map(|input| get_input_field_count(input))
                .unwrap_or(0)
        }
        Some(substrait::proto::rel::RelType::Project(project_rel)) => {
            // For Project relations, get the count from the input
            project_rel
                .input
                .as_ref()
                .map(|input| get_input_field_count(input))
                .unwrap_or(0)
        }
        _ => 0,
    }
}

pub struct PlanParserState<'a> {
    // Trees that have been completed so far.
    completed: Vec<substrait::proto::Rel>,
    // The current line tree being built.
    current: Option<LineNode<'a>>,
}

impl<'a> PlanParserState<'a> {
    pub fn new() -> Self {
        Self {
            completed: Vec::new(),
            current: None,
        }
    }

    pub fn add_line(
        &mut self,
        depth: usize,
        line: &'a str,
        line_no: i64,
        extensions: &SimpleExtensions,
    ) -> Result<(), LineParseError> {
        if depth == 0 {
            if let Some(current) = self.current.take() {
                let relation = current.convert_to_relation(extensions)?;
                self.completed.push(relation);
            }
            self.current = Some(LineNode::new(line, line_no)?);
            return Ok(());
        }
        if let Some(root) = self.current.as_mut() {
            let parent = root.get_at_depth(depth - 1);
            parent.children.push(LineNode::new(line, line_no)?);
            Ok(())
        } else {
            Err(LineParseError::Plan(
                ParseContext {
                    line_no,
                    line: line.to_string(),
                },
                MessageParseError::new(
                    "relation",
                    crate::parser::ErrorKind::InvalidValue,
                    Box::new(pest::error::Error::new_from_span(
                        pest::error::ErrorVariant::CustomError {
                            message: format!("No root found when adding at depth {}", depth),
                        },
                        pest::Span::new("", 0, 0).unwrap(),
                    )),
                ),
            ))
        }
    }

    pub fn complete_current_tree(
        &mut self,
        extensions: &SimpleExtensions,
    ) -> Result<(), LineParseError> {
        if let Some(current) = self.current.take() {
            let relation = current.convert_to_relation(extensions)?;
            self.completed.push(relation);
        }
        Ok(())
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
pub struct Parser<'a> {
    line_no: i64,
    state: State,
    ext_parser: ExtensionParser,
    plan_state: PlanParserState<'a>,
}

impl<'a> Default for Parser<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Parser<'a> {
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

    fn parse_plan_line(&mut self, line: IndentedLine<'a>) -> Result<(), LineParseError> {
        // Simply add the line to the tree - parsing will happen during conversion
        self.plan_state
            .add_line(line.0, line.1, self.line_no, self.ext_parser.extensions())?;
        Ok(())
    }

    pub fn parse_line(&mut self, line: &'a str) -> Result<(), LineParseError> {
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

    /// Parse a complete Substrait plan from a string input.
    ///
    /// This function parses the entire input string line by line, handling
    /// extensions and plan sections, and returns the final Substrait plan.
    ///
    /// The input should follow the substrait-explain format:
    /// - Extensions section (optional) starting with "Extensions:"
    /// - Plan section starting with "=== Plan"
    /// - Relations with indentation indicating the tree structure
    pub fn parse_plan(&mut self, input: &'a str) -> Result<substrait::proto::Plan, LineParseError> {
        // Reset the parser state
        self.line_no = 1;
        self.state = State::Initial;
        self.ext_parser = ExtensionParser::new();
        self.plan_state = PlanParserState::new();

        // Parse each line
        for line in input.lines() {
            if line.trim().is_empty() {
                // Skip empty lines
                self.line_no += 1;
                continue;
            }

            self.parse_line(line)?;
            self.line_no += 1;
        }

        // Get the final plan
        self.get_plan()
    }

    /// Get the final Substrait plan from the parser state.
    ///
    /// This method completes any remaining tree and constructs the final
    /// Substrait plan with extensions and relations.
    pub fn get_plan(&mut self) -> Result<substrait::proto::Plan, LineParseError> {
        // Complete the current tree if there is one
        self.plan_state
            .complete_current_tree(self.ext_parser.extensions())?;

        // Build the final plan
        let mut plan = substrait::proto::Plan::default();

        // Add extensions
        if !self.ext_parser.extensions().is_empty() {
            plan.extension_uris = self.ext_parser.extensions().to_extension_uris();
            plan.extensions = self.ext_parser.extensions().to_extension_declarations();
        }

        // Add relations - convert from Rel to PlanRel
        if !self.plan_state.completed.is_empty() {
            plan.relations = self
                .plan_state
                .completed
                .iter()
                .map(|rel| substrait::proto::PlanRel {
                    rel_type: Some(substrait::proto::plan_rel::RelType::Rel(rel.clone())),
                })
                .collect();
        }

        Ok(plan)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::extensions::simple::ExtensionKind;
    use crate::parser::extensions::ExtensionParserState;

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

        assert_eq!(*parser.extensions(), expected_extensions);

        let extensions_str = parser.extensions().to_string("  ");
        println!("{}", extensions_str);
        assert_eq!(extensions_str.trim(), input_block.trim());
        // Check final state after all lines are processed.
        // The last significant line in input_block is a TypeVariation declaration.
        assert_eq!(
            parser.state(),
            ExtensionParserState::ExtensionDeclarations(ExtensionKind::TypeVariation)
        );

        // Check that a subsequent blank line correctly resets state to Extensions.
        parser.parse_line(IndentedLine(0, "")).unwrap();
        assert_eq!(parser.state(), ExtensionParserState::Extensions);
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

        let extensions_str = parser.extensions().to_string("  ");
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

        // Complete the current tree to convert it to relations
        parser
            .plan_state
            .complete_current_tree(parser.ext_parser.extensions())
            .unwrap();

        // Check the tree structure - now we have relations in completed
        let plan_state = &parser.plan_state;
        assert_eq!(plan_state.completed.len(), 1);

        let root_rel = &plan_state.completed[0];
        // Root should be Project
        match &root_rel.rel_type {
            Some(substrait::proto::rel::RelType::Project(_p)) => {}
            other => panic!("Expected Project at root, got {:?}", other),
        }

        // Check that Project has Filter as input
        match &root_rel.rel_type {
            Some(substrait::proto::rel::RelType::Project(project)) => {
                assert!(project.input.is_some());
                let filter_input = project.input.as_ref().unwrap();

                // Check that Filter has Read as input
                match &filter_input.rel_type {
                    Some(substrait::proto::rel::RelType::Filter(_)) => {
                        match &filter_input.rel_type {
                            Some(substrait::proto::rel::RelType::Filter(filter)) => {
                                assert!(filter.input.is_some());
                                let read_input = filter.input.as_ref().unwrap();

                                // Check that Read has no input (it's a leaf)
                                match &read_input.rel_type {
                                    Some(substrait::proto::rel::RelType::Read(_)) => {}
                                    other => panic!("Expected Read relation, got {:?}", other),
                                }
                            }
                            other => panic!("Expected Filter relation, got {:?}", other),
                        }
                    }
                    other => panic!("Expected Filter relation, got {:?}", other),
                }
            }
            other => panic!("Expected Project relation, got {:?}", other),
        }
    }

    #[test]
    fn test_parse_full_plan() {
        // Test a complete Substrait plan with extensions and relations
        let input = r#"=== Extensions
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
=== Plan
Project[$0, $1, 42, 84]
  Filter[$2 => $0, $1]
    Read[my.table => a:i32, b:string?, c:boolean]
"#;

        let mut parser = Parser::new();
        let plan = parser.parse_plan(input).unwrap();

        // Verify the plan structure
        assert_eq!(plan.extension_uris.len(), 2);
        assert_eq!(plan.extensions.len(), 4);
        assert_eq!(plan.relations.len(), 1);

        // Verify extension URIs
        let uri1 = &plan.extension_uris[0];
        assert_eq!(uri1.extension_uri_anchor, 1);
        assert_eq!(uri1.uri, "/uri/common");

        let uri2 = &plan.extension_uris[1];
        assert_eq!(uri2.extension_uri_anchor, 2);
        assert_eq!(uri2.uri, "/uri/specific_funcs");

        // Verify extensions
        let func1 = &plan.extensions[0];
        match &func1.mapping_type {
            Some(substrait::proto::extensions::simple_extension_declaration::MappingType::ExtensionFunction(f)) => {
                assert_eq!(f.function_anchor, 10);
                assert_eq!(f.extension_uri_reference, 1);
                assert_eq!(f.name, "func_a");
            }
            other => panic!("Expected ExtensionFunction, got {:?}", other),
        }

        let func2 = &plan.extensions[1];
        match &func2.mapping_type {
            Some(substrait::proto::extensions::simple_extension_declaration::MappingType::ExtensionFunction(f)) => {
                assert_eq!(f.function_anchor, 11);
                assert_eq!(f.extension_uri_reference, 2);
                assert_eq!(f.name, "func_b_special");
            }
            other => panic!("Expected ExtensionFunction, got {:?}", other),
        }

        let type1 = &plan.extensions[2];
        match &type1.mapping_type {
            Some(substrait::proto::extensions::simple_extension_declaration::MappingType::ExtensionType(t)) => {
                assert_eq!(t.type_anchor, 20);
                assert_eq!(t.extension_uri_reference, 1);
                assert_eq!(t.name, "SomeType");
            }
            other => panic!("Expected ExtensionType, got {:?}", other),
        }

        let var1 = &plan.extensions[3];
        match &var1.mapping_type {
            Some(substrait::proto::extensions::simple_extension_declaration::MappingType::ExtensionTypeVariation(v)) => {
                assert_eq!(v.type_variation_anchor, 30);
                assert_eq!(v.extension_uri_reference, 2);
                assert_eq!(v.name, "VarX");
            }
            other => panic!("Expected ExtensionTypeVariation, got {:?}", other),
        }

        // Verify the relation tree structure
        let root_rel = &plan.relations[0];
        match &root_rel.rel_type {
            Some(substrait::proto::plan_rel::RelType::Rel(rel)) => {
                match &rel.rel_type {
                    Some(substrait::proto::rel::RelType::Project(project)) => {
                        // Verify Project relation
                        assert_eq!(project.expressions.len(), 2); // 42 and 84
                        println!("Project input: {:?}", project.input.is_some());
                        assert!(project.input.is_some()); // Should have Filter as input

                        // Check the Filter input
                        let filter_input = project.input.as_ref().unwrap();
                        match &filter_input.rel_type {
                            Some(substrait::proto::rel::RelType::Filter(filter)) => {
                                println!("Filter input: {:?}", filter.input.is_some());
                                assert!(filter.input.is_some()); // Should have Read as input

                                // Check the Read input
                                let read_input = filter.input.as_ref().unwrap();
                                match &read_input.rel_type {
                                    Some(substrait::proto::rel::RelType::Read(read)) => {
                                        // Verify Read relation
                                        let schema = read.base_schema.as_ref().unwrap();
                                        assert_eq!(schema.names.len(), 3);
                                        assert_eq!(schema.names[0], "a");
                                        assert_eq!(schema.names[1], "b");
                                        assert_eq!(schema.names[2], "c");

                                        let struct_ = schema.r#struct.as_ref().unwrap();
                                        assert_eq!(struct_.types.len(), 3);
                                    }
                                    other => panic!("Expected Read relation, got {:?}", other),
                                }
                            }
                            other => panic!("Expected Filter relation, got {:?}", other),
                        }
                    }
                    other => panic!("Expected Project relation, got {:?}", other),
                }
            }
            other => panic!("Expected Rel type, got {:?}", other),
        }
    }
}
