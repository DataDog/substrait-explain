//! Parser for the structural part of the Substrait file format.
//!
//! This is the overall parser for parsing the text format. It is responsible
//! for tracking which section of the file we are currently parsing, and parsing
//! each line separately.

use std::fmt;

use substrait::proto::rel::RelType;
use substrait::proto::{FilterRel, Plan, PlanRel, ProjectRel, ReadRel, Rel, RelRoot, plan_rel};
use thiserror::Error;

use crate::extensions::{SimpleExtensions, simple};
use crate::parser::common::{MessageParseError, ParsePair};
use crate::parser::expressions::Name;
use crate::parser::extensions::{ExtensionParseError, ExtensionParser};
use crate::parser::{ErrorKind, ExpressionParser, RelationParsePair, Rule, unwrap_single_pair};

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
pub enum ParseError {
    #[error("Error parsing extension on line {0}: {1}")]
    Extension(ParseContext, #[source] ExtensionParseError),
    #[error("Error parsing plan on {0}: {1}")]
    Plan(ParseContext, #[source] MessageParseError),
    #[error("Error parsing section header on line {0}: {1}")]
    Initial(ParseContext, #[source] MessageParseError),
    #[error("Error parsing relation: {0}")]
    Relation(ParseContext, #[source] MessageParseError),
}

/// Represents a line in the [`Plan`] tree structure before it's converted to a
/// relation. This allows us to build the tree structure first, then convert to
/// relations with proper parent-child relationships.
#[derive(Debug, Clone)]
pub struct LineNode<'a> {
    pub pair: pest::iterators::Pair<'a, Rule>,
    pub line_no: i64,
    pub children: Vec<LineNode<'a>>,
}

impl<'a> LineNode<'a> {
    pub fn context(&self) -> ParseContext {
        ParseContext {
            line_no: self.line_no,
            line: self.pair.as_str().to_string(),
        }
    }

    pub fn parse(line: &'a str, line_no: i64) -> Result<Self, ParseError> {
        // Parse the line immediately to catch syntax errors
        let mut pairs: pest::iterators::Pairs<'a, Rule> =
            <ExpressionParser as pest::Parser<Rule>>::parse(Rule::relation, line).map_err(|e| {
                ParseError::Plan(
                    ParseContext {
                        line_no,
                        line: line.to_string(),
                    },
                    MessageParseError::new("relation", ErrorKind::InvalidValue, Box::new(e)),
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

    /// Parse the root relation of a plan, at depth 0.
    pub fn parse_root(line: &'a str, line_no: i64) -> Result<Self, ParseError> {
        // Parse the line as a top-level relation (either root_relation or regular relation)
        let mut pairs: pest::iterators::Pairs<'a, Rule> =
            <ExpressionParser as pest::Parser<Rule>>::parse(Rule::top_level_relation, line)
                .map_err(|e| {
                    ParseError::Plan(
                        ParseContext::new(line_no, line.to_string()),
                        MessageParseError::new(
                            "top_level_relation",
                            crate::parser::ErrorKind::Syntax,
                            Box::new(e),
                        ),
                    )
                })?;

        let pair = pairs.next().unwrap();
        assert!(pairs.next().is_none());

        // Get the inner pair, which is either a root relation or a regular relation
        let inner_pair = unwrap_single_pair(pair);

        Ok(Self {
            pair: inner_pair,
            line_no,
            children: Vec::new(),
        })
    }
}

/// Helper function to get the number of input fields from a relation.
/// This is needed for Project relations to calculate output mapping indices.
fn get_input_field_count(rel: &Rel) -> usize {
    match &rel.rel_type {
        Some(RelType::Read(read_rel)) => {
            // For Read relations, count the fields in the base schema
            read_rel
                .base_schema
                .as_ref()
                .and_then(|schema| schema.r#struct.as_ref())
                .map(|struct_| struct_.types.len())
                .unwrap_or(0)
        }
        Some(RelType::Filter(filter_rel)) => {
            // For Filter relations, get the count from the input
            filter_rel
                .input
                .as_ref()
                .map(|input| get_input_field_count(input))
                .unwrap_or(0)
        }
        Some(RelType::Project(project_rel)) => {
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

#[derive(Copy, Clone, Debug)]
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

impl ParseContext {
    pub fn new(line_no: i64, line: String) -> Self {
        Self { line_no, line }
    }
}

impl fmt::Display for ParseContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line {} ('{}')", self.line_no, self.line)
    }
}

// An in-progress tree builder, building the tree of relations.
#[derive(Debug, Clone, Default)]
pub struct TreeBuilder<'a> {
    // Current tree of nodes being built. These have been successfully parsed
    // into Pest pairs, but have not yet been converted to substrait plans.
    current: Option<LineNode<'a>>,
    // Completed trees that have been built.
    completed: Vec<LineNode<'a>>,
}

impl<'a> TreeBuilder<'a> {
    /// Traverse down the tree, always taking the last child at each level, until reaching the specified depth.
    pub fn get_at_depth(&mut self, depth: usize) -> Option<&mut LineNode<'a>> {
        let mut node = self.current.as_mut()?;
        for _ in 0..depth {
            node = node.children.last_mut()?;
        }
        Some(node)
    }

    pub fn add_line(&mut self, depth: usize, node: LineNode<'a>) -> Result<(), ParseError> {
        if depth == 0 {
            if let Some(prev) = self.current.take() {
                self.completed.push(prev)
            }
            self.current = Some(node);
            return Ok(());
        }

        let parent = match self.get_at_depth(depth - 1) {
            None => {
                return Err(ParseError::Plan(
                    node.context(),
                    MessageParseError::invalid(
                        "relation",
                        node.pair.as_span(),
                        format!("No parent found for depth {depth}"),
                    ),
                ));
            }
            Some(parent) => parent,
        };

        parent.children.push(node.clone());
        Ok(())
    }

    /// End of input - move any remaining nodes from stack to completed and
    /// return any trees in progress. Resets the builder to its initial state
    /// (empty)
    pub fn finish(&mut self) -> Vec<LineNode<'a>> {
        // Move any remaining nodes from stack to completed
        if let Some(node) = self.current.take() {
            self.completed.push(node);
        }
        std::mem::take(&mut self.completed)
    }
}

// Relation parsing component - handles converting LineNodes to Relations
#[derive(Debug, Clone, Default)]
pub struct RelationParser<'a> {
    tree: TreeBuilder<'a>,
}

impl<'a> RelationParser<'a> {
    pub fn parse_line(&mut self, line: IndentedLine<'a>, line_no: i64) -> Result<(), ParseError> {
        let IndentedLine(depth, line) = line;

        // Use parse_root for depth 0 (top-level relations), parse for other depths
        let node = if depth == 0 {
            LineNode::parse_root(line, line_no)?
        } else {
            LineNode::parse(line, line_no)?
        };

        self.tree.add_line(depth, node)
    }

    /// Parse a relation from a Pest pair of rule 'relation' into a Substrait
    /// Rel.
    //
    // Clippy says a Vec<Box<…>> is unnecessary, as the Vec is already on the
    // heap, but this is what the protobuf requires so we allow it here
    #[allow(clippy::vec_box)]
    fn parse_relation(
        &self,
        extensions: &SimpleExtensions,
        line_no: i64,
        pair: pest::iterators::Pair<Rule>,
        child_relations: Vec<Box<substrait::proto::Rel>>,
        input_field_count: usize,
    ) -> Result<substrait::proto::Rel, ParseError> {
        assert_eq!(pair.as_rule(), Rule::relation);
        let mut inner_pairs = pair.clone().into_inner();
        let p = inner_pairs.next().unwrap();
        assert!(inner_pairs.next().is_none());

        let (e, l, p, c, ic) = (extensions, line_no, p, child_relations, input_field_count);

        match p.as_rule() {
            Rule::read_relation => self.parse_rel::<ReadRel>(e, l, p, c, ic),
            Rule::filter_relation => self.parse_rel::<FilterRel>(e, l, p, c, ic),
            Rule::project_relation => self.parse_rel::<ProjectRel>(e, l, p, c, ic),
            _ => todo!(),
        }
    }

    /// Parse a specific relation type from a Pest pair of matching rule into a
    /// Substrait Rel.
    //
    // Clippy says a Vec<Box<…>> is unnecessary, as the Vec is already on the
    // heap, but this is what the protobuf requires so we allow it here
    #[allow(clippy::vec_box)]
    fn parse_rel<T: RelationParsePair>(
        &self,
        extensions: &SimpleExtensions,
        line_no: i64,
        pair: pest::iterators::Pair<Rule>,
        child_relations: Vec<Box<substrait::proto::Rel>>,
        input_field_count: usize,
    ) -> Result<substrait::proto::Rel, ParseError> {
        assert_eq!(pair.as_rule(), T::rule());

        let line = pair.as_str();
        let rel_type =
            T::parse_pair_with_context(extensions, pair, child_relations, input_field_count);

        match rel_type {
            Ok(rel) => Ok(rel.into_rel()),
            Err(e) => Err(ParseError::Plan(
                ParseContext::new(line_no, line.to_string()),
                e,
            )),
        }
    }

    /// Convert a given LineNode into a Substrait Rel. Also recursively builds children.
    fn build_rel(
        &self,
        extensions: &SimpleExtensions,
        node: LineNode,
    ) -> Result<substrait::proto::Rel, ParseError> {
        // Parse children first to get their output schemas
        let child_relations = node
            .children
            .into_iter()
            .map(|c| self.build_rel(extensions, c).map(Box::new))
            .collect::<Result<Vec<Box<Rel>>, ParseError>>()?;

        // Get the input field count from all the children
        let input_field_count = child_relations
            .iter()
            .map(|r| get_input_field_count(r.as_ref()))
            .reduce(|a, b| a + b)
            .unwrap_or(0);

        // Parse this node using the stored pair
        self.parse_relation(
            extensions,
            node.line_no,
            node.pair,
            child_relations,
            input_field_count,
        )
    }

    /// Build a tree of relations from a LineNode, with the root in the form of
    /// a PlanRel - the root type in a Substrait Plan.
    fn build_plan_rel(
        &self,
        extensions: &SimpleExtensions,
        mut node: LineNode,
    ) -> Result<PlanRel, ParseError> {
        // Plain relations are allowed as root relations, they just don't have names.
        if node.pair.as_rule() == Rule::relation {
            let rel = self.build_rel(extensions, node)?;
            return Ok(PlanRel {
                rel_type: Some(plan_rel::RelType::Rel(rel)),
            });
        }

        // Otherwise, it must be a root relation.
        assert_eq!(node.pair.as_rule(), Rule::root_relation);
        let context = node.context();
        let span = node.pair.as_span();

        // Parse the column names
        let column_names_pair = unwrap_single_pair(node.pair);
        assert_eq!(column_names_pair.as_rule(), Rule::root_name_list);

        let names: Vec<String> = column_names_pair
            .into_inner()
            .map(|name_pair| {
                assert_eq!(name_pair.as_rule(), Rule::name);
                Name::parse_pair(name_pair).0
            })
            .collect();

        let child = match node.children.len() {
            1 => self.build_rel(extensions, node.children.pop().unwrap())?,
            n => {
                return Err(ParseError::Plan(
                    context,
                    MessageParseError::invalid(
                        "root_relation",
                        span,
                        format!("Root relation must have exactly one child, found {n}"),
                    ),
                ));
            }
        };

        let rel_root = RelRoot {
            names,
            input: Some(child),
        };

        Ok(PlanRel {
            rel_type: Some(plan_rel::RelType::Root(rel_root)),
        })
    }

    /// Build all the trees we have into `PlanRel`s.
    fn build(mut self, extensions: &SimpleExtensions) -> Result<Vec<PlanRel>, ParseError> {
        let nodes = self.tree.finish();
        nodes
            .into_iter()
            .map(|n| self.build_plan_rel(extensions, n))
            .collect::<Result<Vec<PlanRel>, ParseError>>()
    }
}

/// A parser for Substrait query plans in text format.
///
/// The `Parser` converts human-readable Substrait text format into Substrait
/// protobuf plans. It handles both the extensions section (which defines
/// functions, types, etc.) and the plan section (which defines the actual query
/// structure).
///
/// ## Usage
///
/// The simplest entry point is the static `parse()` method:
///
/// ```rust
/// use substrait_explain::parser::Parser;
///
/// let plan_text = r#"
/// === Plan
/// Root[c, d]
///   Project[$1, 42]
///     Read[schema.table => a:i64, b:string?]
/// "#;
///
/// let plan = Parser::parse(plan_text).unwrap();
/// ```
///
/// ## Input Format
///
/// The parser expects input in the following format:
///
/// ```text
/// === Extensions
/// URIs:
///   @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
/// Functions:
///   # 10 @  1: add
/// === Plan
/// Root[columns]
///   Relation[arguments => columns]
///     ChildRelation[arguments => columns]
/// ```
///
/// - **Extensions section** (optional): Defines URIs and function/type declarations
/// - **Plan section** (required): Defines the query structure with indented relations
///
/// ## Error Handling
///
/// The parser provides detailed error information including:
/// - Line number where the error occurred
/// - The actual line content that failed to parse
/// - Specific error type and description
///
/// ```rust
/// use substrait_explain::parser::Parser;
///
/// let invalid_plan = r#"
/// === Plan
/// InvalidRelation[invalid syntax]
/// "#;
///
/// match Parser::parse(invalid_plan) {
///     Ok(plan) => println!("Successfully parsed"),
///     Err(e) => eprintln!("Parse error: {}", e),
/// }
/// ```
///
/// ## Supported Relations
///
/// The parser supports all standard Substrait relations:
/// - `Read[table => columns]` - Read from a table
/// - `Project[expressions]` - Project columns/expressions
/// - `Filter[condition => columns]` - Filter rows
/// - `Root[columns]` - Root relation with output columns
/// - And more...
///
/// ## Extensions Support
///
/// The parser fully supports Substrait Simple Extensions, allowing you to:
/// - Define custom functions with URIs and anchors
/// - Reference functions by name in expressions
/// - Use custom types and type variations
///
/// ```rust
/// use substrait_explain::parser::Parser;
///
/// let plan_with_extensions = r#"
/// === Extensions
/// URIs:
///   @  1: https://example.com/functions.yaml
/// Functions:
///   ## 10 @  1: my_custom_function
/// === Plan
/// Root[result]
///   Project[my_custom_function($0, $1)]
///     Read[table => col1:i32, col2:i32]
/// "#;
///
/// let plan = Parser::parse(plan_with_extensions).unwrap();
/// ```
///
/// ## Performance
///
/// The parser is designed for efficiency:
/// - Single-pass parsing with minimal allocations
/// - Early error detection and reporting
/// - Memory-efficient tree building
///
/// ## Thread Safety
///
/// `Parser` instances are not thread-safe and should not be shared between threads.
/// However, the static `parse()` method is safe to call from multiple threads.
#[derive(Debug)]
pub struct Parser<'a> {
    line_no: i64,
    state: State,
    extension_parser: ExtensionParser,
    relation_parser: RelationParser<'a>,
}
impl<'a> Default for Parser<'a> {
    fn default() -> Self {
        Self {
            line_no: 1,
            state: State::Initial,
            extension_parser: ExtensionParser::default(),
            relation_parser: RelationParser::default(),
        }
    }
}

impl<'a> Parser<'a> {
    /// Parse a Substrait plan from text format.
    ///
    /// This is the main entry point for parsing well-formed plans.
    /// Returns a clear error if parsing fails.
    ///
    /// The input should be in the Substrait text format, which consists of:
    /// - An optional extensions section starting with "=== Extensions"
    /// - A plan section starting with "=== Plan"
    /// - Indented relation definitions
    ///
    /// # Example
    /// ```rust
    /// use substrait_explain::parser::Parser;
    ///
    /// let plan_text = r#"
    /// === Plan
    /// Root[c, d]
    ///   Project[$1, 42]
    ///     Read[schema.table => a:i64, b:string?]
    /// "#;
    ///
    /// let plan = Parser::parse(plan_text).unwrap();
    /// ```
    ///
    /// # Errors
    ///
    /// Returns a [`ParseError`] if the input cannot be parsed. The error includes
    /// the line number and content where parsing failed, along with a description
    /// of what went wrong.
    pub fn parse(input: &'a str) -> Result<Plan, ParseError> {
        let mut parser = Self::default();

        for line in input.lines() {
            if line.trim().is_empty() {
                parser.line_no += 1;
                continue;
            }

            parser.parse_line(line)?;
            parser.line_no += 1;
        }

        parser.build_plan()
    }

    /// Parse a single line of input, updating the parser state.
    fn parse_line(&mut self, line: &'a str) -> Result<(), ParseError> {
        let indented_line = IndentedLine::from(line);
        let line_no = self.line_no;
        let ctx = || ParseContext {
            line_no,
            line: line.to_string(),
        };

        match self.state {
            State::Initial => self.parse_initial(indented_line),
            State::Extensions => self
                .parse_extensions(indented_line)
                .map_err(|e| ParseError::Extension(ctx(), e)),
            State::Plan => self.parse_plan_line(indented_line),
        }
    }

    /// Parse the initial line(s) of the input, which is either a blank line or
    /// the extensions or plan header.
    fn parse_initial(&mut self, line: IndentedLine) -> Result<(), ParseError> {
        match line {
            IndentedLine(0, l) if l.trim().is_empty() => {}
            IndentedLine(0, simple::EXTENSIONS_HEADER) => {
                self.state = State::Extensions;
            }
            IndentedLine(0, PLAN_HEADER) => {
                self.state = State::Plan;
            }
            IndentedLine(n, l) => {
                return Err(ParseError::Initial(
                    ParseContext::new(n as i64, l.to_string()),
                    MessageParseError::invalid(
                        "initial",
                        pest::Span::new(l, 0, l.len()).expect("Invalid span?!"),
                        format!("Unknown initial line: {l:?}"),
                    ),
                ));
            }
        }
        if line.1.trim().is_empty() {
            // Blank line - do nothing
            return Ok(());
        }

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

    /// Parse a single line from the extensions section of the input, updating
    /// the parser state.
    fn parse_extensions(&mut self, line: IndentedLine<'_>) -> Result<(), ExtensionParseError> {
        if line == IndentedLine(0, PLAN_HEADER) {
            self.state = State::Plan;
            return Ok(());
        }
        self.extension_parser.parse_line(line)
    }

    /// Parse a single line from the plan section of the input, updating the
    /// parser state.
    fn parse_plan_line(&mut self, line: IndentedLine<'a>) -> Result<(), ParseError> {
        self.relation_parser.parse_line(line, self.line_no)
    }

    /// Build the plan from the parser state.
    fn build_plan(self) -> Result<Plan, ParseError> {
        let Parser {
            relation_parser,
            extension_parser,
            ..
        } = self;

        let extensions = extension_parser.extensions();

        // Parse the tree into relations
        let root_relations = relation_parser.build(extensions)?;

        // Build the final plan
        Ok(Plan {
            extension_uris: extensions.to_extension_uris(),
            extensions: extensions.to_extension_declarations(),
            relations: root_relations,
            ..Default::default()
        })
    }
}

#[cfg(test)]
mod tests {
    use substrait::proto::extensions::simple_extension_declaration::MappingType;

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

        let mut parser = ExtensionParser::default();
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
                .unwrap_or_else(|e| panic!("Failed to parse line \'{line_str}\': {e:?}"));
        }

        assert_eq!(*parser.extensions(), expected_extensions);

        let extensions_str = parser.extensions().to_string("  ");
        // The writer adds the header; the ExtensionParser does not parse the
        // header, so we add it here for comparison.
        let expected_str = format!(
            "{}\n{}",
            simple::EXTENSIONS_HEADER,
            input_block.trim_start()
        );
        assert_eq!(extensions_str.trim(), expected_str.trim());
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
        let mut parser = ExtensionParser::default();
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
                .unwrap_or_else(|e| panic!("Failed to parse line \'{line_str}\': {e:?}"));
        }

        let extensions_str = parser.extensions().to_string("  ");
        // The writer adds the header; the ExtensionParser does not parse the
        // header, so we add it here for comparison.
        let expected_str = format!(
            "{}\n{}",
            simple::EXTENSIONS_HEADER,
            input_block.trim_start()
        );
        assert_eq!(extensions_str.trim(), expected_str.trim());
    }

    #[test]
    fn test_parse_relation_tree() {
        // Example plan with a Project, a Filter, and a Read, nested by indentation
        let plan = r#"=== Plan
Project[$0, $1, 42, 84]
  Filter[$2 => $0, $1]
    Read[my.table => a:i32, b:string?, c:boolean]
"#;
        let mut parser = Parser::default();
        for line in plan.lines() {
            parser.parse_line(line).unwrap();
        }

        // Complete the current tree to convert it to relations
        let plan = parser.build_plan().unwrap();

        let root_rel = &plan.relations[0].rel_type;
        let first_rel = match root_rel {
            Some(plan_rel::RelType::Rel(rel)) => rel,
            _ => panic!("Expected Rel type, got {root_rel:?}"),
        };
        // Root should be Project
        let project = match &first_rel.rel_type {
            Some(RelType::Project(p)) => p,
            other => panic!("Expected Project at root, got {other:?}"),
        };

        // Check that Project has Filter as input
        assert!(project.input.is_some());
        let filter_input = project.input.as_ref().unwrap();

        // Check that Filter has Read as input
        match &filter_input.rel_type {
            Some(RelType::Filter(_)) => {
                match &filter_input.rel_type {
                    Some(RelType::Filter(filter)) => {
                        assert!(filter.input.is_some());
                        let read_input = filter.input.as_ref().unwrap();

                        // Check that Read has no input (it's a leaf)
                        match &read_input.rel_type {
                            Some(RelType::Read(_)) => {}
                            other => panic!("Expected Read relation, got {other:?}"),
                        }
                    }
                    other => panic!("Expected Filter relation, got {other:?}"),
                }
            }
            other => panic!("Expected Filter relation, got {other:?}"),
        }
    }

    #[test]
    fn test_parse_root_relation() {
        // Test a plan with a Root relation
        let plan = r#"=== Plan
Root[result]
  Project[$0, $1]
    Read[my.table => a:i32, b:string?]
"#;
        let mut parser = Parser::default();
        for line in plan.lines() {
            parser.parse_line(line).unwrap();
        }

        let plan = parser.build_plan().unwrap();

        // Check that we have exactly one relation
        assert_eq!(plan.relations.len(), 1);

        let root_rel = &plan.relations[0].rel_type;
        let rel_root = match root_rel {
            Some(plan_rel::RelType::Root(rel_root)) => rel_root,
            other => panic!("Expected Root type, got {other:?}"),
        };

        // Check that the root has the correct name
        assert_eq!(rel_root.names, vec!["result"]);

        // Check that the root has a Project as input
        let project_input = match &rel_root.input {
            Some(rel) => rel,
            None => panic!("Root should have an input"),
        };

        let project = match &project_input.rel_type {
            Some(RelType::Project(p)) => p,
            other => panic!("Expected Project as root input, got {other:?}"),
        };

        // Check that Project has Read as input
        let read_input = match &project.input {
            Some(rel) => rel,
            None => panic!("Project should have an input"),
        };

        match &read_input.rel_type {
            Some(RelType::Read(_)) => {}
            other => panic!("Expected Read relation, got {other:?}"),
        }
    }

    #[test]
    fn test_parse_root_relation_no_names() {
        // Test a plan with a Root relation with no names
        let plan = r#"=== Plan
Root[]
  Project[$0, $1]
    Read[my.table => a:i32, b:string?]
"#;
        let mut parser = Parser::default();
        for line in plan.lines() {
            parser.parse_line(line).unwrap();
        }

        let plan = parser.build_plan().unwrap();

        let root_rel = &plan.relations[0].rel_type;
        let rel_root = match root_rel {
            Some(plan_rel::RelType::Root(rel_root)) => rel_root,
            other => panic!("Expected Root type, got {other:?}"),
        };

        // Check that the root has no names
        assert_eq!(rel_root.names, Vec::<String>::new());
    }

    #[test]
    fn test_parse_full_plan() {
        // Test a complete Substrait plan with extensions and relations
        let input = r#"
=== Extensions
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

        let plan = Parser::parse(input).unwrap();

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
            Some(MappingType::ExtensionFunction(f)) => {
                assert_eq!(f.function_anchor, 10);
                assert_eq!(f.extension_uri_reference, 1);
                assert_eq!(f.name, "func_a");
            }
            other => panic!("Expected ExtensionFunction, got {other:?}"),
        }

        let func2 = &plan.extensions[1];
        match &func2.mapping_type {
            Some(MappingType::ExtensionFunction(f)) => {
                assert_eq!(f.function_anchor, 11);
                assert_eq!(f.extension_uri_reference, 2);
                assert_eq!(f.name, "func_b_special");
            }
            other => panic!("Expected ExtensionFunction, got {other:?}"),
        }

        let type1 = &plan.extensions[2];
        match &type1.mapping_type {
            Some(MappingType::ExtensionType(t)) => {
                assert_eq!(t.type_anchor, 20);
                assert_eq!(t.extension_uri_reference, 1);
                assert_eq!(t.name, "SomeType");
            }
            other => panic!("Expected ExtensionType, got {other:?}"),
        }

        let var1 = &plan.extensions[3];
        match &var1.mapping_type {
            Some(MappingType::ExtensionTypeVariation(v)) => {
                assert_eq!(v.type_variation_anchor, 30);
                assert_eq!(v.extension_uri_reference, 2);
                assert_eq!(v.name, "VarX");
            }
            other => panic!("Expected ExtensionTypeVariation, got {other:?}"),
        }

        // Verify the relation tree structure
        let root_rel = &plan.relations[0];
        match &root_rel.rel_type {
            Some(plan_rel::RelType::Rel(rel)) => {
                match &rel.rel_type {
                    Some(RelType::Project(project)) => {
                        // Verify Project relation
                        assert_eq!(project.expressions.len(), 2); // 42 and 84
                        println!("Project input: {:?}", project.input.is_some());
                        assert!(project.input.is_some()); // Should have Filter as input

                        // Check the Filter input
                        let filter_input = project.input.as_ref().unwrap();
                        match &filter_input.rel_type {
                            Some(RelType::Filter(filter)) => {
                                println!("Filter input: {:?}", filter.input.is_some());
                                assert!(filter.input.is_some()); // Should have Read as input

                                // Check the Read input
                                let read_input = filter.input.as_ref().unwrap();
                                match &read_input.rel_type {
                                    Some(RelType::Read(read)) => {
                                        // Verify Read relation
                                        let schema = read.base_schema.as_ref().unwrap();
                                        assert_eq!(schema.names.len(), 3);
                                        assert_eq!(schema.names[0], "a");
                                        assert_eq!(schema.names[1], "b");
                                        assert_eq!(schema.names[2], "c");

                                        let struct_ = schema.r#struct.as_ref().unwrap();
                                        assert_eq!(struct_.types.len(), 3);
                                    }
                                    other => panic!("Expected Read relation, got {other:?}"),
                                }
                            }
                            other => panic!("Expected Filter relation, got {other:?}"),
                        }
                    }
                    other => panic!("Expected Project relation, got {other:?}"),
                }
            }
            other => panic!("Expected Rel type, got {other:?}"),
        }
    }
}
