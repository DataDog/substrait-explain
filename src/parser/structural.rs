//! Parser for the structural part of the Substrait file format.
//!
//! This is the overall parser for parsing the text format. It is responsible
//! for tracking which section of the file we are currently parsing, and parsing
//! each line separately.

use std::fmt;

use substrait::proto::rel::RelType;
use substrait::proto::{Plan, PlanRel, Rel, RelRoot, plan_rel};

use crate::extensions::{ExtensionRegistry, SimpleExtensions, simple};
use crate::parser::common::{MessageParseError, line_span, parse_typed, rules};
use crate::parser::errors::{ParseContext, ParseError, ParseResult};
use crate::parser::expressions::parse_name_node;
use crate::parser::extensions::{
    ExtensionInvocation, ExtensionParseError, ExtensionParser, parse_extension_invocation,
};
use crate::parser::relations::{RelationParsingContext, parse_standard_relation_from_line};

pub const PLAN_HEADER: &str = "=== Plan";

fn strip_inline_comment(line: &str) -> &str {
    let bytes = line.as_bytes();
    let mut in_single = false;
    let mut in_double = false;
    let mut escaped = false;
    let mut i = 0;
    while i + 1 < bytes.len() {
        let b = bytes[i];
        if escaped {
            escaped = false;
            i += 1;
            continue;
        }
        match b {
            b'\\' if in_single || in_double => {
                escaped = true;
            }
            b'\'' if !in_double => in_single = !in_single,
            b'"' if !in_single => in_double = !in_double,
            b'/' if !in_single
                && !in_double
                && bytes[i + 1] == b'/'
                && (i == 0 || bytes[i - 1].is_ascii_whitespace()) =>
            {
                return line[..i].trim_end();
            }
            _ => {}
        }
        i += 1;
    }
    line
}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NodeKind {
    Relation { is_extension: bool },
    RootRelation,
}

/// Represents a line in the [`Plan`] tree structure before it's converted to a
/// relation. This allows us to build the tree structure first, then convert to
/// relations with proper parent-child relationships.
#[derive(Debug, Clone)]
pub struct LineNode {
    pub line: String,
    pub line_no: i64,
    kind: NodeKind,
    pub children: Vec<LineNode>,
}

impl LineNode {
    pub fn context(&self) -> ParseContext {
        ParseContext::new(self.line_no, self.line.clone())
    }

    pub fn parse(line: &str, line_no: i64) -> Result<Self, ParseError> {
        let relation = parse_typed::<rules::relation<'_>>(line, "relation")
            .map_err(|e| ParseError::Plan(ParseContext::new(line_no, line.to_string()), e))?;

        Ok(Self {
            line: relation.span.as_str().to_string(),
            line_no,
            kind: NodeKind::Relation {
                is_extension: relation.extension_relation().is_some(),
            },
            children: Vec::new(),
        })
    }

    /// Parse the root relation of a plan, at depth 0.
    pub fn parse_root(line: &str, line_no: i64) -> Result<Self, ParseError> {
        let top = parse_typed::<rules::top_level_relation<'_>>(line, "top_level_relation")
            .map_err(|e| ParseError::Plan(ParseContext::new(line_no, line.to_string()), e))?;

        if let Some(root_relation) = top.root_relation() {
            return Ok(Self {
                line: root_relation.span.as_str().to_string(),
                line_no,
                kind: NodeKind::RootRelation,
                children: Vec::new(),
            });
        }

        let relation = top
            .relation()
            .expect("top_level_relation must be root or relation");
        Ok(Self {
            line: relation.span.as_str().to_string(),
            line_no,
            kind: NodeKind::Relation {
                is_extension: relation.extension_relation().is_some(),
            },
            children: Vec::new(),
        })
    }
}

/// Helper function to get the number of input fields from a relation.
/// This is needed for Project relations to calculate output mapping indices.
fn get_input_field_count(rel: &Rel) -> usize {
    match &rel.rel_type {
        Some(RelType::Read(read_rel)) => read_rel
            .base_schema
            .as_ref()
            .and_then(|schema| schema.r#struct.as_ref())
            .map(|struct_| struct_.types.len())
            .unwrap_or(0),
        Some(RelType::Filter(filter_rel)) => filter_rel
            .input
            .as_ref()
            .map(|input| get_input_field_count(input))
            .unwrap_or(0),
        Some(RelType::Project(project_rel)) => project_rel
            .input
            .as_ref()
            .map(|input| get_input_field_count(input))
            .unwrap_or(0),
        _ => 0,
    }
}

#[derive(Copy, Clone, Debug)]
pub enum State {
    Initial,
    Extensions,
    Plan,
}

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

// An in-progress tree builder, building the tree of relations.
#[derive(Debug, Clone, Default)]
pub struct TreeBuilder {
    current: Option<LineNode>,
    completed: Vec<LineNode>,
}

impl TreeBuilder {
    /// Traverse down the tree, always taking the last child at each level, until reaching the specified depth.
    pub fn get_at_depth(&mut self, depth: usize) -> Option<&mut LineNode> {
        let mut node = self.current.as_mut()?;
        for _ in 0..depth {
            node = node.children.last_mut()?;
        }
        Some(node)
    }

    pub fn add_line(&mut self, depth: usize, node: LineNode) -> Result<(), ParseError> {
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
                        line_span(&node.line),
                        format!("No parent found for depth {depth}"),
                    ),
                ));
            }
            Some(parent) => parent,
        };

        parent.children.push(node);
        Ok(())
    }

    /// End of input - move any remaining nodes from stack to completed and
    /// return any trees in progress. Resets the builder to its initial state
    /// (empty)
    pub fn finish(&mut self) -> Vec<LineNode> {
        if let Some(node) = self.current.take() {
            self.completed.push(node);
        }
        std::mem::take(&mut self.completed)
    }
}

// Relation parsing component - handles converting LineNodes to Relations
#[derive(Debug, Clone, Default)]
pub struct RelationParser {
    tree: TreeBuilder,
}

impl RelationParser {
    /// Parse extension relations.
    #[allow(clippy::vec_box)]
    fn parse_extension_relation(
        &self,
        registry: &ExtensionRegistry,
        line_no: i64,
        line: &str,
        child_relations: Vec<Box<Rel>>,
    ) -> Result<Rel, ParseError> {
        let pair_span = line_span(line);

        let ExtensionInvocation {
            name,
            args: extension_args,
        } = parse_extension_invocation(line)
            .map_err(|e| ParseError::Plan(ParseContext::new(line_no, line.to_string()), e))?;

        let child_count = child_relations.len();
        extension_args
            .relation_type
            .validate_child_count(child_count)
            .map_err(|e| {
                ParseError::Plan(
                    ParseContext::new(line_no, line.to_string()),
                    MessageParseError::invalid("extension_relation", pair_span, e),
                )
            })?;

        let context = RelationParsingContext {
            registry,
            line_no,
            line,
        };

        let detail = context.resolve_extension_detail(&name, &extension_args)?;

        extension_args
            .relation_type
            .create_rel(detail, child_relations)
            .map_err(|e| {
                ParseError::Plan(
                    ParseContext::new(line_no, line.to_string()),
                    MessageParseError::invalid("extension_relation", pair_span, e),
                )
            })
    }

    /// Convert a LineNode into a Substrait Rel.
    fn build_rel(
        &self,
        extensions: &SimpleExtensions,
        registry: &ExtensionRegistry,
        node: LineNode,
    ) -> Result<Rel, ParseError> {
        let node_kind = node.kind;
        let node_line = node.line.clone();
        let node_line_no = node.line_no;
        let node_context = node.context();
        let child_relations = node
            .children
            .into_iter()
            .map(|c| self.build_rel(extensions, registry, c).map(Box::new))
            .collect::<Result<Vec<Box<Rel>>, ParseError>>()?;

        let input_field_count = child_relations
            .iter()
            .map(|r| get_input_field_count(r.as_ref()))
            .reduce(|a, b| a + b)
            .unwrap_or(0);

        match node_kind {
            NodeKind::RootRelation => Err(ParseError::Plan(
                node_context,
                MessageParseError::invalid(
                    "root_relation",
                    line_span(&node_line),
                    "Root relation cannot be nested as a child",
                ),
            )),
            NodeKind::Relation { is_extension: true } => {
                self.parse_extension_relation(registry, node_line_no, &node_line, child_relations)
            }
            NodeKind::Relation {
                is_extension: false,
            } => {
                let parsed = parse_standard_relation_from_line(
                    extensions,
                    &node_line,
                    child_relations,
                    input_field_count,
                )
                .map_err(|e| {
                    ParseError::Plan(ParseContext::new(node_line_no, node_line.clone()), e)
                })?;

                match parsed {
                    Some(rel) => Ok(rel),
                    None => Err(ParseError::Plan(
                        ParseContext::new(node_line_no, node_line.clone()),
                        MessageParseError::invalid(
                            "relation",
                            line_span(&node_line),
                            "Unexpected extension relation while parsing standard relation",
                        ),
                    )),
                }
            }
        }
    }

    /// Build a tree of relations.
    fn build_plan_rel(
        &self,
        extensions: &SimpleExtensions,
        registry: &ExtensionRegistry,
        mut node: LineNode,
    ) -> Result<PlanRel, ParseError> {
        match node.kind {
            NodeKind::Relation { .. } => {
                let rel = self.build_rel(extensions, registry, node)?;
                Ok(PlanRel {
                    rel_type: Some(plan_rel::RelType::Rel(rel)),
                })
            }
            NodeKind::RootRelation => {
                let context = node.context();
                let span = line_span(&node.line);

                let root = parse_typed::<rules::root_relation<'_>>(&node.line, "root_relation")
                    .map_err(|e| ParseError::Plan(context.clone(), e))?;
                let root_names = root.root_name_list();

                let mut names = Vec::new();
                if let Some((first, rest)) = root_names.name() {
                    names.push(parse_name_node(first).0);
                    for name in rest {
                        names.push(parse_name_node(name).0);
                    }
                }

                let child = match node.children.len() {
                    1 => self.build_rel(extensions, registry, node.children.pop().unwrap())?,
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

                Ok(PlanRel {
                    rel_type: Some(plan_rel::RelType::Root(RelRoot {
                        names,
                        input: Some(child),
                    })),
                })
            }
        }
    }

    /// Build all the trees.
    fn build(
        mut self,
        extensions: &SimpleExtensions,
        registry: &ExtensionRegistry,
    ) -> Result<Vec<PlanRel>, ParseError> {
        let nodes = self.tree.finish();
        nodes
            .into_iter()
            .map(|n| self.build_plan_rel(extensions, registry, n))
            .collect::<Result<Vec<PlanRel>, ParseError>>()
    }
}

/// A parser for Substrait query plans in text format.
#[derive(Debug)]
pub struct Parser {
    line_no: i64,
    state: State,
    extension_parser: ExtensionParser,
    extension_registry: ExtensionRegistry,
    relation_parser: RelationParser,
}

impl Default for Parser {
    fn default() -> Self {
        Self::new()
    }
}

impl Parser {
    /// Parse a Substrait plan from text format.
    pub fn parse(input: &str) -> ParseResult {
        Self::new().parse_plan(input)
    }

    /// Create a new parser with default configuration.
    pub fn new() -> Self {
        Self {
            line_no: 1,
            state: State::Initial,
            extension_parser: ExtensionParser::default(),
            extension_registry: ExtensionRegistry::new(),
            relation_parser: RelationParser::default(),
        }
    }

    /// Configure the parser to use the specified extension registry.
    pub fn with_extension_registry(mut self, registry: ExtensionRegistry) -> Self {
        self.extension_registry = registry;
        self
    }

    /// Parse a Substrait plan with the current parser configuration.
    pub fn parse_plan(mut self, input: &str) -> ParseResult {
        for line in input.lines() {
            self.parse_line(line)?;
            self.line_no += 1;
        }

        self.build_plan()
    }

    /// Parse a single line of input.
    fn parse_line(&mut self, line: &str) -> Result<(), ParseError> {
        let line = strip_inline_comment(line);
        if line.trim().is_empty() {
            return Ok(());
        }

        let indented_line = IndentedLine::from(line);
        let line_no = self.line_no;
        let ctx = || ParseContext::new(line_no, line.to_string());

        match self.state {
            State::Initial => self.parse_initial(indented_line),
            State::Extensions => self
                .parse_extensions(indented_line)
                .map_err(|e| ParseError::Extension(ctx(), e)),
            State::Plan => {
                let IndentedLine(depth, line_str) = indented_line;

                let node = if depth == 0 {
                    LineNode::parse_root(line_str, line_no)?
                } else {
                    LineNode::parse(line_str, line_no)?
                };

                self.relation_parser.tree.add_line(depth, node)
            }
        }
    }

    /// Parse the initial line(s) of the input, which is either a blank line or
    /// the extensions or plan header.
    fn parse_initial(&mut self, line: IndentedLine) -> Result<(), ParseError> {
        match line {
            IndentedLine(0, l) if l.trim().is_empty() => Ok(()),
            IndentedLine(0, simple::EXTENSIONS_HEADER) => {
                self.state = State::Extensions;
                Ok(())
            }
            IndentedLine(0, PLAN_HEADER) => {
                self.state = State::Plan;
                Ok(())
            }
            IndentedLine(_, l) => Err(ParseError::Initial(
                ParseContext::new(self.line_no, l.to_string()),
                MessageParseError::invalid(
                    "initial",
                    line_span(l),
                    format!("Unknown initial line: {l:?}"),
                ),
            )),
        }
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

    /// Build the plan from the parser state with warning collection.
    fn build_plan(self) -> Result<Plan, ParseError> {
        let Parser {
            relation_parser,
            extension_parser,
            extension_registry,
            ..
        } = self;

        let extensions = extension_parser.extensions();
        let root_relations = relation_parser.build(extensions, &extension_registry)?;

        Ok(Plan {
            extension_urns: extensions.to_extension_urns(),
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
            .add_extension_urn("/urn/common".to_string(), 1)
            .unwrap();
        expected_extensions
            .add_extension_urn("/urn/specific_funcs".to_string(), 2)
            .unwrap();
        expected_extensions
            .add_extension(ExtensionKind::Function, 1, 10, "func_a".to_string())
            .unwrap();
        expected_extensions
            .add_extension(ExtensionKind::Function, 2, 11, "func_b_special".to_string())
            .unwrap();

        let mut parser = ExtensionParser::default();
        let input_block = r#"
URNs:
  @  1: /urn/common
  @  2: /urn/specific_funcs
Functions:
  # 10 @  1: func_a
  # 11 @  2: func_b_special
"#;

        for line_str in input_block.trim().lines() {
            parser.parse_line(IndentedLine::from(line_str)).unwrap();
        }

        assert_eq!(*parser.extensions(), expected_extensions);
        assert_eq!(
            parser.state(),
            ExtensionParserState::ExtensionDeclarations(ExtensionKind::Function)
        );
    }

    #[test]
    fn test_parse_relation_tree() {
        let plan = r#"=== Plan
Project[$0, $1, 42, 84]
  Filter[$2 => $0, $1]
    Read[my.table => a:i32, b:string?, c:boolean]
"#;

        let mut parser = Parser::default();
        for line in plan.lines() {
            parser.parse_line(line).unwrap();
        }

        let plan = parser.build_plan().unwrap();
        let root_rel = &plan.relations[0].rel_type;
        let first_rel = match root_rel {
            Some(plan_rel::RelType::Rel(rel)) => rel,
            _ => panic!("Expected Rel type, got {root_rel:?}"),
        };

        let project = match &first_rel.rel_type {
            Some(RelType::Project(p)) => p,
            other => panic!("Expected Project at root, got {other:?}"),
        };

        let filter_input = project.input.as_ref().unwrap();
        match &filter_input.rel_type {
            Some(RelType::Filter(filter)) => {
                let read_input = filter.input.as_ref().unwrap();
                match &read_input.rel_type {
                    Some(RelType::Read(_)) => {}
                    other => panic!("Expected Read relation, got {other:?}"),
                }
            }
            other => panic!("Expected Filter relation, got {other:?}"),
        }
    }

    #[test]
    fn test_parse_root_relation() {
        let plan = r#"=== Plan
Root[result]
  Project[$0, $1]
    Read[my.table => a:i32, b:string?]
"#;

        let parsed = Parser::parse(plan).unwrap();
        assert_eq!(parsed.relations.len(), 1);

        let root_rel = &parsed.relations[0].rel_type;
        let rel_root = match root_rel {
            Some(plan_rel::RelType::Root(rel_root)) => rel_root,
            other => panic!("Expected Root type, got {other:?}"),
        };

        assert_eq!(rel_root.names, vec!["result"]);
    }

    #[test]
    fn test_parse_full_plan() {
        let input = r#"
=== Extensions
URNs:
  @  1: /urn/common
Functions:
  # 10 @  1: func_a

=== Plan
Project[$0, 42]
  Read[my.table => a:i32, b:string?]
"#;

        let plan = Parser::parse(input).unwrap();

        assert_eq!(plan.extension_urns.len(), 1);
        assert_eq!(plan.extensions.len(), 1);
        assert_eq!(plan.relations.len(), 1);

        let func = &plan.extensions[0];
        match &func.mapping_type {
            Some(MappingType::ExtensionFunction(f)) => {
                assert_eq!(f.function_anchor, 10);
                assert_eq!(f.extension_urn_reference, 1);
                assert_eq!(f.name, "func_a");
            }
            other => panic!("Expected ExtensionFunction, got {other:?}"),
        }
    }
}
