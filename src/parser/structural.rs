//! Parser for the structural part of the Substrait file format.
//!
//! This is the overall parser for parsing the text format. It is responsible
//! for tracking which section of the file we are currently parsing, and parsing
//! each line separately.
//!
//! It delegates typed relation parsing to `parser::relations` and extension
//! line parsing to `parser::extensions`.

use std::fmt;

use substrait::proto::rel::RelType;
use substrait::proto::{Plan, PlanRel, Rel, RelRoot, plan_rel};

use crate::extensions::{ExtensionRegistry, SimpleExtensions, simple};
use crate::parser::ParseFragment;
use crate::parser::common::{MessageParseError, parse_typed, rules};
use crate::parser::convert::{LowerWith, Name, ParseCtx, collect_first_rest};
use crate::parser::errors::{ParseContext, ParseError, ParseResult};
use crate::parser::extensions::{
    ExtensionInvocation, ExtensionParseError, ExtensionParser, parse_extension_invocation_node,
};
use crate::parser::relations::ast::StandardRelationLine;
use crate::parser::relations::{
    LowerStandardInput, RelationParsingContext, parse_standard_relation,
};

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

#[derive(Debug, Clone)]
enum LineKind {
    Root { names: Vec<Name> },
    Standard(Box<StandardRelationLine>),
    Extension(ExtensionInvocation),
}

/// Parse-only representation of one line before tree construction.
#[derive(Debug, Clone)]
struct ParsedLine {
    depth: usize,
    line_no: i64,
    line: String,
    kind: LineKind,
}

/// Represents a line in the [`Plan`] tree structure before it's converted to a
/// relation. This allows us to build the tree structure first, then convert to
/// relations with proper parent-child relationships.
#[derive(Debug, Clone)]
pub struct LineNode {
    pub line: String,
    pub line_no: i64,
    kind: LineKind,
    pub children: Vec<LineNode>,
}

impl LineNode {
    pub fn context(&self) -> ParseContext {
        ParseContext::new(self.line_no, self.line.clone())
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

    fn push(&mut self, parsed: ParsedLine) -> Result<(), ParseError> {
        let ParsedLine {
            depth,
            line_no,
            line,
            kind,
        } = parsed;
        let node = LineNode {
            line,
            line_no,
            kind,
            children: Vec::new(),
        };

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
                    MessageParseError::invalid_line(
                        "relation",
                        &node.line,
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

/// Parse-only phase: ingest plan lines into semantic [`LineNode`] trees.
#[derive(Debug, Clone, Default)]
pub struct LineIngestor {
    tree: TreeBuilder,
}

impl LineIngestor {
    fn relation_to_kind(
        &self,
        extensions: &SimpleExtensions,
        relation: &rules::relation<'_>,
        context: &ParseContext,
    ) -> Result<LineKind, ParseError> {
        if let Some(extension_relation) = relation.extension_relation() {
            return Ok(LineKind::Extension(
                parse_extension_invocation_node(extension_relation)
                    .map_err(|e| ParseError::Plan(context.clone(), e))?,
            ));
        }

        let cx = ParseCtx { extensions };
        let standard = parse_standard_relation(&cx, relation)
            .map_err(|e| ParseError::Plan(context.clone(), e))?;
        Ok(LineKind::Standard(Box::new(standard)))
    }

    fn parse_relation_line(
        &self,
        extensions: &SimpleExtensions,
        depth: usize,
        line: &str,
        line_no: i64,
    ) -> Result<ParsedLine, ParseError> {
        let context = ParseContext::new(line_no, line.to_string());
        let relation = parse_typed::<rules::relation<'_>>(line, "relation")
            .map_err(|e| ParseError::Plan(context.clone(), e))?;
        let kind = self.relation_to_kind(extensions, &relation, &context)?;

        Ok(ParsedLine {
            depth,
            line: relation.span.as_str().to_string(),
            line_no,
            kind,
        })
    }

    fn parse_top_level_line(
        &self,
        extensions: &SimpleExtensions,
        depth: usize,
        line: &str,
        line_no: i64,
    ) -> Result<ParsedLine, ParseError> {
        let top = parse_typed::<rules::top_level_relation<'_>>(line, "top_level_relation")
            .map_err(|e| ParseError::Plan(ParseContext::new(line_no, line.to_string()), e))?;

        if let Some(root_relation) = top.root_relation() {
            let root_names = root_relation.root_name_list();
            let names = if let Some((first, rest)) = root_names.name() {
                collect_first_rest(Name::from(first), rest.into_iter().map(Name::from))
            } else {
                Vec::new()
            };

            return Ok(ParsedLine {
                depth,
                line: root_relation.span.as_str().to_string(),
                line_no,
                kind: LineKind::Root { names },
            });
        }

        let relation = top
            .relation()
            .expect("top_level_relation must be root or relation");

        let context = ParseContext::new(line_no, line.to_string());
        let kind = self.relation_to_kind(extensions, relation, &context)?;
        Ok(ParsedLine {
            depth,
            line: relation.span.as_str().to_string(),
            line_no,
            kind,
        })
    }

    fn parse_indented_line(
        &self,
        extensions: &SimpleExtensions,
        line: IndentedLine<'_>,
        line_no: i64,
    ) -> Result<ParsedLine, ParseError> {
        let IndentedLine(depth, line) = line;
        if depth == 0 {
            self.parse_top_level_line(extensions, depth, line, line_no)
        } else {
            self.parse_relation_line(extensions, depth, line, line_no)
        }
    }

    pub fn ingest(
        &mut self,
        extensions: &SimpleExtensions,
        line: IndentedLine<'_>,
        line_no: i64,
    ) -> Result<(), ParseError> {
        let parsed = self.parse_indented_line(extensions, line, line_no)?;
        self.tree.push(parsed)
    }

    fn finish(mut self) -> Vec<LineNode> {
        self.tree.finish()
    }
}

/// Lowering phase: convert semantic [`LineNode`] trees into protobuf plan relations.
#[derive(Debug, Clone, Copy)]
struct PlanLowerer<'a> {
    extensions: &'a SimpleExtensions,
    registry: &'a ExtensionRegistry,
}

impl<'a> PlanLowerer<'a> {
    fn new(extensions: &'a SimpleExtensions, registry: &'a ExtensionRegistry) -> Self {
        Self {
            extensions,
            registry,
        }
    }

    /// Parse extension relations.
    #[allow(clippy::vec_box)]
    fn lower_extension_relation(
        &self,
        line_no: i64,
        line: &str,
        extension_invocation: ExtensionInvocation,
        child_relations: Vec<Box<Rel>>,
    ) -> Result<Rel, ParseError> {
        let ExtensionInvocation {
            name,
            args: extension_args,
        } = extension_invocation;

        let child_count = child_relations.len();
        extension_args
            .relation_type
            .validate_child_count(child_count)
            .map_err(|e| {
                ParseError::Plan(
                    ParseContext::new(line_no, line.to_string()),
                    MessageParseError::invalid_line("extension_relation", line, e),
                )
            })?;

        let context = RelationParsingContext {
            registry: self.registry,
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
                    MessageParseError::invalid_line("extension_relation", line, e),
                )
            })
    }

    /// Convert a LineNode into a Substrait Rel.
    fn build_rel(&self, node: LineNode) -> Result<Rel, ParseError> {
        let line = node.line.clone();
        let line_no = node.line_no;
        let context = node.context();

        let child_relations = node
            .children
            .into_iter()
            .map(|c| self.build_rel(c).map(Box::new))
            .collect::<Result<Vec<Box<Rel>>, ParseError>>()?;

        let input_field_count = child_relations
            .iter()
            .map(|r| get_input_field_count(r.as_ref()))
            .reduce(|a, b| a + b)
            .unwrap_or(0);

        match node.kind {
            LineKind::Root { .. } => Err(ParseError::Plan(
                context,
                MessageParseError::invalid_line(
                    "root_relation",
                    &line,
                    "Root relation cannot be nested as a child",
                ),
            )),
            LineKind::Extension(invocation) => {
                self.lower_extension_relation(line_no, &line, invocation, child_relations)
            }
            LineKind::Standard(standard) => standard
                .lower_with(
                    &ParseCtx {
                        extensions: self.extensions,
                    },
                    LowerStandardInput {
                        line_text: &line,
                        input_children: child_relations,
                        input_field_count,
                    },
                )
                .map_err(|e| ParseError::Plan(ParseContext::new(line_no, line), e)),
        }
    }

    /// Build a tree of relations.
    fn build_plan_rel(&self, mut node: LineNode) -> Result<PlanRel, ParseError> {
        let context = node.context();
        match node.kind {
            LineKind::Root { names } => {
                let child = match node.children.len() {
                    1 => self.build_rel(node.children.pop().expect("len checked above"))?,
                    n => {
                        return Err(ParseError::Plan(
                            context,
                            MessageParseError::invalid_line(
                                "root_relation",
                                &node.line,
                                format!("Root relation must have exactly one child, found {n}"),
                            ),
                        ));
                    }
                };

                Ok(PlanRel {
                    rel_type: Some(plan_rel::RelType::Root(RelRoot {
                        names: names.into_iter().map(|name| name.0).collect(),
                        input: Some(child),
                    })),
                })
            }
            LineKind::Standard(_) | LineKind::Extension(_) => {
                let rel = self.build_rel(node)?;
                Ok(PlanRel {
                    rel_type: Some(plan_rel::RelType::Rel(rel)),
                })
            }
        }
    }

    /// Build all the trees.
    fn build(self, nodes: Vec<LineNode>) -> Result<Vec<PlanRel>, ParseError> {
        nodes
            .into_iter()
            .map(|n| self.build_plan_rel(n))
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
    fragment_extensions: SimpleExtensions,
    line_ingestor: LineIngestor,
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
            fragment_extensions: SimpleExtensions::new(),
            line_ingestor: LineIngestor::default(),
        }
    }

    /// Configure the parser to use the specified extension registry.
    pub fn with_extension_registry(mut self, registry: ExtensionRegistry) -> Self {
        self.extension_registry = registry;
        self
    }

    /// Configure simple extensions used by [`Self::parse_fragment`].
    pub fn with_simple_extensions(mut self, extensions: SimpleExtensions) -> Self {
        self.fragment_extensions = extensions;
        self
    }

    /// Parse a non-plan Substrait fragment value from text.
    pub fn parse_fragment<T: ParseFragment>(&self, input: &str) -> Result<T, MessageParseError> {
        T::parse_fragment(&self.fragment_extensions, input)
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
            State::Plan => self.line_ingestor.ingest(
                self.extension_parser.extensions(),
                indented_line,
                line_no,
            ),
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
                MessageParseError::invalid_line(
                    "initial",
                    l,
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
            line_ingestor,
            extension_parser,
            extension_registry,
            ..
        } = self;

        let extensions = extension_parser.extensions();
        let nodes = line_ingestor.finish();
        let root_relations = PlanLowerer::new(extensions, &extension_registry).build(nodes)?;

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
    use super::*;

    #[test]
    fn parse_root_relation_plan() {
        let plan = r#"=== Plan
Root[result]
  Project[$0]
    Read[my.table => a:i32]
"#;

        let parsed = Parser::parse(plan).unwrap();
        assert_eq!(parsed.relations.len(), 1);

        let root_rel = parsed.relations[0].rel_type.as_ref().unwrap();
        let root = match root_rel {
            plan_rel::RelType::Root(root) => root,
            other => panic!("Expected root relation, got {other:?}"),
        };
        assert_eq!(root.names, vec!["result"]);
    }

    #[test]
    fn parse_extension_plan_comment_url() {
        let input = r#"=== Extensions
URNs:
  @  1: https://example.com/test // comment
Functions:
  # 10 @  1: fn
=== Plan
Read[t => a:i32]
"#;
        let parsed = Parser::parse(input).unwrap();
        assert_eq!(parsed.extension_urns.len(), 1);
    }

    #[test]
    fn parse_relation_line_stores_semantic_standard_payload() {
        let ingestor = LineIngestor::default();
        let node = ingestor
            .parse_indented_line(
                &SimpleExtensions::new(),
                IndentedLine::from("Read[t => a:i32]"),
                1,
            )
            .unwrap();

        match node.kind {
            LineKind::Standard(standard) => {
                assert!(matches!(*standard, StandardRelationLine::Read { .. }));
            }
            LineKind::Root { .. } | LineKind::Extension(_) => {
                panic!("expected standard relation payload")
            }
        }
    }

    #[test]
    fn parse_relation_line_stores_semantic_extension_payload() {
        let ingestor = LineIngestor::default();
        let node = ingestor
            .parse_indented_line(
                &SimpleExtensions::new(),
                IndentedLine::from("ExtensionLeaf:MyLeaf[]"),
                1,
            )
            .unwrap();

        match node.kind {
            LineKind::Extension(invocation) => {
                assert_eq!(invocation.name, "MyLeaf");
            }
            LineKind::Root { .. } | LineKind::Standard(_) => {
                panic!("expected extension relation payload")
            }
        }
    }
}
