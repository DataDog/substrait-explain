//! Parser for the structural part of the Substrait file format.

use std::borrow::Cow;
use std::fmt;

use substrait::proto::rel::RelType;
use substrait::proto::{Plan, PlanRel, Rel, RelRoot, plan_rel};

use crate::extensions::{ExtensionRegistry, SimpleExtensions, simple};
use crate::parser::errors::{
    MessageParseError, ParseContext, ParseError, ParseResult, SyntaxErrorDetail,
};
use crate::parser::extensions::{ExtensionParseError, ExtensionParser};
use crate::parser::{ast, lalrpop_line, lower};

pub const PLAN_HEADER: &str = "=== Plan";

/// Strip `//` comments while preserving comment markers inside quoted strings
/// and URL-like tokens such as `https://...`.
fn strip_inline_comment(line: &str) -> Cow<'_, str> {
    let mut in_single = false;
    let mut in_double = false;
    let mut escaped = false;

    for (idx, ch) in line.char_indices() {
        if escaped {
            escaped = false;
            continue;
        }

        if (in_single || in_double) && ch == '\\' {
            escaped = true;
            continue;
        }

        if ch == '\'' && !in_double {
            in_single = !in_single;
            continue;
        }

        if ch == '"' && !in_single {
            in_double = !in_double;
            continue;
        }

        if !in_single && !in_double && line[idx..].starts_with("//") {
            // Treat `//` as comment start only at beginning of line or when
            // preceded by whitespace. This keeps tokens like `https://...`
            // intact in extension URNs.
            let starts_comment =
                idx == 0 || line[..idx].chars().last().is_some_and(char::is_whitespace);
            if starts_comment {
                return Cow::Owned(line[..idx].trim_end().to_string());
            }
        }
    }

    Cow::Borrowed(line)
}

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
pub enum LineData {
    Relation(ast::Relation),
    Root(Vec<String>),
}

#[derive(Debug, Clone)]
pub struct LineNode {
    pub data: LineData,
    pub line_no: i64,
    pub line: String,
    pub children: Vec<LineNode>,
}

impl LineNode {
    pub fn context(&self) -> ParseContext {
        ParseContext {
            line_no: self.line_no,
            line: self.line.clone(),
        }
    }

    pub fn parse(line: &str, line_no: i64) -> Result<Self, ParseError> {
        let relation = lalrpop_line::parse_relation(line)
            .map_err(|e| parse_syntax_error("relation", line_no, line, e))?;

        Ok(Self {
            data: LineData::Relation(relation),
            line_no,
            line: line.to_string(),
            children: Vec::new(),
        })
    }

    pub fn parse_root(line: &str, line_no: i64) -> Result<Self, ParseError> {
        match lalrpop_line::parse_root_names(line) {
            Ok(names) => {
                return Ok(Self {
                    data: LineData::Root(names),
                    line_no,
                    line: line.to_string(),
                    children: Vec::new(),
                });
            }
            Err(root_error) if starts_with_root_keyword(line) => {
                return Err(parse_syntax_error("root_names", line_no, line, root_error));
            }
            Err(_) => {}
        }

        let relation = lalrpop_line::parse_relation(line)
            .map_err(|e| parse_syntax_error("top_level_relation", line_no, line, e))?;

        Ok(Self {
            data: LineData::Relation(relation),
            line_no,
            line: line.to_string(),
            children: Vec::new(),
        })
    }
}

fn starts_with_root_keyword(line: &str) -> bool {
    let trimmed = line.trim_start();
    let Some(rest) = trimmed.strip_prefix("Root") else {
        return false;
    };
    rest.starts_with('[') || rest.starts_with(char::is_whitespace)
}

fn parse_syntax_error(
    target: &'static str,
    line_no: i64,
    line: &str,
    detail: SyntaxErrorDetail,
) -> ParseError {
    ParseError::Syntax {
        target,
        context: ParseContext::new(line_no, line.to_string()),
        detail: Box::new(detail),
    }
}

fn get_output_field_count(rel: &Rel) -> usize {
    match &rel.rel_type {
        Some(RelType::Read(read_rel)) => read_rel
            .base_schema
            .as_ref()
            .and_then(|schema| schema.r#struct.as_ref())
            .map(|s| s.types.len())
            .unwrap_or_else(|| {
                read_rel
                    .base_schema
                    .as_ref()
                    .map(|s| s.names.len())
                    .unwrap_or(0)
            }),
        Some(RelType::Filter(filter_rel)) => rel_common_count(
            filter_rel
                .common
                .as_ref()
                .and_then(|c| c.emit_kind.as_ref()),
            filter_rel
                .input
                .as_ref()
                .map(|input| get_output_field_count(input))
                .unwrap_or(0),
        ),
        Some(RelType::Project(project_rel)) => rel_common_count(
            project_rel
                .common
                .as_ref()
                .and_then(|c| c.emit_kind.as_ref()),
            project_rel
                .input
                .as_ref()
                .map(|input| get_output_field_count(input))
                .unwrap_or(0)
                + project_rel.expressions.len(),
        ),
        Some(RelType::Aggregate(aggregate_rel)) => rel_common_count(
            aggregate_rel
                .common
                .as_ref()
                .and_then(|c| c.emit_kind.as_ref()),
            aggregate_rel.grouping_expressions.len() + aggregate_rel.measures.len(),
        ),
        Some(RelType::Sort(sort_rel)) => rel_common_count(
            sort_rel.common.as_ref().and_then(|c| c.emit_kind.as_ref()),
            sort_rel
                .input
                .as_ref()
                .map(|input| get_output_field_count(input))
                .unwrap_or(0),
        ),
        Some(RelType::Fetch(fetch_rel)) => rel_common_count(
            fetch_rel.common.as_ref().and_then(|c| c.emit_kind.as_ref()),
            fetch_rel
                .input
                .as_ref()
                .map(|input| get_output_field_count(input))
                .unwrap_or(0),
        ),
        Some(RelType::Join(join_rel)) => rel_common_count(
            join_rel.common.as_ref().and_then(|c| c.emit_kind.as_ref()),
            join_rel
                .left
                .as_ref()
                .map(|left| get_output_field_count(left))
                .unwrap_or(0)
                + join_rel
                    .right
                    .as_ref()
                    .map(|right| get_output_field_count(right))
                    .unwrap_or(0),
        ),
        Some(RelType::ExtensionLeaf(ext_rel)) => rel_common_count(
            ext_rel.common.as_ref().and_then(|c| c.emit_kind.as_ref()),
            0,
        ),
        Some(RelType::ExtensionSingle(ext_rel)) => rel_common_count(
            ext_rel.common.as_ref().and_then(|c| c.emit_kind.as_ref()),
            ext_rel
                .input
                .as_ref()
                .map(|input| get_output_field_count(input))
                .unwrap_or(0),
        ),
        Some(RelType::ExtensionMulti(ext_rel)) => rel_common_count(
            ext_rel.common.as_ref().and_then(|c| c.emit_kind.as_ref()),
            ext_rel.inputs.iter().map(get_output_field_count).sum(),
        ),
        _ => 0,
    }
}

fn rel_common_count(
    emit_kind: Option<&substrait::proto::rel_common::EmitKind>,
    default: usize,
) -> usize {
    match emit_kind {
        // When Emit is explicit, output mapping is authoritative.
        Some(substrait::proto::rel_common::EmitKind::Emit(emit)) => emit.output_mapping.len(),
        Some(substrait::proto::rel_common::EmitKind::Direct(_)) => default,
        None => default,
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

#[derive(Debug, Clone, Default)]
pub struct TreeBuilder {
    current: Option<LineNode>,
    completed: Vec<LineNode>,
}

impl TreeBuilder {
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
                        format!("No parent found for depth {depth}"),
                    ),
                ));
            }
            Some(parent) => parent,
        };

        parent.children.push(node);
        Ok(())
    }

    pub fn finish(&mut self) -> Vec<LineNode> {
        if let Some(node) = self.current.take() {
            self.completed.push(node);
        }
        std::mem::take(&mut self.completed)
    }
}

#[derive(Debug, Clone, Default)]
pub struct RelationParser {
    tree: TreeBuilder,
}

impl RelationParser {
    pub fn parse_line(&mut self, line: IndentedLine<'_>, line_no: i64) -> Result<(), ParseError> {
        let IndentedLine(depth, line) = line;

        let node = if depth == 0 {
            LineNode::parse_root(line, line_no)?
        } else {
            LineNode::parse(line, line_no)?
        };

        self.tree.add_line(depth, node)
    }

    fn build_rel(
        &self,
        extensions: &SimpleExtensions,
        registry: &ExtensionRegistry,
        node: LineNode,
    ) -> Result<Rel, ParseError> {
        let context = node.context();
        let line = node.line.clone();
        let line_no = node.line_no;

        let child_relations = node
            .children
            .into_iter()
            .map(|c| self.build_rel(extensions, registry, c).map(Box::new))
            .collect::<Result<Vec<Box<Rel>>, ParseError>>()?;

        let input_field_count = child_relations
            .iter()
            .map(|r| get_output_field_count(r.as_ref()))
            .sum();

        let relation = match node.data {
            LineData::Relation(relation) => relation,
            LineData::Root(_) => {
                return Err(ParseError::Plan(
                    context,
                    MessageParseError::invalid(
                        "relation",
                        "Root cannot be used as non-root relation",
                    ),
                ));
            }
        };

        lower::lower_relation(
            extensions,
            registry,
            &relation,
            &line,
            line_no,
            child_relations,
            input_field_count,
        )
    }

    fn build_plan_rel(
        &self,
        extensions: &SimpleExtensions,
        registry: &ExtensionRegistry,
        mut node: LineNode,
    ) -> Result<PlanRel, ParseError> {
        let context = node.context();
        let data = node.data.clone();

        if let LineData::Relation(_) = data {
            let rel = self.build_rel(extensions, registry, node)?;
            return Ok(PlanRel {
                rel_type: Some(plan_rel::RelType::Rel(rel)),
            });
        }

        let LineData::Root(names) = data else {
            unreachable!()
        };

        let child = match node.children.len() {
            1 => self.build_rel(
                extensions,
                registry,
                node.children.pop().expect("one child"),
            )?,
            n => {
                return Err(ParseError::Plan(
                    context,
                    MessageParseError::invalid(
                        "root_relation",
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
    pub fn parse(input: &str) -> ParseResult {
        Self::new().parse_plan(input)
    }

    pub fn new() -> Self {
        Self {
            line_no: 1,
            state: State::Initial,
            extension_parser: ExtensionParser::default(),
            extension_registry: ExtensionRegistry::new(),
            relation_parser: RelationParser::default(),
        }
    }

    pub fn with_extension_registry(mut self, registry: ExtensionRegistry) -> Self {
        self.extension_registry = registry;
        self
    }

    pub fn parse_plan(mut self, input: &str) -> ParseResult {
        for raw_line in input.lines() {
            let line = strip_inline_comment(raw_line);
            if line.trim().is_empty() {
                self.line_no += 1;
                continue;
            }

            self.parse_line(line.as_ref(), raw_line)?;
            self.line_no += 1;
        }

        self.build_plan()
    }

    fn parse_line(&mut self, line: &str, original_line: &str) -> Result<(), ParseError> {
        let indented_line = IndentedLine::from(line);
        let line_no = self.line_no;
        let ctx = || ParseContext {
            line_no,
            line: original_line.to_string(),
        };

        match self.state {
            State::Initial => self.parse_initial(indented_line),
            State::Extensions => self
                .parse_extensions(indented_line)
                .map_err(|e| ParseError::Extension(ctx(), e)),
            State::Plan => self.relation_parser.parse_line(indented_line, line_no),
        }
    }

    fn parse_initial(&mut self, line: IndentedLine<'_>) -> Result<(), ParseError> {
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
                MessageParseError::invalid("initial", format!("Unknown initial line: {l:?}")),
            )),
        }
    }

    fn parse_extensions(&mut self, line: IndentedLine<'_>) -> Result<(), ExtensionParseError> {
        if line == IndentedLine(0, PLAN_HEADER) {
            self.state = State::Plan;
            return Ok(());
        }
        self.extension_parser.parse_line(line)
    }

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
    use super::*;

    #[test]
    fn parse_simple_plan() {
        let input = r#"
=== Plan
Root[a]
  Read[t => a:i32]
"#;

        let plan = Parser::parse(input).expect("parse plan");
        assert_eq!(plan.relations.len(), 1);
    }

    #[test]
    fn parse_plan_with_inline_comments() {
        let input = r#"
=== Plan
Root[a] // top-level result
  Read[t => a:i32] // leaf relation
"#;

        let plan = Parser::parse(input).expect("parse plan");
        assert_eq!(plan.relations.len(), 1);
    }

    #[test]
    fn parse_extensions_with_url_no_comment_strip() {
        let input = r#"
=== Extensions
URNs:
  @  1: https://example.com/functions.yaml // keep trailing comment
Functions:
  # 10 @  1: foo

=== Plan
Root[a]
  Project[foo($0)]
    Read[t => a:i32]
"#;

        let plan = Parser::parse(input).expect("parse plan");
        assert_eq!(plan.extension_urns.len(), 1);
        assert_eq!(plan.extensions.len(), 1);
        assert_eq!(plan.relations.len(), 1);
    }

    #[test]
    fn parse_root_reports_root_names_errors() {
        let input = r#"
=== Plan
Root[42]
  Read[t => a:i32]
"#;

        let err = Parser::parse(input).expect_err("parse should fail");
        match err {
            ParseError::Syntax {
                target, context, ..
            } => {
                assert_eq!(target, "root_names");
                assert_eq!(context.line_no, 3);
                assert_eq!(context.line, "Root[42]");
            }
            other => panic!("expected ParseError::Syntax, got {other:?}"),
        }
    }
}
