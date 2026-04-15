//! Parser for the structural part of the Substrait file format.
//!
//! This is the overall parser for parsing the text format. It is responsible
//! for tracking which section of the file we are currently parsing, and parsing
//! each line separately.

use std::fmt;

use pest::iterators::Pair;
use substrait::proto::expression::nested;
use substrait::proto::extensions::AdvancedExtension;
use substrait::proto::rel::RelType;
use substrait::proto::{
    AggregateRel, FetchRel, FilterRel, JoinRel, Plan, PlanRel, ProjectRel, ReadRel, Rel, RelRoot,
    SortRel, plan_rel,
};

use crate::extensions::{ExtensionRegistry, ExtensionType, SimpleExtensions, simple};
use crate::parser::common::{MessageParseError, ParsePair};
use crate::parser::errors::{ParseContext, ParseError, ParseResult};
use crate::parser::expressions::Name;
use crate::parser::extensions::{
    AdvExtInvocation, ExtensionInvocation, ExtensionParseError, ExtensionParser,
};
use crate::parser::relations::{
    ParsedVirtualReadRel, RelationAddenda, RelationParsingContext, parse_row_addendum,
};
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

/// An advanced-extension annotation (`+ Enh:Name[args]` or `+ Opt:Name[args]`)
/// that is attached to a relation node. The `pair` holds the `adv_extension`
/// grammar rule directly (already unwrapped from the outer `planNode`).
#[derive(Debug, Clone)]
pub struct AdvExt<'a> {
    pub pair: Pair<'a, Rule>, // Rule::adv_extension
    pub line_no: i64,
}

/// A row addendum (`+ Row[expr, expr, ...]`) for VirtualTable reads.
#[derive(Debug, Clone)]
pub struct RowNode<'a> {
    pub pair: Pair<'a, Rule>, // Rule::row_addendum
    pub line_no: i64,
}

/// A `+`-prefixed addendum attached to a relation node. Addenda appear at
/// the same indentation as child relations but before them, and carry
/// metadata or data belonging to the parent relation.
#[derive(Debug, Clone)]
pub enum Addendum<'a> {
    AdvExt(AdvExt<'a>),
    Row(RowNode<'a>),
}

impl<'a> Addendum<'a> {
    fn line_no(&self) -> i64 {
        match self {
            Addendum::AdvExt(a) => a.line_no,
            Addendum::Row(r) => r.line_no,
        }
    }
}

/// A relation node in the plan tree, before conversion to a Substrait proto.
#[derive(Debug, Clone)]
pub struct RelationNode<'a> {
    pub pair: Pair<'a, Rule>,
    pub line_no: i64,
    pub addenda: Vec<Addendum<'a>>,
    pub children: Vec<RelationNode<'a>>,
}

impl<'a> RelationNode<'a> {
    pub fn context(&self) -> ParseContext {
        ParseContext {
            line_no: self.line_no,
            line: self.pair.as_str().to_string(),
        }
    }
}

/// A parsed plan line: either a relation or an addendum (`+`-prefixed line).
///
/// Classification happens at construction time by inspecting the inner grammar
/// rule, so downstream code can use standard Rust pattern matching rather than
/// runtime rule inspection.
#[derive(Debug, Clone)]
pub enum LineNode<'a> {
    Relation(RelationNode<'a>),
    Addendum(Addendum<'a>),
}

impl<'a> LineNode<'a> {
    pub fn parse(line: &'a str, line_no: i64) -> Result<Self, ParseError> {
        let mut pairs: pest::iterators::Pairs<'a, Rule> =
            <ExpressionParser as pest::Parser<Rule>>::parse(Rule::planNode, line).map_err(|e| {
                ParseError::Plan(
                    ParseContext {
                        line_no,
                        line: line.to_string(),
                    },
                    MessageParseError::new("planNode", ErrorKind::InvalidValue, Box::new(e)),
                )
            })?;

        let outer = pairs.next().unwrap();
        assert!(pairs.next().is_none()); // Should be exactly one pair
        let inner = unwrap_single_pair(outer);

        Ok(match inner.as_rule() {
            Rule::adv_extension => LineNode::Addendum(Addendum::AdvExt(AdvExt {
                pair: inner,
                line_no,
            })),
            Rule::row_addendum => LineNode::Addendum(Addendum::Row(RowNode {
                pair: inner,
                line_no,
            })),
            _ => LineNode::Relation(RelationNode {
                pair: inner,
                line_no,
                addenda: Vec::new(),
                children: Vec::new(),
            }),
        })
    }

    /// Parse the line as a top-level relation at depth 0 (either root_relation or regular relation)
    pub fn parse_root(line: &'a str, line_no: i64) -> Result<Self, ParseError> {
        let mut pairs: pest::iterators::Pairs<'a, Rule> = <ExpressionParser as pest::Parser<
            Rule,
        >>::parse(
            Rule::top_level_relation, line
        )
        .map_err(|e| {
            ParseError::Plan(
                ParseContext::new(line_no, line.to_string()),
                MessageParseError::new("top_level_relation", ErrorKind::Syntax, Box::new(e)),
            )
        })?;

        let outer = pairs.next().unwrap();
        assert!(pairs.next().is_none());

        // top_level_relation is either root_relation or planNode.
        // If planNode, unwrap one more level to obtain the specific relation rule.
        let inner = unwrap_single_pair(outer);
        let pair = if inner.as_rule() == Rule::planNode {
            unwrap_single_pair(inner)
        } else {
            inner // root_relation
        };

        // planNode can include addenda (+Enh:, +Opt:, +Row); surface them so
        // TreeBuilder::add_line can produce the appropriate depth-0 error.
        match pair.as_rule() {
            Rule::adv_extension => {
                return Ok(LineNode::Addendum(Addendum::AdvExt(AdvExt {
                    pair,
                    line_no,
                })));
            }
            Rule::row_addendum => {
                return Ok(LineNode::Addendum(Addendum::Row(RowNode { pair, line_no })));
            }
            _ => {} // Normal relation rule — fall through
        }

        Ok(LineNode::Relation(RelationNode {
            pair,
            line_no,
            addenda: Vec::new(),
            children: Vec::new(),
        }))
    }
}

/// Set the `advanced_extension` field on a [`Rel`], covering all standard
/// relation variants that carry the field directly.
fn set_advanced_extension(rel: &mut Rel, adv_ext: AdvancedExtension) {
    match &mut rel.rel_type {
        Some(RelType::Read(r)) => r.advanced_extension = Some(adv_ext),
        Some(RelType::Filter(r)) => r.advanced_extension = Some(adv_ext),
        Some(RelType::Project(r)) => r.advanced_extension = Some(adv_ext),
        Some(RelType::Aggregate(r)) => r.advanced_extension = Some(adv_ext),
        Some(RelType::Sort(r)) => r.advanced_extension = Some(adv_ext),
        Some(RelType::Fetch(r)) => r.advanced_extension = Some(adv_ext),
        Some(RelType::Join(r)) => r.advanced_extension = Some(adv_ext),
        Some(RelType::ExtensionLeaf(_)) => {
            unreachable!("Extension types do not have advanced extensions defined on them")
        }
        Some(RelType::ExtensionSingle(_)) => {
            unreachable!("Extension types do not have advanced extensions defined on them")
        }
        Some(RelType::ExtensionMulti(_)) => {
            unreachable!("Extension types do not have advanced extensions defined on them")
        }
        _ => {}
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

// An in-progress tree builder, building the tree of relations.
#[derive(Debug, Clone, Default)]
pub struct TreeBuilder<'a> {
    // Current tree of nodes being built. These have been successfully parsed
    // into Pest pairs, but have not yet been converted to substrait plans.
    current: Option<RelationNode<'a>>,
    // Completed trees that have been built.
    completed: Vec<RelationNode<'a>>,
}

impl<'a> TreeBuilder<'a> {
    /// Traverse down the tree, always taking the last child at each level, until reaching the specified depth.
    pub fn get_at_depth(&mut self, depth: usize) -> Option<&mut RelationNode<'a>> {
        let mut node = self.current.as_mut()?;
        for _ in 0..depth {
            node = node.children.last_mut()?;
        }
        Some(node)
    }

    pub fn add_line(&mut self, depth: usize, node: LineNode<'a>) -> Result<(), ParseError> {
        match node {
            LineNode::Relation(rel_node) => {
                if depth == 0 {
                    if let Some(prev) = self.current.take() {
                        self.completed.push(prev);
                    }
                    self.current = Some(rel_node);
                    return Ok(());
                }

                let parent = match self.get_at_depth(depth - 1) {
                    None => {
                        return Err(ParseError::Plan(
                            rel_node.context(),
                            MessageParseError::invalid(
                                "relation",
                                rel_node.pair.as_span(),
                                format!("No parent found for depth {depth}"),
                            ),
                        ));
                    }
                    Some(parent) => parent,
                };

                parent.children.push(rel_node);
            }
            LineNode::Addendum(addendum) => {
                let line_no = addendum.line_no();
                if depth == 0 {
                    return Err(ParseError::ValidationError(format!(
                        "line {line_no}: addenda (+ Enh: / + Opt: / + Row) \
                         cannot appear at the top level",
                    )));
                }

                let parent = match self.get_at_depth(depth - 1) {
                    None => {
                        return Err(ParseError::ValidationError(format!(
                            "line {line_no}: no parent found for addendum at depth {depth}",
                        )));
                    }
                    Some(parent) => parent,
                };

                if !parent.children.is_empty() {
                    return Err(ParseError::ValidationError(format!(
                        "line {line_no}: addenda (+ Enh: / + Opt: / + Row) must \
                         appear before child relations, not after",
                    )));
                }

                parent.addenda.push(addendum);
            }
        }
        Ok(())
    }

    /// End of input - move any remaining nodes from stack to completed and
    /// return any trees in progress. Resets the builder to its initial state
    /// (empty)
    /// Move any remaining nodes from stack to completed
    pub fn finish(&mut self) -> Vec<RelationNode<'a>> {
        if let Some(node) = self.current.take() {
            self.completed.push(node);
        }
        std::mem::take(&mut self.completed)
    }
}

/// Intermediate state for relation parsing: the structural tree data
/// (children, addenda) has been processed into proto types, but the
/// relation's own grammar pair hasn't been parsed yet.
struct RelationContext<'a> {
    pair: Pair<'a, Rule>,
    line_no: i64,
    #[allow(clippy::vec_box)]
    children: Vec<Box<Rel>>,
    input_field_count: usize,
    rows: Vec<nested::Struct>,
    advanced_extension: Option<AdvancedExtension>,
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

    /// Dispatch by grammar rule after validating addenda constraints.
    /// Standard relations go through [`parse_rel`](Self::parse_rel);
    /// extension relations go through
    /// [`parse_extension_relation`](Self::parse_extension_relation).
    fn parse_relation(
        &self,
        extensions: &SimpleExtensions,
        registry: &ExtensionRegistry,
        ctx: RelationContext,
    ) -> Result<(substrait::proto::Rel, usize), ParseError> {
        let rule = ctx.pair.as_rule();
        if !ctx.rows.is_empty() && rule != Rule::virtual_read_relation {
            return Err(ParseError::ValidationError(format!(
                "line {}: + Row addenda can only be attached to Read:Virtual relations",
                ctx.line_no
            )));
        }

        match rule {
            Rule::virtual_read_relation => self.parse_rel::<ParsedVirtualReadRel>(extensions, ctx),
            Rule::read_relation => self.parse_rel::<ReadRel>(extensions, ctx),
            Rule::filter_relation => self.parse_rel::<FilterRel>(extensions, ctx),
            Rule::project_relation => self.parse_rel::<ProjectRel>(extensions, ctx),
            Rule::aggregate_relation => self.parse_rel::<AggregateRel>(extensions, ctx),
            Rule::sort_relation => self.parse_rel::<SortRel>(extensions, ctx),
            Rule::fetch_relation => self.parse_rel::<FetchRel>(extensions, ctx),
            Rule::join_relation => self.parse_rel::<JoinRel>(extensions, ctx),
            Rule::extension_relation => self.parse_extension_relation(extensions, registry, ctx),
            _ => unreachable!("unhandled relation rule: {:?}", rule),
        }
    }

    /// Generic bridge between [`parse_relation`](Self::parse_relation) and
    /// the [`RelationParsePair`] trait: wraps `MessageParseError` with line
    /// context and calls [`into_rel`](RelationParsePair::into_rel) to apply
    /// addenda and produce the final [`Rel`].
    fn parse_rel<T: RelationParsePair>(
        &self,
        extensions: &SimpleExtensions,
        ctx: RelationContext,
    ) -> Result<(Rel, usize), ParseError> {
        assert_eq!(ctx.pair.as_rule(), T::rule());
        let line_no = ctx.line_no;
        let line = ctx.pair.as_str();
        let addenda = RelationAddenda {
            rows: ctx.rows,
            advanced_extension: ctx.advanced_extension,
        };

        match T::parse_pair_with_context(extensions, ctx.pair, ctx.children, ctx.input_field_count)
        {
            Ok((parsed, count)) => Ok((parsed.into_rel(addenda), count)),
            Err(e) => Err(ParseError::Plan(
                ParseContext::new(line_no, line.to_string()),
                e,
            )),
        }
    }

    /// Handle extension relations separately from [`parse_rel`](Self::parse_rel)
    /// because they need registry lookups that [`RelationParsePair`] doesn't
    /// support.
    fn parse_extension_relation(
        &self,
        extensions: &SimpleExtensions,
        registry: &ExtensionRegistry,
        ctx: RelationContext,
    ) -> Result<(Rel, usize), ParseError> {
        assert_eq!(ctx.pair.as_rule(), Rule::extension_relation);
        let line_no = ctx.line_no;
        let line = ctx.pair.as_str();
        let pair_span = ctx.pair.as_span();

        let ExtensionInvocation {
            name,
            args: extension_args,
        } = ExtensionInvocation::parse_pair(ctx.pair);

        let child_count = ctx.children.len();
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
            extensions,
            registry,
            line_no,
            line,
        };

        let detail = context.resolve_extension_detail(&name, &extension_args)?;
        let output_column_count = extension_args.output_columns.len();

        let mut rel = extension_args
            .relation_type
            .create_rel(detail, ctx.children)
            .map_err(|e| {
                ParseError::Plan(
                    ParseContext::new(line_no, line.to_string()),
                    MessageParseError::invalid("extension_relation", pair_span, e),
                )
            })?;

        if let Some(adv_ext) = ctx.advanced_extension {
            set_advanced_extension(&mut rel, adv_ext);
        }
        Ok((rel, output_column_count))
    }

    /// Walk the relation tree depth-first, converting structural types
    /// (children, addenda) into proto types via [`RelationContext`].
    /// Delegates grammar-rule-specific work to
    /// [`parse_relation`](Self::parse_relation).
    fn build_rel(
        &self,
        extensions: &SimpleExtensions,
        registry: &ExtensionRegistry,
        node: RelationNode,
    ) -> Result<(substrait::proto::Rel, usize), ParseError> {
        let mut children: Vec<Box<Rel>> = Vec::new();
        let mut input_field_count: usize = 0;
        for child in node.children {
            let (rel, count) = self.build_rel(extensions, registry, child)?;
            input_field_count += count;
            children.push(Box::new(rel));
        }

        let mut adv_exts = Vec::new();
        let mut rows = Vec::new();
        for addendum in node.addenda {
            match addendum {
                Addendum::AdvExt(a) => adv_exts.push(a),
                Addendum::Row(r) => rows.push(parse_row_addendum(extensions, r.pair)),
            }
        }
        let advanced_extension = if adv_exts.is_empty() {
            None
        } else {
            Some(self.build_advanced_extension(extensions, registry, adv_exts)?)
        };

        self.parse_relation(
            extensions,
            registry,
            RelationContext {
                pair: node.pair,
                line_no: node.line_no,
                children,
                input_field_count,
                rows,
                advanced_extension,
            },
        )
    }

    /// Parse a list of [`AdvExt`] nodes into an [`AdvancedExtension`] proto.
    fn build_advanced_extension(
        &self,
        extensions: &SimpleExtensions,
        registry: &ExtensionRegistry,
        adv_exts: Vec<AdvExt>,
    ) -> Result<AdvancedExtension, ParseError> {
        let mut enhancement = None;
        let mut optimizations = Vec::new();

        for adv_ext in adv_exts {
            let line_no = adv_ext.line_no;
            let line = adv_ext.pair.as_str().to_string();
            let invocation = AdvExtInvocation::parse_pair(adv_ext.pair);
            let context = RelationParsingContext {
                extensions,
                registry,
                line_no,
                line: &line,
            };

            match invocation.ext_type {
                ExtensionType::Enhancement => {
                    let detail = context.resolve_adv_ext_detail(
                        ExtensionType::Enhancement,
                        &invocation.name,
                        &invocation.args,
                    )?;
                    if enhancement.is_some() {
                        return Err(ParseError::ValidationError(
                            "at most one enhancement per relation is allowed".to_string(),
                        ));
                    }
                    enhancement = Some(detail.into());
                }
                ExtensionType::Optimization => {
                    let detail = context.resolve_adv_ext_detail(
                        ExtensionType::Optimization,
                        &invocation.name,
                        &invocation.args,
                    )?;
                    optimizations.push(detail.into());
                }
                ExtensionType::Relation => {
                    unreachable!("Grammar restricts adv_ext_type to 'Enh' or 'Opt'")
                }
            }
        }

        Ok(AdvancedExtension {
            enhancement,
            optimization: optimizations,
        })
    }

    /// Build a tree of relations.
    fn build_plan_rel(
        &self,
        extensions: &SimpleExtensions,
        registry: &ExtensionRegistry,
        node: RelationNode,
    ) -> Result<PlanRel, ParseError> {
        // Plain relations are allowed as root relations; they just don't have names.
        if node.pair.as_rule() != Rule::root_relation {
            let (rel, _) = self.build_rel(extensions, registry, node)?;
            return Ok(PlanRel {
                rel_type: Some(plan_rel::RelType::Rel(rel)),
            });
        }

        // Root relations don't support addenda — reject rather than silently discard.
        if !node.addenda.is_empty() {
            let line_no = node.addenda[0].line_no();
            return Err(ParseError::ValidationError(format!(
                "line {line_no}: addenda (+ Enh: / + Opt: / + Row) are not \
                 supported on Root relations",
            )));
        }

        // Named root relation.
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

        let mut children = node.children;
        let child = match children.len() {
            1 => {
                let (rel, _) = self.build_rel(extensions, registry, children.pop().unwrap())?;
                rel
            }
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
/// URNs:
///   @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
/// Functions:
///   # 10 @  1: add
/// === Plan
/// Root[columns]
///   Relation[arguments => columns]
///     ChildRelation[arguments => columns]
/// ```
///
/// - **Extensions section** (optional): Defines URNs and function/type declarations
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
/// - Define custom functions with URNs and anchors
/// - Reference functions by name in expressions
/// - Use custom types and type variations
///
/// ```rust
/// use substrait_explain::parser::Parser;
///
/// let plan_with_extensions = r#"
/// === Extensions
/// URNs:
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
    extension_registry: ExtensionRegistry,
    relation_parser: RelationParser<'a>,
}
impl<'a> Default for Parser<'a> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Parser<'a> {
    /// Parse a Substrait plan from text format.
    ///
    /// This is the main entry point for parsing.
    ///
    /// The input should be in the Substrait text format, which consists of:
    /// - An optional extensions section starting with "=== Extensions"
    /// - A plan section starting with "=== Plan"
    /// - Indented relation definitions
    ///
    /// # Examples
    ///
    /// Simple parsing:
    /// ```rust
    /// use substrait_explain::parser::Parser;
    ///
    /// let plan_text = r#"
    /// === Plan
    /// Root[result]
    ///   Read[table => col:i32]
    /// "#;
    ///
    /// let plan = Parser::parse(plan_text).unwrap();
    /// assert_eq!(plan.relations.len(), 1);
    /// ```
    ///
    /// # Errors
    ///
    /// Returns a [`ParseError`] if the input cannot be parsed.
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
    pub fn parse_plan(mut self, input: &'a str) -> ParseResult {
        for line in input.lines() {
            if line.trim().is_empty() {
                self.line_no += 1;
                continue;
            }

            self.parse_line(line)?;
            self.line_no += 1;
        }

        let plan = self.build_plan()?;
        Ok(plan)
    }

    /// Parse a single line of input.
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
            State::Plan => {
                let IndentedLine(depth, line_str) = indented_line;

                // Parse the line
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
        Ok(())
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

        // Parse the tree into relations
        let root_relations = relation_parser.build(extensions, &extension_registry)?;

        // Build the final plan
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
        expected_extensions
            .add_extension(ExtensionKind::Type, 1, 20, "SomeType".to_string())
            .unwrap();
        expected_extensions
            .add_extension(ExtensionKind::TypeVariation, 2, 30, "VarX".to_string())
            .unwrap();

        let mut parser = ExtensionParser::default();
        let input_block = r#"
URNs:
  @  1: /urn/common
  @  2: /urn/specific_funcs
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
URNs:
  @  1: /urn/common
  @  2: /urn/specific_funcs
  @  3: /urn/types_lib
  @  4: /urn/variations_lib
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
URNs:
  @  1: /urn/common
  @  2: /urn/specific_funcs
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
        assert_eq!(plan.extension_urns.len(), 2);
        assert_eq!(plan.extensions.len(), 4);
        assert_eq!(plan.relations.len(), 1);

        // Verify extension URIs
        let urn1 = &plan.extension_urns[0];
        assert_eq!(urn1.extension_urn_anchor, 1);
        assert_eq!(urn1.urn, "/urn/common");

        let urn2 = &plan.extension_urns[1];
        assert_eq!(urn2.extension_urn_anchor, 2);
        assert_eq!(urn2.urn, "/urn/specific_funcs");

        // Verify extensions
        let func1 = &plan.extensions[0];
        match &func1.mapping_type {
            Some(MappingType::ExtensionFunction(f)) => {
                assert_eq!(f.function_anchor, 10);
                assert_eq!(f.extension_urn_reference, 1);
                assert_eq!(f.name, "func_a");
            }
            other => panic!("Expected ExtensionFunction, got {other:?}"),
        }

        let func2 = &plan.extensions[1];
        match &func2.mapping_type {
            Some(MappingType::ExtensionFunction(f)) => {
                assert_eq!(f.function_anchor, 11);
                assert_eq!(f.extension_urn_reference, 2);
                assert_eq!(f.name, "func_b_special");
            }
            other => panic!("Expected ExtensionFunction, got {other:?}"),
        }

        let type1 = &plan.extensions[2];
        match &type1.mapping_type {
            Some(MappingType::ExtensionType(t)) => {
                assert_eq!(t.type_anchor, 20);
                assert_eq!(t.extension_urn_reference, 1);
                assert_eq!(t.name, "SomeType");
            }
            other => panic!("Expected ExtensionType, got {other:?}"),
        }

        let var1 = &plan.extensions[3];
        match &var1.mapping_type {
            Some(MappingType::ExtensionTypeVariation(v)) => {
                assert_eq!(v.type_variation_anchor, 30);
                assert_eq!(v.extension_urn_reference, 2);
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
                        assert!(project.input.is_some()); // Should have Filter as input

                        // Check the Filter input
                        let filter_input = project.input.as_ref().unwrap();
                        match &filter_input.rel_type {
                            Some(RelType::Filter(filter)) => {
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
