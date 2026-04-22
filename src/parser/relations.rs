use std::collections::HashMap;

use pest::iterators::Pair;
use prost::Message;
use substrait::proto::aggregate_rel::Grouping;
use substrait::proto::expression::literal::LiteralType;
use substrait::proto::expression::{Literal, RexType, nested};
use substrait::proto::extensions::AdvancedExtension;
use substrait::proto::fetch_rel::{CountMode, OffsetMode};
use substrait::proto::rel::RelType;
use substrait::proto::rel_common::{Direct, Emit, EmitKind};
use substrait::proto::sort_field::{SortDirection, SortKind};
use substrait::proto::{
    AggregateRel, Expression, FetchRel, FilterRel, JoinRel, NamedStruct, ProjectRel, ReadRel, Rel,
    RelCommon, SortField, SortRel, Type, aggregate_rel, join_rel, read_rel, r#type,
};

use super::{MessageParseError, ParsePair, Rule, RuleIter, ScopedParsePair, unwrap_single_pair};
use crate::extensions::any::Any;
use crate::extensions::registry::{ExtensionError, ExtensionType};
use crate::extensions::{ExtensionArgs, ExtensionRegistry, SimpleExtensions};
use crate::parser::errors::{ParseContext, ParseError};
use crate::parser::expressions::{FieldIndex, Name};

/// Parsing context for relations that includes extensions, registry, and optional warning collection
pub struct RelationParsingContext<'a> {
    pub extensions: &'a SimpleExtensions,
    pub registry: &'a ExtensionRegistry,
    pub line_no: i64,
    pub line: &'a str,
}

impl<'a> RelationParsingContext<'a> {
    /// Resolve extension detail using registry. Any failure is treated as a hard parse error.
    pub fn resolve_extension_detail(
        &self,
        extension_name: &str,
        extension_args: &ExtensionArgs,
    ) -> Result<Option<Any>, ParseError> {
        let detail = self
            .registry
            .parse_extension(extension_name, extension_args);

        match detail {
            Ok(any) => Ok(Some(any)),
            Err(ExtensionError::NotFound { .. }) => Err(ParseError::UnregisteredExtension {
                name: extension_name.to_string(),
                context: ParseContext::new(self.line_no, self.line.to_string()),
            }),
            Err(err) => Err(ParseError::ExtensionDetail(
                ParseContext::new(self.line_no, self.line.to_string()),
                err,
            )),
        }
    }

    /// Resolve an advanced-extension detail (enhancement or optimization) using the registry.
    /// Any failure is treated as a hard parse error.
    ///
    /// `ext_type` must be `Enhancement` or `Optimization`; `Relation` is not valid here and
    /// will panic via the unreachable branch in the dispatch below.
    pub fn resolve_adv_ext_detail(
        &self,
        ext_type: ExtensionType,
        name: &str,
        args: &ExtensionArgs,
    ) -> Result<Any, ParseError> {
        let result = match ext_type {
            ExtensionType::Enhancement => self.registry.parse_enhancement(name, args),
            ExtensionType::Optimization => self.registry.parse_optimization(name, args),
            ExtensionType::Relation => unreachable!("Relation is not an advanced extension type"),
        };
        result.map_err(|err| match err {
            ExtensionError::NotFound { .. } => ParseError::UnregisteredExtension {
                name: name.to_string(),
                context: ParseContext::new(self.line_no, self.line.to_string()),
            },
            err => ParseError::ExtensionDetail(
                ParseContext::new(self.line_no, self.line.to_string()),
                err,
            ),
        })
    }
}

/// A trait for parsing relations with full context for tree building.
pub trait RelationParsePair: Sized {
    fn rule() -> Rule;
    fn message() -> &'static str;

    /// Parse the grammar pair into this relation type and its output field
    /// count.
    ///
    /// Returns `(Self, usize)` where `usize` is the output field count —
    /// computed during parsing when `input_field_count` is available.
    fn parse_pair_with_context(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
        input_children: Vec<Box<Rel>>,
        input_field_count: usize,
    ) -> Result<(Self, usize), MessageParseError>;

    /// Consume this parsed relation, apply the advanced extension, and produce
    /// the final `Rel`.
    fn into_rel(self, adv_ext: Option<AdvancedExtension>) -> Rel;
}

pub struct TableName(Vec<String>);

impl ParsePair for TableName {
    fn rule() -> Rule {
        Rule::table_name
    }

    fn message() -> &'static str {
        "TableName"
    }

    fn parse_pair(pair: Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());
        let pairs = pair.into_inner();
        let mut names = Vec::with_capacity(pairs.len());
        let mut iter = RuleIter::from(pairs);
        while let Some(name) = iter.parse_if_next::<Name>() {
            names.push(name.0);
        }
        iter.done();
        Self(names)
    }
}

#[derive(Debug, Clone)]
pub struct Column {
    pub name: String,
    pub typ: Type,
}

impl ScopedParsePair for Column {
    fn rule() -> Rule {
        Rule::named_column
    }

    fn message() -> &'static str {
        "Column"
    }

    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        let mut iter = RuleIter::from(pair.into_inner());
        let name = iter.parse_next::<Name>().0;
        let typ = iter.parse_next_scoped(extensions)?;
        iter.done();
        Ok(Self { name, typ })
    }
}

pub struct NamedColumnList(Vec<Column>);

impl ScopedParsePair for NamedColumnList {
    fn rule() -> Rule {
        Rule::named_column_list
    }

    fn message() -> &'static str {
        "NamedColumnList"
    }

    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        let mut columns = Vec::new();
        for col in pair.into_inner() {
            columns.push(Column::parse_pair(extensions, col)?);
        }
        Ok(Self(columns))
    }
}

/// This is a utility function for extracting a single child from the list of
/// children, to be used in the RelationParsePair trait. The RelationParsePair
/// trait passes a Vec of children, because some relations have multiple
/// children - but most accept exactly one child.
#[allow(clippy::vec_box)]
pub(crate) fn expect_one_child(
    message: &'static str,
    pair: &Pair<Rule>,
    mut input_children: Vec<Box<Rel>>,
) -> Result<Box<Rel>, MessageParseError> {
    match input_children.len() {
        0 => Err(MessageParseError::invalid(
            message,
            pair.as_span(),
            format!("{message} missing child"),
        )),
        1 => Ok(input_children.pop().unwrap()),
        n => Err(MessageParseError::invalid(
            message,
            pair.as_span(),
            format!("{message} should have 1 input child, got {n}"),
        )),
    }
}

/// Parse a reference list Pair and return an EmitKind::Emit.
/// Parse a reference list into field indices for emit mapping.
fn parse_output_mapping(pair: Pair<Rule>) -> Vec<i32> {
    assert_eq!(pair.as_rule(), Rule::reference_list);
    pair.into_inner()
        .map(|p| FieldIndex::parse_pair(p).0)
        .collect()
}

/// Build an emit: `Direct` if the mapping is the identity `[0, 1, ..., N-1]`
/// (where N = `direct_output_count`), otherwise `Emit` with the explicit mapping.
fn make_emit(output_mapping: Vec<i32>, direct_output_count: usize) -> EmitKind {
    let is_identity = output_mapping.len() == direct_output_count
        && output_mapping
            .iter()
            .enumerate()
            .all(|(i, &v)| v == i as i32);
    if is_identity {
        EmitKind::Direct(Direct {})
    } else {
        EmitKind::Emit(Emit { output_mapping })
    }
}

/// Parse a reference list into an emit and output field count.
fn parse_emit(reference_list: Pair<Rule>, direct_output_count: usize) -> (EmitKind, usize) {
    let output_mapping = parse_output_mapping(reference_list);
    let output_count = output_mapping.len();
    let emit = make_emit(output_mapping, direct_output_count);
    (emit, output_count)
}

/// Extracts named arguments from pest pairs with duplicate detection and completeness checking.
///
/// Usage: `extractor.pop("limit", Rule::fetch_value).0.pop("offset", Rule::fetch_value).0.done()`
///
/// The fluent API ensures all arguments are processed exactly once and none are forgotten.
pub struct ParsedNamedArgs<'a> {
    map: HashMap<&'a str, Pair<'a, Rule>>,
}

impl<'a> ParsedNamedArgs<'a> {
    pub fn new(
        pairs: pest::iterators::Pairs<'a, Rule>,
        rule: Rule,
    ) -> Result<Self, MessageParseError> {
        let mut map = HashMap::new();
        for pair in pairs {
            assert_eq!(pair.as_rule(), rule);
            let mut inner = pair.clone().into_inner();
            let name_pair = inner.next().unwrap();
            let value_pair = inner.next().unwrap();
            assert_eq!(inner.next(), None);
            let name = name_pair.as_str();
            if map.contains_key(name) {
                return Err(MessageParseError::invalid(
                    "NamedArg",
                    name_pair.as_span(),
                    format!("Duplicate argument: {name}"),
                ));
            }
            map.insert(name, value_pair);
        }
        Ok(Self { map })
    }

    // Returns the pair if it exists and matches the rule, otherwise None.
    // Asserts that the rule must match the rule of the pair (and therefore
    // panics in non-release-mode if not)
    pub fn pop(mut self, name: &str, rule: Rule) -> (Self, Option<Pair<'a, Rule>>) {
        let pair = self.map.remove(name).inspect(|pair| {
            assert_eq!(pair.as_rule(), rule, "Rule mismatch for argument {name}");
        });
        (self, pair)
    }

    // Returns an error if there are any unused arguments.
    pub fn done(self) -> Result<(), MessageParseError> {
        if let Some((name, pair)) = self.map.iter().next() {
            return Err(MessageParseError::invalid(
                "NamedArgExtractor",
                // No span available for all unused args; use default.
                pair.as_span(),
                format!("Unknown argument: {name}"),
            ));
        }
        Ok(())
    }
}

impl RelationParsePair for ReadRel {
    fn rule() -> Rule {
        Rule::read_relation
    }

    fn message() -> &'static str {
        "ReadRel"
    }

    fn parse_pair_with_context(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
        input_children: Vec<Box<Rel>>,
        input_field_count: usize,
    ) -> Result<(Self, usize), MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        // ReadRel is a leaf node - it should have no input children and 0 input fields
        if !input_children.is_empty() {
            return Err(MessageParseError::invalid(
                Self::message(),
                pair.as_span(),
                "ReadRel should have no input children",
            ));
        }
        if input_field_count != 0 {
            return Err(MessageParseError::invalid(
                "ReadRel",
                pair.as_span(),
                "ReadRel should have 0 input fields",
            ));
        }

        let mut iter = RuleIter::from(pair.into_inner());
        let table = iter.parse_next::<TableName>().0;
        let columns = iter.parse_next_scoped::<NamedColumnList>(extensions)?.0;
        iter.done();

        let output_count = columns.len();
        Ok((
            ReadRel {
                base_schema: Some(build_named_struct(columns)),
                read_type: Some(read_rel::ReadType::NamedTable(read_rel::NamedTable {
                    names: table,
                    advanced_extension: None,
                })),
                ..Default::default()
            },
            output_count,
        ))
    }

    fn into_rel(mut self, adv_ext: Option<AdvancedExtension>) -> Rel {
        self.advanced_extension = adv_ext;
        Rel {
            rel_type: Some(RelType::Read(Box::new(self))),
        }
    }
}

/// Parsed `Read:Virtual[rows => columns]` relation. Needs a newtype because the
/// proto type is `ReadRel` (same as `NamedTable`), but the grammar and handling
/// are different.
pub(crate) struct VirtualReadRel(ReadRel);

impl RelationParsePair for VirtualReadRel {
    fn rule() -> Rule {
        Rule::virtual_read_relation
    }

    fn message() -> &'static str {
        "VirtualReadRel"
    }

    fn parse_pair_with_context(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
        input_children: Vec<Box<Rel>>,
        _input_field_count: usize,
    ) -> Result<(Self, usize), MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        if !input_children.is_empty() {
            return Err(MessageParseError::invalid(
                Self::message(),
                pair.as_span(),
                "Read:Virtual should have no input children",
            ));
        }

        let mut iter = RuleIter::from(pair.into_inner());
        let args_pair = iter.pop(Rule::virtual_read_args);
        let columns_pair = iter.pop(Rule::named_column_list);
        iter.done();

        let rows = parse_virtual_read_args(extensions, args_pair)?;
        let columns = NamedColumnList::parse_pair(extensions, columns_pair)?.0;

        // TODO: Validate that each row has the same number of expressions as
        // columns. Currently no parser-side warning mechanism exists, and while
        // this is an invalid plan, it is constructible as Substrait. Consider
        // adding once a warning collection path is available.
        let output_count = columns.len();
        Ok((
            VirtualReadRel(ReadRel {
                base_schema: Some(build_named_struct(columns)),
                read_type: Some(read_rel::ReadType::VirtualTable(read_rel::VirtualTable {
                    expressions: rows,
                    ..Default::default()
                })),
                ..Default::default()
            }),
            output_count,
        ))
    }

    fn into_rel(mut self, adv_ext: Option<AdvancedExtension>) -> Rel {
        self.0.advanced_extension = adv_ext;
        Rel {
            rel_type: Some(RelType::Read(Box::new(self.0))),
        }
    }
}

/// Build a `NamedStruct` from parsed columns.
fn build_named_struct(columns: Vec<Column>) -> NamedStruct {
    let (names, types): (Vec<_>, Vec<_>) = columns.into_iter().map(|c| (c.name, c.typ)).unzip();
    NamedStruct {
        names,
        r#struct: Some(r#type::Struct {
            types,
            type_variation_reference: 0,
            nullability: r#type::Nullability::Required as i32,
        }),
    }
}

/// `Read:Virtual` positional args: either `empty` or a list of row tuples.
fn parse_virtual_read_args(
    extensions: &SimpleExtensions,
    pair: Pair<Rule>,
) -> Result<Vec<nested::Struct>, MessageParseError> {
    assert_eq!(pair.as_rule(), Rule::virtual_read_args);
    let inner = unwrap_single_pair(pair);
    match inner.as_rule() {
        Rule::empty => Ok(vec![]),
        Rule::virtual_row_list => inner
            .into_inner()
            .map(|row| parse_virtual_row(extensions, row))
            .collect(),
        _ => unreachable!(
            "Unexpected rule in virtual_read_args: {:?}",
            inner.as_rule()
        ),
    }
}

/// Parse a single `virtual_row` (`(expr, expr, ...)` or `()`) into a `nested::Struct`.
fn parse_virtual_row(
    extensions: &SimpleExtensions,
    pair: Pair<Rule>,
) -> Result<nested::Struct, MessageParseError> {
    assert_eq!(pair.as_rule(), Rule::virtual_row);
    let fields = match pair.into_inner().next() {
        Some(expression_list) => {
            assert_eq!(expression_list.as_rule(), Rule::expression_list);
            parse_expression_list(extensions, expression_list)?
        }
        // Empty virtual row. An unusual but valid case.
        None => vec![],
    };
    Ok(nested::Struct { fields })
}

impl RelationParsePair for FilterRel {
    fn rule() -> Rule {
        Rule::filter_relation
    }

    fn message() -> &'static str {
        "FilterRel"
    }

    fn into_rel(mut self, adv_ext: Option<AdvancedExtension>) -> Rel {
        self.advanced_extension = adv_ext;
        Rel {
            rel_type: Some(RelType::Filter(Box::new(self))),
        }
    }

    fn parse_pair_with_context(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
        input_children: Vec<Box<Rel>>,
        input_field_count: usize,
    ) -> Result<(Self, usize), MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        let input = expect_one_child(Self::message(), &pair, input_children)?;
        let mut iter = RuleIter::from(pair.into_inner());
        let condition = iter.parse_next_scoped::<Expression>(extensions)?;
        // references (which become the emit)
        let references_pair = iter.pop(Rule::reference_list);
        iter.done();

        let (emit, output_count) = parse_emit(references_pair, input_field_count);
        let common = RelCommon {
            emit_kind: Some(emit),
            ..Default::default()
        };

        Ok((
            FilterRel {
                input: Some(input),
                condition: Some(Box::new(condition)),
                common: Some(common),
                advanced_extension: None,
            },
            output_count,
        ))
    }
}

impl RelationParsePair for ProjectRel {
    fn rule() -> Rule {
        Rule::project_relation
    }

    fn message() -> &'static str {
        "ProjectRel"
    }

    fn into_rel(mut self, adv_ext: Option<AdvancedExtension>) -> Rel {
        self.advanced_extension = adv_ext;
        Rel {
            rel_type: Some(RelType::Project(Box::new(self))),
        }
    }

    fn parse_pair_with_context(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
        input_children: Vec<Box<Rel>>,
        input_field_count: usize,
    ) -> Result<(Self, usize), MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        let input = expect_one_child(Self::message(), &pair, input_children)?;

        let arguments_pair = unwrap_single_pair(pair);

        let mut expressions = Vec::new();
        let mut output_mapping = Vec::new();

        for arg in arguments_pair.into_inner() {
            let inner_arg = unwrap_single_pair(arg);
            match inner_arg.as_rule() {
                Rule::reference => {
                    let field_index = FieldIndex::parse_pair(inner_arg);
                    output_mapping.push(field_index.0);
                }
                Rule::expression => {
                    let expr = Expression::parse_pair(extensions, inner_arg)?;
                    expressions.push(expr);
                    // Index into the combined schema: [input fields][computed expressions].
                    output_mapping.push(input_field_count as i32 + (expressions.len() as i32 - 1));
                }
                _ => panic!("Unexpected inner argument rule: {:?}", inner_arg.as_rule()),
            }
        }

        let output_count = output_mapping.len();
        let direct_count = input_field_count + expressions.len();
        let emit = make_emit(output_mapping, direct_count);
        let common = RelCommon {
            emit_kind: Some(emit),
            ..Default::default()
        };

        Ok((
            ProjectRel {
                input: Some(input),
                expressions,
                common: Some(common),
                advanced_extension: None,
            },
            output_count,
        ))
    }
}

impl RelationParsePair for AggregateRel {
    fn rule() -> Rule {
        Rule::aggregate_relation
    }

    fn message() -> &'static str {
        "AggregateRel"
    }

    fn into_rel(mut self, adv_ext: Option<AdvancedExtension>) -> Rel {
        self.advanced_extension = adv_ext;
        Rel {
            rel_type: Some(RelType::Aggregate(Box::new(self))),
        }
    }

    fn parse_pair_with_context(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
        input_children: Vec<Box<Rel>>,
        // Aggregate defines its own output schema (grouping keys + measures),
        // so the input field count isn't needed for emit construction.
        _input_field_count: usize,
    ) -> Result<(Self, usize), MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        let input = expect_one_child(Self::message(), &pair, input_children)?;
        let mut iter = RuleIter::from(pair.into_inner());
        let group_by_pair = iter.pop(Rule::aggregate_group_by);
        let output_pair = iter.pop(Rule::aggregate_output);
        iter.done();

        let inner = group_by_pair
            .into_inner()
            .next()
            .expect("aggregate_group_by must have one inner item");

        let grouping_sets = parse_grouping_sets(extensions, inner)?;
        let (groupings, grouping_expressions) = build_grouping_fields(&grouping_sets);

        let (measures, output_mapping) =
            parse_aggregate_measures(extensions, output_pair, &grouping_expressions)?;

        let output_count = output_mapping.len();
        let direct_count = grouping_expressions.len() + measures.len();
        let emit = make_emit(output_mapping, direct_count);
        let common = RelCommon {
            emit_kind: Some(emit),
            ..Default::default()
        };

        Ok((
            AggregateRel {
                input: Some(input),
                grouping_expressions,
                groupings,
                measures,
                common: Some(common),
                advanced_extension: None,
            },
            output_count,
        ))
    }
}

/// Parses the output section of an aggregate (everything after `=>`).
///
/// For example, in `Aggregate[($0, $1), _ => sum($2), $0, count($2)]`,
/// this parses `sum($2), $0, count($2)`.
fn parse_aggregate_measures(
    extensions: &SimpleExtensions,
    output_pair: Pair<'_, Rule>,
    grouping_expressions: &[Expression],
) -> Result<(Vec<aggregate_rel::Measure>, Vec<i32>), MessageParseError> {
    assert_eq!(output_pair.as_rule(), Rule::aggregate_output);
    let mut measures = Vec::new();
    let mut output_mapping = Vec::new();

    for aggregate_output_item in output_pair.into_inner() {
        let inner_item = unwrap_single_pair(aggregate_output_item);
        match inner_item.as_rule() {
            Rule::reference => {
                let field_index = FieldIndex::parse_pair(inner_item);
                output_mapping.push(field_index.0);
            }
            Rule::aggregate_measure => {
                let measure = aggregate_rel::Measure::parse_pair(extensions, inner_item)?;
                output_mapping.push(grouping_expressions.len() as i32 + measures.len() as i32);
                measures.push(measure);
            }
            _ => panic!(
                "Unexpected inner output item rule: {:?}",
                inner_item.as_rule()
            ),
        }
    }

    Ok((measures, output_mapping))
}

/// Parses the grouping section of an aggregate (everything before `=>`).
///
/// For example, in `Aggregate[($0, $1), _ => sum($2), $0, count($2)]`,
/// this parses `($0, $1), _`.
///
/// Each inner Vec is one grouping set; an empty vec represents no grouping (global aggregate).
///
/// Grammar: `aggregate_group_by = { grouping_set_list | expression_list }`
fn parse_grouping_sets(
    extensions: &SimpleExtensions,
    inner: Pair<'_, Rule>,
) -> Result<Vec<Vec<Expression>>, MessageParseError> {
    assert!(
        matches!(
            inner.as_rule(),
            Rule::expression_list | Rule::grouping_set_list
        ),
        "Expected expression_list or grouping_set_list, got {:?}",
        inner.as_rule()
    );
    match inner.as_rule() {
        Rule::expression_list => Ok(vec![parse_expression_list(extensions, inner)?]),
        Rule::grouping_set_list => inner
            .into_inner()
            .map(|pair| parse_grouping_set(extensions, pair))
            .collect(),
        _ => unreachable!(
            "Unexpected rule in aggregate_group_by: {:?}",
            inner.as_rule()
        ),
    }
}

/// Parses a single grouping set, e.g. `($0, $1)` or `_`.
///
/// Grammar: `grouping_set = { ("(" ~ expression_list ~ ")") | empty }`
fn parse_grouping_set(
    extensions: &SimpleExtensions,
    pair: Pair<'_, Rule>,
) -> Result<Vec<Expression>, MessageParseError> {
    assert_eq!(pair.as_rule(), Rule::grouping_set);
    let inner = pair
        .into_inner()
        .next()
        .expect("grouping_set must have one inner item");
    match inner.as_rule() {
        Rule::empty => Ok(vec![]),
        Rule::expression_list => parse_expression_list(extensions, inner),
        _ => unreachable!("Unexpected item in grouping_set: {:?}", inner.as_rule()),
    }
}

/// Grammar: `expression_list = { expression ~ ("," ~ expression)* }`
pub(crate) fn parse_expression_list(
    extensions: &SimpleExtensions,
    pair: Pair<'_, Rule>,
) -> Result<Vec<Expression>, MessageParseError> {
    pair.into_inner()
        .map(|expr_pair| Expression::parse_pair(extensions, expr_pair))
        .collect()
}

/// Deduplicates expressions across all sets and produces the AggregateRel's
/// protobuf fields: a flat deduplicated expression list and per-set Grouping
/// messages with index references into that list.
fn build_grouping_fields(expression_sets: &[Vec<Expression>]) -> (Vec<Grouping>, Vec<Expression>) {
    let mut expressions: Vec<Expression> = Vec::new();
    let mut seen: HashMap<Vec<u8>, u32> = HashMap::new();

    let groupings = expression_sets
        .iter()
        .map(|set| {
            let expression_references = set
                .iter()
                .map(|exp| {
                    // TODO: use a better key here than encoding to bytes.
                    // Ideally, substrait-rs would support `PartialEq` and `Hash`,
                    // but as there isn't an easy way to do that now, we'll skip.
                    let key = exp.encode_to_vec();
                    let next_idx = expressions.len() as u32;
                    *seen.entry(key).or_insert_with(|| {
                        expressions.push(exp.clone());
                        next_idx
                    })
                })
                .collect();
            Grouping {
                expression_references,
                #[allow(deprecated)]
                grouping_expressions: vec![],
            }
        })
        .collect();

    (groupings, expressions)
}

impl ScopedParsePair for SortField {
    fn rule() -> Rule {
        Rule::sort_field
    }

    fn message() -> &'static str {
        "SortField"
    }

    fn parse_pair(
        _extensions: &SimpleExtensions,
        pair: Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        let mut iter = RuleIter::from(pair.into_inner());
        let reference_pair = iter.pop(Rule::reference);
        let field_index = FieldIndex::parse_pair(reference_pair);
        let direction_pair = iter.pop(Rule::sort_direction);
        // Strip the '&' prefix from enum syntax (e.g., "&AscNullsFirst" ->
        // "AscNullsFirst") The grammar includes '&' to distinguish enums from
        // identifiers, but the enum variant names don't include it
        let direction = match direction_pair.as_str().trim_start_matches('&') {
            "AscNullsFirst" => SortDirection::AscNullsFirst,
            "AscNullsLast" => SortDirection::AscNullsLast,
            "DescNullsFirst" => SortDirection::DescNullsFirst,
            "DescNullsLast" => SortDirection::DescNullsLast,
            other => {
                return Err(MessageParseError::invalid(
                    "SortDirection",
                    direction_pair.as_span(),
                    format!("Unknown sort direction: {other}"),
                ));
            }
        };
        iter.done();
        Ok(SortField {
            expr: Some(Expression {
                rex_type: Some(substrait::proto::expression::RexType::Selection(Box::new(
                    field_index.to_field_reference(),
                ))),
            }),
            // TODO: Add support for SortKind::ComparisonFunctionReference
            sort_kind: Some(SortKind::Direction(direction as i32)),
        })
    }
}

impl RelationParsePair for SortRel {
    fn rule() -> Rule {
        Rule::sort_relation
    }

    fn message() -> &'static str {
        "SortRel"
    }

    fn into_rel(mut self, adv_ext: Option<AdvancedExtension>) -> Rel {
        self.advanced_extension = adv_ext;
        Rel {
            rel_type: Some(RelType::Sort(Box::new(self))),
        }
    }

    fn parse_pair_with_context(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
        input_children: Vec<Box<Rel>>,
        input_field_count: usize,
    ) -> Result<(Self, usize), MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        let input = expect_one_child(Self::message(), &pair, input_children)?;
        let mut iter = RuleIter::from(pair.into_inner());
        let sort_field_list_pair = iter.pop(Rule::sort_field_list);
        let reference_list_pair = iter.pop(Rule::reference_list);
        let mut sorts = Vec::new();
        for sort_field_pair in sort_field_list_pair.into_inner() {
            let sort_field = SortField::parse_pair(extensions, sort_field_pair)?;
            sorts.push(sort_field);
        }
        let (emit, output_count) = parse_emit(reference_list_pair, input_field_count);
        let common = RelCommon {
            emit_kind: Some(emit),
            ..Default::default()
        };
        iter.done();
        Ok((
            SortRel {
                input: Some(input),
                sorts,
                common: Some(common),
                advanced_extension: None,
            },
            output_count,
        ))
    }
}

impl ScopedParsePair for CountMode {
    fn rule() -> Rule {
        Rule::fetch_value
    }
    fn message() -> &'static str {
        "CountMode"
    }
    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        let mut arg_inner = RuleIter::from(pair.into_inner());
        let value_pair = if let Some(int_pair) = arg_inner.try_pop(Rule::integer) {
            int_pair
        } else {
            arg_inner.pop(Rule::expression)
        };
        match value_pair.as_rule() {
            Rule::integer => {
                let value = value_pair.as_str().parse::<i64>().map_err(|e| {
                    MessageParseError::invalid(
                        Self::message(),
                        value_pair.as_span(),
                        format!("Invalid integer: {e}"),
                    )
                })?;
                if value < 0 {
                    return Err(MessageParseError::invalid(
                        Self::message(),
                        value_pair.as_span(),
                        format!("Fetch limit must be non-negative, got: {value}"),
                    ));
                }
                Ok(CountMode::CountExpr(i64_literal_expr(value)))
            }
            Rule::expression => {
                let expr = Expression::parse_pair(extensions, value_pair)?;
                Ok(CountMode::CountExpr(Box::new(expr)))
            }
            _ => Err(MessageParseError::invalid(
                Self::message(),
                value_pair.as_span(),
                format!("Unexpected rule for CountMode: {:?}", value_pair.as_rule()),
            )),
        }
    }
}

fn i64_literal_expr(value: i64) -> Box<Expression> {
    Box::new(Expression {
        rex_type: Some(RexType::Literal(Literal {
            nullable: false,
            type_variation_reference: 0,
            literal_type: Some(LiteralType::I64(value)),
        })),
    })
}

impl ScopedParsePair for OffsetMode {
    fn rule() -> Rule {
        Rule::fetch_value
    }
    fn message() -> &'static str {
        "OffsetMode"
    }
    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        let mut arg_inner = RuleIter::from(pair.into_inner());
        let value_pair = if let Some(int_pair) = arg_inner.try_pop(Rule::integer) {
            int_pair
        } else {
            arg_inner.pop(Rule::expression)
        };
        match value_pair.as_rule() {
            Rule::integer => {
                let value = value_pair.as_str().parse::<i64>().map_err(|e| {
                    MessageParseError::invalid(
                        Self::message(),
                        value_pair.as_span(),
                        format!("Invalid integer: {e}"),
                    )
                })?;
                if value < 0 {
                    return Err(MessageParseError::invalid(
                        Self::message(),
                        value_pair.as_span(),
                        format!("Fetch offset must be non-negative, got: {value}"),
                    ));
                }
                Ok(OffsetMode::OffsetExpr(i64_literal_expr(value)))
            }
            Rule::expression => {
                let expr = Expression::parse_pair(extensions, value_pair)?;
                Ok(OffsetMode::OffsetExpr(Box::new(expr)))
            }
            _ => Err(MessageParseError::invalid(
                Self::message(),
                value_pair.as_span(),
                format!("Unexpected rule for OffsetMode: {:?}", value_pair.as_rule()),
            )),
        }
    }
}

impl RelationParsePair for FetchRel {
    fn rule() -> Rule {
        Rule::fetch_relation
    }

    fn message() -> &'static str {
        "FetchRel"
    }

    fn into_rel(mut self, adv_ext: Option<AdvancedExtension>) -> Rel {
        self.advanced_extension = adv_ext;
        Rel {
            rel_type: Some(RelType::Fetch(Box::new(self))),
        }
    }

    fn parse_pair_with_context(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
        input_children: Vec<Box<Rel>>,
        input_field_count: usize,
    ) -> Result<(Self, usize), MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        let input = expect_one_child(Self::message(), &pair, input_children)?;
        let mut iter = RuleIter::from(pair.into_inner());

        // Extract all pairs before any validation: RuleIter's Drop panics on
        // incomplete consumption, so we must exhaust the iterator before any
        // early return. Validation runs after iter.done() below.
        let (limit_pair, offset_pair) = match iter.try_pop(Rule::fetch_named_arg_list) {
            None => {
                iter.pop(Rule::empty);
                (None, None)
            }
            Some(fetch_args_pair) => {
                let extractor =
                    ParsedNamedArgs::new(fetch_args_pair.into_inner(), Rule::fetch_named_arg)?;
                let (extractor, limit_pair) = extractor.pop("limit", Rule::fetch_value);
                let (extractor, offset_pair) = extractor.pop("offset", Rule::fetch_value);
                extractor.done()?;
                (limit_pair, offset_pair)
            }
        };

        let reference_list_pair = iter.pop(Rule::reference_list);
        let (emit, output_count) = parse_emit(reference_list_pair, input_field_count);
        let common = RelCommon {
            emit_kind: Some(emit),
            ..Default::default()
        };
        iter.done();

        let count_mode = limit_pair
            .map(|pair| CountMode::parse_pair(extensions, pair))
            .transpose()?;
        let offset_mode = offset_pair
            .map(|pair| OffsetMode::parse_pair(extensions, pair))
            .transpose()?;
        Ok((
            FetchRel {
                input: Some(input),
                common: Some(common),
                advanced_extension: None,
                offset_mode,
                count_mode,
            },
            output_count,
        ))
    }
}

impl ParsePair for join_rel::JoinType {
    fn rule() -> Rule {
        Rule::join_type
    }

    fn message() -> &'static str {
        "JoinType"
    }

    fn parse_pair(pair: Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());
        let join_type_str = pair.as_str().trim_start_matches('&');
        match join_type_str {
            "Inner" => join_rel::JoinType::Inner,
            "Left" => join_rel::JoinType::Left,
            "Right" => join_rel::JoinType::Right,
            "Outer" => join_rel::JoinType::Outer,
            "LeftSemi" => join_rel::JoinType::LeftSemi,
            "RightSemi" => join_rel::JoinType::RightSemi,
            "LeftAnti" => join_rel::JoinType::LeftAnti,
            "RightAnti" => join_rel::JoinType::RightAnti,
            "LeftSingle" => join_rel::JoinType::LeftSingle,
            "RightSingle" => join_rel::JoinType::RightSingle,
            "LeftMark" => join_rel::JoinType::LeftMark,
            "RightMark" => join_rel::JoinType::RightMark,
            _ => panic!("Unknown join type: {join_type_str} (this should be caught by grammar)"),
        }
    }
}

impl RelationParsePair for JoinRel {
    fn rule() -> Rule {
        Rule::join_relation
    }

    fn message() -> &'static str {
        "JoinRel"
    }

    fn into_rel(mut self, adv_ext: Option<AdvancedExtension>) -> Rel {
        self.advanced_extension = adv_ext;
        Rel {
            rel_type: Some(RelType::Join(Box::new(self))),
        }
    }

    fn parse_pair_with_context(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
        input_children: Vec<Box<Rel>>,
        input_field_count: usize,
    ) -> Result<(Self, usize), MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());

        if input_children.len() != 2 {
            return Err(MessageParseError::invalid(
                Self::message(),
                pair.as_span(),
                format!(
                    "JoinRel should have exactly 2 input children, got {}",
                    input_children.len()
                ),
            ));
        }

        let mut children_iter = input_children.into_iter();
        let left = children_iter.next().unwrap();
        let right = children_iter.next().unwrap();

        let mut iter = RuleIter::from(pair.into_inner());
        let join_type = iter.parse_next::<join_rel::JoinType>();
        let condition = iter.parse_next_scoped::<Expression>(extensions)?;
        let reference_list_pair = iter.pop(Rule::reference_list);
        iter.done();

        // TODO: For semi/anti joins, the direct output width differs from
        // left+right — `input_field_count` would misclassify the emit as Direct.
        // Revisit when those join types are supported.
        let (emit, output_count) = parse_emit(reference_list_pair, input_field_count);
        let common = RelCommon {
            emit_kind: Some(emit),
            ..Default::default()
        };

        Ok((
            JoinRel {
                common: Some(common),
                left: Some(left),
                right: Some(right),
                expression: Some(Box::new(condition)),
                post_join_filter: None, // not yet represented in the grammar
                r#type: join_type as i32,
                advanced_extension: None,
            },
            output_count,
        ))
    }
}

#[cfg(test)]
mod tests {
    use pest::Parser;

    use super::*;
    use crate::fixtures::TestContext;
    use crate::parser::{ExpressionParser, Rule};

    #[test]
    fn test_parse_relation() {
        // Removed: test_parse_relation for old Relation struct
    }

    #[test]
    fn test_parse_read_relation() {
        let extensions = SimpleExtensions::default();
        let read = ReadRel::parse_pair_with_context(
            &extensions,
            parse_exact(Rule::read_relation, "Read[ab.cd.ef => a:i32, b:string?]"),
            vec![],
            0,
        )
        .unwrap()
        .0;
        let names = match &read.read_type {
            Some(read_rel::ReadType::NamedTable(table)) => &table.names,
            _ => panic!("Expected NamedTable"),
        };
        assert_eq!(names, &["ab", "cd", "ef"]);
        let columns = &read
            .base_schema
            .as_ref()
            .unwrap()
            .r#struct
            .as_ref()
            .unwrap()
            .types;
        assert_eq!(columns.len(), 2);
    }

    /// Produces a ReadRel with 3 columns: a:i32, b:string?, c:i64
    fn example_read_relation() -> ReadRel {
        let extensions = SimpleExtensions::default();
        ReadRel::parse_pair_with_context(
            &extensions,
            parse_exact(
                Rule::read_relation,
                "Read[ab.cd.ef => a:i32, b:string?, c:i64]",
            ),
            vec![],
            0,
        )
        .unwrap()
        .0
    }

    #[test]
    fn test_parse_filter_relation() {
        let extensions = SimpleExtensions::default();
        let filter = FilterRel::parse_pair_with_context(
            &extensions,
            parse_exact(Rule::filter_relation, "Filter[$1 => $0, $1, $2]"),
            vec![Box::new(example_read_relation().into_rel(None))],
            3,
        )
        .unwrap()
        .0;
        // Identity mapping [0, 1, 2] over 3 inputs → Direct
        let emit_kind = filter.common.as_ref().unwrap().emit_kind.as_ref().unwrap();
        assert!(
            matches!(emit_kind, EmitKind::Direct(_)),
            "Expected Direct for identity emit, got {emit_kind:?}"
        );
    }

    #[test]
    fn test_parse_project_relation() {
        let extensions = SimpleExtensions::default();
        let project = ProjectRel::parse_pair_with_context(
            &extensions,
            parse_exact(Rule::project_relation, "Project[$0, $1, 42]"),
            vec![Box::new(example_read_relation().into_rel(None))],
            3,
        )
        .unwrap()
        .0;

        // Should have 1 expression (42) and 2 references ($0, $1)
        assert_eq!(project.expressions.len(), 1);

        let emit_kind = &project.common.as_ref().unwrap().emit_kind.as_ref().unwrap();
        let emit = match emit_kind {
            EmitKind::Emit(emit) => &emit.output_mapping,
            _ => panic!("Expected EmitKind::Emit, got {emit_kind:?}"),
        };
        // Output mapping should be [0, 1, 3]. References are 0-2; expression is 3.
        assert_eq!(emit, &[0, 1, 3]);
    }

    #[test]
    fn test_parse_project_relation_complex() {
        let extensions = SimpleExtensions::default();
        let project = ProjectRel::parse_pair_with_context(
            &extensions,
            parse_exact(Rule::project_relation, "Project[42, $0, 100, $2, $1]"),
            vec![Box::new(example_read_relation().into_rel(None))],
            5, // Assume 5 input fields
        )
        .unwrap()
        .0;

        // Should have 2 expressions (42, 100) and 3 references ($0, $2, $1)
        assert_eq!(project.expressions.len(), 2);

        let emit_kind = &project.common.as_ref().unwrap().emit_kind.as_ref().unwrap();
        let emit = match emit_kind {
            EmitKind::Emit(emit) => &emit.output_mapping,
            _ => panic!("Expected EmitKind::Emit, got {emit_kind:?}"),
        };
        // Direct mapping: [input_fields..., 42, 100] (input fields first, then expressions)
        // Output mapping: [5, 0, 6, 2, 1] (to get: 42, $0, 100, $2, $1)
        assert_eq!(emit, &[5, 0, 6, 2, 1]);
    }

    #[test]
    fn test_parse_aggregate_relation() {
        let extensions = TestContext::new()
            .with_urn(1, "https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml")
            .with_function(1, 10, "sum")
            .with_function(1, 11, "count")
            .extensions;

        let aggregate = AggregateRel::parse_pair_with_context(
            &extensions,
            parse_exact(
                Rule::aggregate_relation,
                "Aggregate[($0, $1), _ => sum($2), $0, count($2)]",
            ),
            vec![Box::new(example_read_relation().into_rel(None))],
            3,
        )
        .unwrap()
        .0;

        // Should have 2 group-by sets ($0, $1) and an empty group, and emit 2 measures (sum($2), count($2))
        assert_eq!(aggregate.grouping_expressions.len(), 2);
        assert_eq!(aggregate.groupings[0].expression_references.len(), 2);
        assert_eq!(aggregate.groupings.len(), 2);
        assert_eq!(aggregate.measures.len(), 2);

        let emit_kind = &aggregate
            .common
            .as_ref()
            .unwrap()
            .emit_kind
            .as_ref()
            .unwrap();
        let emit = match emit_kind {
            EmitKind::Emit(emit) => &emit.output_mapping,
            _ => panic!("Expected EmitKind::Emit, got {emit_kind:?}"),
        };
        // Output mapping should be [2, 0, 3] (measures and group-by fields in order)
        // sum($2) -> 2, $0 -> 0, count($2) -> 3
        assert_eq!(emit, &[2, 0, 3]);
    }

    #[test]
    fn test_parse_aggregate_relation_maintain_column_order() {
        let extensions = TestContext::new()
            .with_urn(1, "https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml")
            .with_function(1, 10, "sum")
            .with_function(1, 11, "count")
            .extensions;

        let aggregate = AggregateRel::parse_pair_with_context(
            &extensions,
            parse_exact(
                Rule::aggregate_relation,
                "Aggregate[$0 => sum($1), $0, count($1)]",
            ),
            vec![Box::new(example_read_relation().into_rel(None))],
            3,
        )
        .unwrap()
        .0;

        // Should have 1 group-by field ($0) and 2 measures (sum($1), count($1))
        assert_eq!(aggregate.grouping_expressions.len(), 1);
        assert_eq!(aggregate.groupings.len(), 1);
        assert_eq!(aggregate.measures.len(), 2);

        let emit_kind = &aggregate
            .common
            .as_ref()
            .unwrap()
            .emit_kind
            .as_ref()
            .unwrap();
        let emit = match emit_kind {
            EmitKind::Emit(emit) => &emit.output_mapping,
            _ => panic!("Expected EmitKind::Emit, got {emit_kind:?}"),
        };
        // Output mapping should be [1, 0, 2] (grouping fields + measures)
        assert_eq!(emit, &[1, 0, 2]);
    }

    #[test]
    fn test_parse_aggregate_relation_simple() {
        let extensions = TestContext::new()
            .with_urn(1, "https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml")
            .with_function(1, 10, "sum")
            .extensions;

        let aggregate = AggregateRel::parse_pair_with_context(
            &extensions,
            parse_exact(Rule::aggregate_relation, "Aggregate[$2, $0 => sum($1)]"),
            vec![Box::new(example_read_relation().into_rel(None))],
            3,
        )
        .unwrap()
        .0;

        assert_eq!(aggregate.grouping_expressions.len(), 2);
        assert_eq!(aggregate.groupings.len(), 1);
        // expression_references must be positions [0, 1], not raw field indices [2, 0]
        assert_eq!(aggregate.groupings[0].expression_references, vec![0, 1]);
    }

    #[test]
    fn test_parse_aggregate_relation_global_aggregate() {
        let extensions = TestContext::new()
            .with_urn(1, "https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml")
            .with_function(1, 10, "sum")
            .with_function(1, 11, "count")
            .extensions;

        let aggregate = AggregateRel::parse_pair_with_context(
            &extensions,
            parse_exact(
                Rule::aggregate_relation,
                "Aggregate[_ => sum($0), count($1)]",
            ),
            vec![Box::new(example_read_relation().into_rel(None))],
            3,
        )
        .unwrap()
        .0;

        // Should have 0 group-by fields and 2 measures
        assert_eq!(aggregate.grouping_expressions.len(), 0);
        assert_eq!(aggregate.groupings.len(), 1);
        assert_eq!(aggregate.groupings[0].expression_references.len(), 0);
        assert_eq!(aggregate.measures.len(), 2);

        // Identity mapping [0, 1] over 2 outputs (0 grouping + 2 measures) → Direct
        let emit_kind = aggregate
            .common
            .as_ref()
            .unwrap()
            .emit_kind
            .as_ref()
            .unwrap();
        assert!(
            matches!(emit_kind, EmitKind::Direct(_)),
            "Expected Direct for identity emit, got {emit_kind:?}"
        );
    }

    #[test]
    fn test_parse_aggregate_relation_grouping_sets() {
        let extensions = TestContext::new()
            .with_urn(1, "https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml")
            .with_function(1, 11, "count")
            .extensions;

        let read_rel = ReadRel::parse_pair_with_context(
            &extensions,
            parse_exact(
                Rule::read_relation,
                "Read[ab.cd.ef => a:i32, b:string?, c:i64, d:i64]",
            ),
            vec![],
            0,
        )
        .unwrap()
        .0;

        let aggregate = AggregateRel::parse_pair_with_context(
            &extensions,
            parse_exact(
                Rule::aggregate_relation,
                "Aggregate[($0, $1, $2), ($2, $0), ($1), _ => $0, $1, $2, count($3)]",
            ),
            vec![Box::new(read_rel.into_rel(None))],
            4,
        )
        .unwrap()
        .0;

        assert_eq!(aggregate.grouping_expressions.len(), 3);
        assert_eq!(aggregate.groupings.len(), 4);
        // ($0, $1, $2) -> [0, 1, 2]
        assert_eq!(aggregate.groupings[0].expression_references, vec![0, 1, 2]);
        // ($2, $0) -> [2, 0] (reuses indices, different order)
        assert_eq!(aggregate.groupings[1].expression_references, vec![2, 0]);
        // ($1) -> [1]
        assert_eq!(aggregate.groupings[2].expression_references, vec![1]);
        // _ -> empty
        assert!(aggregate.groupings[3].expression_references.is_empty());
        assert_eq!(aggregate.measures.len(), 1);
    }

    #[test]
    fn test_fetch_relation_positive_values() {
        let extensions = SimpleExtensions::default();

        // Test valid positive values should work
        let fetch_rel = FetchRel::parse_pair_with_context(
            &extensions,
            parse_exact(Rule::fetch_relation, "Fetch[limit=10, offset=5 => $0]"),
            vec![Box::new(example_read_relation().into_rel(None))],
            3,
        )
        .unwrap()
        .0;

        // Verify the limit and offset values are correct
        assert_eq!(
            fetch_rel.count_mode,
            Some(CountMode::CountExpr(i64_literal_expr(10)))
        );
        assert_eq!(
            fetch_rel.offset_mode,
            Some(OffsetMode::OffsetExpr(i64_literal_expr(5)))
        );
    }

    #[test]
    fn test_fetch_relation_negative_limit_rejected() {
        let extensions = SimpleExtensions::default();

        // Test that fetch relations with negative limits are properly rejected
        let parsed_result = ExpressionParser::parse(Rule::fetch_relation, "Fetch[limit=-5 => $0]");
        if let Ok(mut pairs) = parsed_result {
            let pair = pairs.next().unwrap();
            if pair.as_str() == "Fetch[limit=-5 => $0]" {
                // Full parse succeeded, now test that validation catches the negative value
                let result = FetchRel::parse_pair_with_context(
                    &extensions,
                    pair,
                    vec![Box::new(example_read_relation().into_rel(None))],
                    3,
                );
                assert!(result.is_err());
                let error_msg = result.unwrap_err().to_string();
                assert!(error_msg.contains("Fetch limit must be non-negative"));
            } else {
                // If grammar doesn't fully support negative values, that's also acceptable
                // since it would prevent negative values at parse time
                println!("Grammar prevents negative limit values at parse time");
            }
        } else {
            // Grammar doesn't support negative values in fetch context
            println!("Grammar prevents negative limit values at parse time");
        }
    }

    #[test]
    fn test_fetch_relation_negative_offset_rejected() {
        let extensions = SimpleExtensions::default();

        // Test that fetch relations with negative offsets are properly rejected
        let parsed_result =
            ExpressionParser::parse(Rule::fetch_relation, "Fetch[offset=-10 => $0]");
        if let Ok(mut pairs) = parsed_result {
            let pair = pairs.next().unwrap();
            if pair.as_str() == "Fetch[offset=-10 => $0]" {
                // Full parse succeeded, now test that validation catches the negative value
                let result = FetchRel::parse_pair_with_context(
                    &extensions,
                    pair,
                    vec![Box::new(example_read_relation().into_rel(None))],
                    3,
                );
                assert!(result.is_err());
                let error_msg = result.unwrap_err().to_string();
                assert!(error_msg.contains("Fetch offset must be non-negative"));
            } else {
                // If grammar doesn't fully support negative values, that's also acceptable
                println!("Grammar prevents negative offset values at parse time");
            }
        } else {
            // Grammar doesn't support negative values in fetch context
            println!("Grammar prevents negative offset values at parse time");
        }
    }

    #[test]
    fn test_parse_join_relation() {
        let extensions = TestContext::new()
            .with_urn(1, "https://github.com/substrait-io/substrait/blob/main/extensions/functions_comparison.yaml")
            .with_function(1, 10, "eq")
            .extensions;

        let left_rel = example_read_relation().into_rel(None);
        let right_rel = example_read_relation().into_rel(None);

        let join = JoinRel::parse_pair_with_context(
            &extensions,
            parse_exact(
                Rule::join_relation,
                "Join[&Inner, eq($0, $3) => $0, $1, $3, $4]",
            ),
            vec![Box::new(left_rel), Box::new(right_rel)],
            6, // left (3) + right (3) = 6 total input fields
        )
        .unwrap()
        .0;

        // Should be an Inner join
        assert_eq!(join.r#type, join_rel::JoinType::Inner as i32);

        // Should have left and right relations
        assert!(join.left.is_some());
        assert!(join.right.is_some());

        // Should have a join condition
        assert!(join.expression.is_some());

        let emit_kind = &join.common.as_ref().unwrap().emit_kind.as_ref().unwrap();
        let emit = match emit_kind {
            EmitKind::Emit(emit) => &emit.output_mapping,
            _ => panic!("Expected EmitKind::Emit, got {emit_kind:?}"),
        };
        // Output mapping should be [0, 1, 3, 4] (selected columns)
        assert_eq!(emit, &[0, 1, 3, 4]);
    }

    #[test]
    fn test_parse_join_relation_left_outer() {
        let extensions = TestContext::new()
            .with_urn(1, "https://github.com/substrait-io/substrait/blob/main/extensions/functions_comparison.yaml")
            .with_function(1, 10, "eq")
            .extensions;

        let left_rel = example_read_relation().into_rel(None);
        let right_rel = example_read_relation().into_rel(None);

        let join = JoinRel::parse_pair_with_context(
            &extensions,
            parse_exact(Rule::join_relation, "Join[&Left, eq($0, $3) => $0, $1, $2]"),
            vec![Box::new(left_rel), Box::new(right_rel)],
            6,
        )
        .unwrap()
        .0;

        // Should be a Left join
        assert_eq!(join.r#type, join_rel::JoinType::Left as i32);

        let emit_kind = &join.common.as_ref().unwrap().emit_kind.as_ref().unwrap();
        let emit = match emit_kind {
            EmitKind::Emit(emit) => &emit.output_mapping,
            _ => panic!("Expected EmitKind::Emit, got {emit_kind:?}"),
        };
        // Output mapping should be [0, 1, 2]
        assert_eq!(emit, &[0, 1, 2]);
    }

    #[test]
    fn test_parse_join_relation_left_semi() {
        let extensions = TestContext::new()
            .with_urn(1, "https://github.com/substrait-io/substrait/blob/main/extensions/functions_comparison.yaml")
            .with_function(1, 10, "eq")
            .extensions;

        let left_rel = example_read_relation().into_rel(None);
        let right_rel = example_read_relation().into_rel(None);

        let join = JoinRel::parse_pair_with_context(
            &extensions,
            parse_exact(Rule::join_relation, "Join[&LeftSemi, eq($0, $3) => $0, $1]"),
            vec![Box::new(left_rel), Box::new(right_rel)],
            6,
        )
        .unwrap()
        .0;

        // Should be a LeftSemi join
        assert_eq!(join.r#type, join_rel::JoinType::LeftSemi as i32);

        let emit_kind = &join.common.as_ref().unwrap().emit_kind.as_ref().unwrap();
        let emit = match emit_kind {
            EmitKind::Emit(emit) => &emit.output_mapping,
            _ => panic!("Expected EmitKind::Emit, got {emit_kind:?}"),
        };
        // Output mapping should be [0, 1] (only left columns for semi join)
        assert_eq!(emit, &[0, 1]);
    }

    #[test]
    fn test_parse_join_relation_requires_two_children() {
        let extensions = SimpleExtensions::default();

        // Test with 0 children
        let result = JoinRel::parse_pair_with_context(
            &extensions,
            parse_exact(Rule::join_relation, "Join[&Inner, eq($0, $1) => $0, $1]"),
            vec![],
            0,
        );
        assert!(result.is_err());

        // Test with 1 child
        let result = JoinRel::parse_pair_with_context(
            &extensions,
            parse_exact(Rule::join_relation, "Join[&Inner, eq($0, $1) => $0, $1]"),
            vec![Box::new(example_read_relation().into_rel(None))],
            3,
        );
        assert!(result.is_err());

        // Test with 3 children
        let result = JoinRel::parse_pair_with_context(
            &extensions,
            parse_exact(Rule::join_relation, "Join[&Inner, eq($0, $1) => $0, $1]"),
            vec![
                Box::new(example_read_relation().into_rel(None)),
                Box::new(example_read_relation().into_rel(None)),
                Box::new(example_read_relation().into_rel(None)),
            ],
            9,
        );
        assert!(result.is_err());
    }

    fn parse_exact(rule: Rule, input: &'_ str) -> pest::iterators::Pair<'_, Rule> {
        let mut pairs = ExpressionParser::parse(rule, input).unwrap();
        assert_eq!(pairs.as_str(), input);
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);
        pair
    }
}
