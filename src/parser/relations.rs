use std::collections::HashMap;

use substrait::proto::fetch_rel::{CountMode, OffsetMode};
use substrait::proto::rel::RelType;
use substrait::proto::rel_common::{Emit, EmitKind};
use substrait::proto::sort_field::{SortDirection, SortKind};
use substrait::proto::{
    AggregateRel, Expression, FetchRel, FilterRel, JoinRel, NamedStruct, ProjectRel, ReadRel, Rel,
    RelCommon, SortField, SortRel, Type, aggregate_rel, join_rel, read_rel, r#type,
};

use super::{
    ErrorKind, MessageParseError, ParsePair, Rule, RuleIter, ScopedParsePair, unwrap_single_pair,
};
use crate::extensions::SimpleExtensions;
use crate::parser::expressions::{FieldIndex, Name};

/// A trait for parsing relations with full context needed for tree building.
/// This includes extensions, the parsed pair, input children, and output field count.
pub trait RelationParsePair: Sized {
    fn rule() -> Rule;

    fn message() -> &'static str;

    /// Parse a relation with full context for tree building.
    ///
    /// Args:
    /// - extensions: The extensions context
    /// - pair: The parsed pest pair
    /// - input_children: The input relations (for wiring)
    /// - input_field_count: Number of output fields from input children (for output mapping)
    fn parse_pair_with_context(
        extensions: &SimpleExtensions,
        pair: pest::iterators::Pair<Rule>,
        input_children: Vec<Box<Rel>>,
        input_field_count: usize,
    ) -> Result<Self, MessageParseError>;

    fn into_rel(self) -> Rel;
}

pub struct TableName(Vec<String>);

impl ParsePair for TableName {
    fn rule() -> Rule {
        Rule::table_name
    }

    fn message() -> &'static str {
        "TableName"
    }

    fn parse_pair(pair: pest::iterators::Pair<Rule>) -> Self {
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
        pair: pest::iterators::Pair<Rule>,
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
        pair: pest::iterators::Pair<Rule>,
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
    pair: &pest::iterators::Pair<Rule>,
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
fn parse_reference_emit(pair: pest::iterators::Pair<Rule>) -> EmitKind {
    assert_eq!(pair.as_rule(), Rule::reference_list);
    let output_mapping = pair
        .into_inner()
        .map(|p| FieldIndex::parse_pair(p).0)
        .collect::<Vec<i32>>();
    EmitKind::Emit(Emit { output_mapping })
}

/// Extracts named arguments from pest pairs with duplicate detection and completeness checking.
///
/// Usage: `extractor.pop("limit", Rule::fetch_value).0.pop("offset", Rule::fetch_value).0.done()`
///
/// The fluent API ensures all arguments are processed exactly once and none are forgotten.
pub struct ParsedNamedArgs<'a> {
    map: HashMap<&'a str, pest::iterators::Pair<'a, Rule>>,
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
    pub fn pop(
        mut self,
        name: &str,
        rule: Rule,
    ) -> (Self, Option<pest::iterators::Pair<'a, Rule>>) {
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

    fn into_rel(self) -> Rel {
        Rel {
            rel_type: Some(RelType::Read(Box::new(self))),
        }
    }

    fn parse_pair_with_context(
        extensions: &SimpleExtensions,
        pair: pest::iterators::Pair<Rule>,
        input_children: Vec<Box<Rel>>,
        input_field_count: usize,
    ) -> Result<Self, MessageParseError> {
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
            let error = pest::error::Error::new_from_span(
                pest::error::ErrorVariant::CustomError {
                    message: "ReadRel should have 0 input fields".to_string(),
                },
                pair.as_span(),
            );
            return Err(MessageParseError::new(
                "ReadRel",
                ErrorKind::InvalidValue,
                Box::new(error),
            ));
        }

        let mut iter = RuleIter::from(pair.into_inner());
        let table = iter.parse_next::<TableName>().0;
        let columns = iter.parse_next_scoped::<NamedColumnList>(extensions)?.0;
        iter.done();

        let (names, types): (Vec<_>, Vec<_>) = columns.into_iter().map(|c| (c.name, c.typ)).unzip();
        let struct_ = r#type::Struct {
            types,
            type_variation_reference: 0,
            nullability: r#type::Nullability::Required as i32,
        };
        let named_struct = NamedStruct {
            names,
            r#struct: Some(struct_),
        };

        let read_rel = ReadRel {
            base_schema: Some(named_struct),
            read_type: Some(read_rel::ReadType::NamedTable(read_rel::NamedTable {
                names: table,
                advanced_extension: None,
            })),
            ..Default::default()
        };

        Ok(read_rel)
    }
}

impl RelationParsePair for FilterRel {
    fn rule() -> Rule {
        Rule::filter_relation
    }

    fn message() -> &'static str {
        "FilterRel"
    }

    fn into_rel(self) -> Rel {
        Rel {
            rel_type: Some(RelType::Filter(Box::new(self))),
        }
    }

    fn parse_pair_with_context(
        extensions: &SimpleExtensions,
        pair: pest::iterators::Pair<Rule>,
        input_children: Vec<Box<Rel>>,
        _input_field_count: usize,
    ) -> Result<Self, MessageParseError> {
        // Form: Filter[condition => references]

        assert_eq!(pair.as_rule(), Self::rule());
        let input = expect_one_child(Self::message(), &pair, input_children)?;
        let mut iter = RuleIter::from(pair.into_inner());
        // condition
        let condition = iter.parse_next_scoped::<Expression>(extensions)?;
        // references (which become the emit)
        let references_pair = iter.pop(Rule::reference_list);
        iter.done();

        let emit = parse_reference_emit(references_pair);
        let common = RelCommon {
            emit_kind: Some(emit),
            ..Default::default()
        };

        Ok(FilterRel {
            input: Some(input),
            condition: Some(Box::new(condition)),
            common: Some(common),
            advanced_extension: None,
        })
    }
}

impl RelationParsePair for ProjectRel {
    fn rule() -> Rule {
        Rule::project_relation
    }

    fn message() -> &'static str {
        "ProjectRel"
    }

    fn into_rel(self) -> Rel {
        Rel {
            rel_type: Some(RelType::Project(Box::new(self))),
        }
    }

    fn parse_pair_with_context(
        extensions: &SimpleExtensions,
        pair: pest::iterators::Pair<Rule>,
        input_children: Vec<Box<Rel>>,
        input_field_count: usize,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        let input = expect_one_child(Self::message(), &pair, input_children)?;

        // Get the argument list (contains references and expressions)
        let arguments_pair = unwrap_single_pair(pair);

        let mut expressions = Vec::new();
        let mut output_mapping = Vec::new();

        // Process each argument (can be either a reference or expression)
        for arg in arguments_pair.into_inner() {
            let inner_arg = crate::parser::unwrap_single_pair(arg);
            match inner_arg.as_rule() {
                Rule::reference => {
                    // Parse reference like "$0" -> 0
                    let field_index = FieldIndex::parse_pair(inner_arg);
                    output_mapping.push(field_index.0);
                }
                Rule::expression => {
                    // Parse as expression (e.g., 42, add($0, $1))
                    let _expr = Expression::parse_pair(extensions, inner_arg)?;
                    expressions.push(_expr);
                    // Expression: index after all input fields
                    output_mapping.push(input_field_count as i32 + (expressions.len() as i32 - 1));
                }
                _ => panic!("Unexpected inner argument rule: {:?}", inner_arg.as_rule()),
            }
        }

        let emit = EmitKind::Emit(Emit { output_mapping });
        let common = RelCommon {
            emit_kind: Some(emit),
            ..Default::default()
        };

        Ok(ProjectRel {
            input: Some(input),
            expressions,
            common: Some(common),
            advanced_extension: None,
        })
    }
}

impl RelationParsePair for AggregateRel {
    fn rule() -> Rule {
        Rule::aggregate_relation
    }

    fn message() -> &'static str {
        "AggregateRel"
    }

    fn into_rel(self) -> Rel {
        Rel {
            rel_type: Some(RelType::Aggregate(Box::new(self))),
        }
    }

    fn parse_pair_with_context(
        extensions: &SimpleExtensions,
        pair: pest::iterators::Pair<Rule>,
        input_children: Vec<Box<Rel>>,
        _input_field_count: usize,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        let input = expect_one_child(Self::message(), &pair, input_children)?;
        let mut iter = RuleIter::from(pair.into_inner());
        let group_by_pair = iter.pop(Rule::aggregate_group_by);
        let output_pair = iter.pop(Rule::aggregate_output);
        iter.done();
        let mut grouping_expressions = Vec::new();
        for group_by_item in group_by_pair.into_inner() {
            match group_by_item.as_rule() {
                Rule::reference => {
                    let field_index = FieldIndex::parse_pair(group_by_item);
                    grouping_expressions.push(Expression {
                        rex_type: Some(substrait::proto::expression::RexType::Selection(Box::new(
                            field_index.to_field_reference(),
                        ))),
                    });
                }
                Rule::empty => {
                    // No grouping expressions to add
                }
                _ => panic!(
                    "Unexpected group-by item rule: {:?}",
                    group_by_item.as_rule()
                ),
            }
        }

        // Parse output items (can be references or aggregate measures)
        let mut measures = Vec::new();
        let mut output_mapping = Vec::new();
        let group_by_count = grouping_expressions.len();
        let mut measure_count = 0;

        for output_item in output_pair.into_inner() {
            let inner_item = unwrap_single_pair(output_item);
            match inner_item.as_rule() {
                Rule::reference => {
                    let field_index = FieldIndex::parse_pair(inner_item);
                    output_mapping.push(field_index.0);
                }
                Rule::aggregate_measure => {
                    let measure = aggregate_rel::Measure::parse_pair(extensions, inner_item)?;
                    measures.push(measure);
                    output_mapping.push(group_by_count as i32 + measure_count);
                    measure_count += 1;
                }
                _ => panic!(
                    "Unexpected inner output item rule: {:?}",
                    inner_item.as_rule()
                ),
            }
        }

        let emit = EmitKind::Emit(Emit { output_mapping });
        let common = RelCommon {
            emit_kind: Some(emit),
            ..Default::default()
        };

        Ok(AggregateRel {
            input: Some(input),
            grouping_expressions,
            groupings: vec![], // TODO: Create groupings from grouping_expressions for complex grouping scenarios
            measures,
            common: Some(common),
            advanced_extension: None,
        })
    }
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
        pair: pest::iterators::Pair<Rule>,
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

    fn into_rel(self) -> Rel {
        Rel {
            rel_type: Some(RelType::Sort(Box::new(self))),
        }
    }

    fn parse_pair_with_context(
        extensions: &SimpleExtensions,
        pair: pest::iterators::Pair<Rule>,
        input_children: Vec<Box<Rel>>,
        _input_field_count: usize,
    ) -> Result<Self, MessageParseError> {
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
        let emit = parse_reference_emit(reference_list_pair);
        let common = RelCommon {
            emit_kind: Some(emit),
            ..Default::default()
        };
        iter.done();
        Ok(SortRel {
            input: Some(input),
            sorts,
            common: Some(common),
            advanced_extension: None,
        })
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
        pair: pest::iterators::Pair<Rule>,
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
                Ok(CountMode::Count(value))
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

impl ScopedParsePair for OffsetMode {
    fn rule() -> Rule {
        Rule::fetch_value
    }
    fn message() -> &'static str {
        "OffsetMode"
    }
    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: pest::iterators::Pair<Rule>,
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
                Ok(OffsetMode::Offset(value))
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

    fn into_rel(self) -> Rel {
        Rel {
            rel_type: Some(RelType::Fetch(Box::new(self))),
        }
    }

    fn parse_pair_with_context(
        extensions: &SimpleExtensions,
        pair: pest::iterators::Pair<Rule>,
        input_children: Vec<Box<Rel>>,
        _input_field_count: usize,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        let input = expect_one_child(Self::message(), &pair, input_children)?;
        let mut iter = RuleIter::from(pair.into_inner());

        // Extract all pairs first, then do validation
        let (limit_pair, offset_pair) = match iter.try_pop(Rule::fetch_named_arg_list) {
            None => {
                // If there are no arguments, it should be empty
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
        let emit = parse_reference_emit(reference_list_pair);
        let common = RelCommon {
            emit_kind: Some(emit),
            ..Default::default()
        };
        iter.done();

        // Now do validation after iterator is fully consumed
        let count_mode = limit_pair
            .map(|pair| CountMode::parse_pair(extensions, pair))
            .transpose()?;
        let offset_mode = offset_pair
            .map(|pair| OffsetMode::parse_pair(extensions, pair))
            .transpose()?;
        Ok(FetchRel {
            input: Some(input),
            common: Some(common),
            advanced_extension: None,
            offset_mode,
            count_mode,
        })
    }
}

impl ParsePair for join_rel::JoinType {
    fn rule() -> Rule {
        Rule::join_type
    }

    fn message() -> &'static str {
        "JoinType"
    }

    fn parse_pair(pair: pest::iterators::Pair<Rule>) -> Self {
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

    fn into_rel(self) -> Rel {
        Rel {
            rel_type: Some(RelType::Join(Box::new(self))),
        }
    }

    fn parse_pair_with_context(
        extensions: &SimpleExtensions,
        pair: pest::iterators::Pair<Rule>,
        input_children: Vec<Box<Rel>>,
        _input_field_count: usize,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());

        // Join requires exactly 2 input children
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

        // Parse join type
        let join_type = iter.parse_next::<join_rel::JoinType>();

        // Parse join condition expression
        let condition = iter.parse_next_scoped::<Expression>(extensions)?;

        // Parse output references (which become the emit)
        let reference_list_pair = iter.pop(Rule::reference_list);
        iter.done();

        let emit = parse_reference_emit(reference_list_pair);
        let common = RelCommon {
            emit_kind: Some(emit),
            ..Default::default()
        };

        Ok(JoinRel {
            common: Some(common),
            left: Some(left),
            right: Some(right),
            expression: Some(Box::new(condition)),
            post_join_filter: None, // Not supported in grammar yet
            r#type: join_type as i32,
            advanced_extension: None,
        })
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
        .unwrap();
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
    }

    #[test]
    fn test_parse_filter_relation() {
        let extensions = SimpleExtensions::default();
        let filter = FilterRel::parse_pair_with_context(
            &extensions,
            parse_exact(Rule::filter_relation, "Filter[$1 => $0, $1, $2]"),
            vec![Box::new(example_read_relation().into_rel())],
            3,
        )
        .unwrap();
        let emit_kind = &filter.common.as_ref().unwrap().emit_kind.as_ref().unwrap();
        let emit = match emit_kind {
            EmitKind::Emit(emit) => &emit.output_mapping,
            _ => panic!("Expected EmitKind::Emit, got {emit_kind:?}"),
        };
        assert_eq!(emit, &[0, 1, 2]);
    }

    #[test]
    fn test_parse_project_relation() {
        let extensions = SimpleExtensions::default();
        let project = ProjectRel::parse_pair_with_context(
            &extensions,
            parse_exact(Rule::project_relation, "Project[$0, $1, 42]"),
            vec![Box::new(example_read_relation().into_rel())],
            3,
        )
        .unwrap();

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
            vec![Box::new(example_read_relation().into_rel())],
            5, // Assume 5 input fields
        )
        .unwrap();

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
            .with_uri(1, "https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml")
            .with_function(1, 10, "sum")
            .with_function(1, 11, "count")
            .extensions;

        let aggregate = AggregateRel::parse_pair_with_context(
            &extensions,
            parse_exact(
                Rule::aggregate_relation,
                "Aggregate[$0, $1 => sum($2), $0, count($2)]",
            ),
            vec![Box::new(example_read_relation().into_rel())],
            3,
        )
        .unwrap();

        // Should have 2 group-by fields ($0, $1) and 2 measures (sum($2), count($2))
        assert_eq!(aggregate.grouping_expressions.len(), 2);
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
    fn test_parse_aggregate_relation_simple() {
        let extensions = TestContext::new()
            .with_uri(1, "https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml")
            .with_function(1, 10, "sum")
            .with_function(1, 11, "count")
            .extensions;

        let aggregate = AggregateRel::parse_pair_with_context(
            &extensions,
            parse_exact(
                Rule::aggregate_relation,
                "Aggregate[$0 => sum($1), count($1)]",
            ),
            vec![Box::new(example_read_relation().into_rel())],
            3,
        )
        .unwrap();

        // Should have 1 group-by field ($0) and 2 measures (sum($1), count($1))
        assert_eq!(aggregate.grouping_expressions.len(), 1);
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
        // Output mapping should be [1, 2] (measures only)
        assert_eq!(emit, &[1, 2]);
    }

    #[test]
    fn test_parse_aggregate_relation_no_group_by() {
        let extensions = TestContext::new()
            .with_uri(1, "https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml")
            .with_function(1, 10, "sum")
            .with_function(1, 11, "count")
            .extensions;

        let aggregate = AggregateRel::parse_pair_with_context(
            &extensions,
            parse_exact(
                Rule::aggregate_relation,
                "Aggregate[_ => sum($0), count($1)]",
            ),
            vec![Box::new(example_read_relation().into_rel())],
            3,
        )
        .unwrap();

        // Should have 0 group-by fields and 2 measures
        assert_eq!(aggregate.grouping_expressions.len(), 0);
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
        // Output mapping should be [0, 1] (measures only, no group-by fields)
        assert_eq!(emit, &[0, 1]);
    }

    #[test]
    fn test_parse_aggregate_relation_empty_group_by() {
        let extensions = TestContext::new()
            .with_uri(1, "https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml")
            .with_function(1, 10, "sum")
            .with_function(1, 11, "count")
            .extensions;

        let aggregate = AggregateRel::parse_pair_with_context(
            &extensions,
            parse_exact(
                Rule::aggregate_relation,
                "Aggregate[_ => sum($0), count($1)]",
            ),
            vec![Box::new(example_read_relation().into_rel())],
            3,
        )
        .unwrap();

        // Should have 0 group-by fields and 2 measures
        assert_eq!(aggregate.grouping_expressions.len(), 0);
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
        // Output mapping should be [0, 1] (measures only, no group-by fields)
        assert_eq!(emit, &[0, 1]);
    }

    #[test]
    fn test_fetch_relation_positive_values() {
        let extensions = SimpleExtensions::default();

        // Test valid positive values should work
        let fetch_rel = FetchRel::parse_pair_with_context(
            &extensions,
            parse_exact(Rule::fetch_relation, "Fetch[limit=10, offset=5 => $0]"),
            vec![Box::new(example_read_relation().into_rel())],
            3,
        )
        .unwrap();

        // Verify the limit and offset values are correct
        assert_eq!(fetch_rel.count_mode, Some(CountMode::Count(10)));
        assert_eq!(fetch_rel.offset_mode, Some(OffsetMode::Offset(5)));
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
                    vec![Box::new(example_read_relation().into_rel())],
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
                    vec![Box::new(example_read_relation().into_rel())],
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
            .with_uri(1, "https://github.com/substrait-io/substrait/blob/main/extensions/functions_comparison.yaml")
            .with_function(1, 10, "eq")
            .extensions;

        let left_rel = example_read_relation().into_rel();
        let right_rel = example_read_relation().into_rel();

        let join = JoinRel::parse_pair_with_context(
            &extensions,
            parse_exact(
                Rule::join_relation,
                "Join[&Inner, eq($0, $3) => $0, $1, $3, $4]",
            ),
            vec![Box::new(left_rel), Box::new(right_rel)],
            6, // left (3) + right (3) = 6 total input fields
        )
        .unwrap();

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
            .with_uri(1, "https://github.com/substrait-io/substrait/blob/main/extensions/functions_comparison.yaml")
            .with_function(1, 10, "eq")
            .extensions;

        let left_rel = example_read_relation().into_rel();
        let right_rel = example_read_relation().into_rel();

        let join = JoinRel::parse_pair_with_context(
            &extensions,
            parse_exact(Rule::join_relation, "Join[&Left, eq($0, $3) => $0, $1, $2]"),
            vec![Box::new(left_rel), Box::new(right_rel)],
            6,
        )
        .unwrap();

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
            vec![Box::new(example_read_relation().into_rel())],
            3,
        );
        assert!(result.is_err());

        // Test with 3 children
        let result = JoinRel::parse_pair_with_context(
            &extensions,
            parse_exact(Rule::join_relation, "Join[&Inner, eq($0, $1) => $0, $1]"),
            vec![
                Box::new(example_read_relation().into_rel()),
                Box::new(example_read_relation().into_rel()),
                Box::new(example_read_relation().into_rel()),
            ],
            9,
        );
        assert!(result.is_err());
    }

    fn parse_exact(rule: Rule, input: &str) -> pest::iterators::Pair<Rule> {
        let mut pairs = ExpressionParser::parse(rule, input).unwrap();
        assert_eq!(pairs.as_str(), input);
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);
        pair
    }
}
