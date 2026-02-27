//! Parsing and lowering logic for standard (non-extension) relations.
//!
//! The parser first converts typed relation nodes into [`StandardRelationLine`]
//! semantic values, then lowers those values into Substrait `Rel` messages.

use std::collections::HashMap;

use pest_typed::Spanned;
use substrait::proto::expression::RexType;
use substrait::proto::fetch_rel::{CountMode, OffsetMode};
use substrait::proto::rel::RelType;
use substrait::proto::rel_common::{Emit, EmitKind};
use substrait::proto::sort_field::SortDirection;
use substrait::proto::{
    AggregateRel, Expression, FetchRel, FilterRel, JoinRel, NamedStruct, ProjectRel, ReadRel, Rel,
    RelCommon, SortField, SortRel, join_rel, read_rel, r#type,
};

use super::ast::{
    AggregateOutputItem, Column, FetchValue, ProjectArgument, SortFieldSpec, StandardRelationLine,
};
use crate::extensions::any::Any;
use crate::extensions::registry::ExtensionError;
use crate::extensions::{ExtensionArgs, ExtensionRegistry};
use crate::parser::common::{MessageParseError, rules};
use crate::parser::convert::{
    FieldIndex, Lower, LowerWith, ParseCtx, RelationOutputIndex, TablePath, collect_first_rest,
};
use crate::parser::errors::{ParseContext, ParseError};

/// Parsing context for relations that includes extension registry and line context.
pub(crate) struct RelationParsingContext<'a> {
    /// Registry used to resolve extension relation detail payloads.
    pub registry: &'a ExtensionRegistry,
    /// 1-based source line number for error context.
    pub line_no: i64,
    /// Original source line text for error context.
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
            Err(ExtensionError::ExtensionNotFound(_)) => Err(ParseError::UnregisteredExtension {
                name: extension_name.to_string(),
                context: ParseContext::new(self.line_no, self.line.to_string()),
            }),
            Err(err) => Err(ParseError::ExtensionDetail(
                ParseContext::new(self.line_no, self.line.to_string()),
                err,
            )),
        }
    }
}

fn parse_reference_emit(
    node: &rules::reference_list<'_>,
) -> Result<Vec<RelationOutputIndex>, MessageParseError> {
    let mut output_mapping = Vec::new();
    if let Some((first, rest)) = node.reference() {
        output_mapping.push(FieldIndex::try_from(first)?.into());
        for reference in rest {
            output_mapping.push(FieldIndex::try_from(reference)?.into());
        }
    }
    Ok(output_mapping)
}

fn parse_read_line(
    cx: &ParseCtx<'_>,
    node: &rules::read_relation<'_>,
) -> Result<StandardRelationLine, MessageParseError> {
    let mut columns = Vec::new();
    if let Some((first, rest)) = node.named_column_list().named_column() {
        let named_columns = collect_first_rest(first, rest);
        for named in named_columns {
            columns.push(Column {
                name: named.name().into(),
                typ: named.r#type().lower(cx)?,
            });
        }
    }

    Ok(StandardRelationLine::Read {
        table: TablePath::from(node.table_name()),
        columns,
    })
}

fn parse_filter_line(
    cx: &ParseCtx<'_>,
    node: &rules::filter_relation<'_>,
) -> Result<StandardRelationLine, MessageParseError> {
    Ok(StandardRelationLine::Filter {
        condition: node.expression().lower(cx)?,
        emit: parse_reference_emit(node.reference_list())?,
    })
}

fn parse_project_line(
    cx: &ParseCtx<'_>,
    node: &rules::project_relation<'_>,
) -> Result<StandardRelationLine, MessageParseError> {
    let mut args = Vec::new();
    if let Some((first, rest)) = node.project_argument_list().project_argument() {
        let values = collect_first_rest(first, rest);
        for arg in values {
            if let Some(reference) = arg.reference() {
                args.push(ProjectArgument::Reference(FieldIndex::try_from(reference)?));
                continue;
            }
            if let Some(expression) = arg.expression() {
                args.push(ProjectArgument::Expression(Box::new(expression.lower(cx)?)));
                continue;
            }
            // Grammar guarantees project_argument is either reference or expression.
            unreachable!("project_argument must be reference or expression");
        }
    }

    Ok(StandardRelationLine::Project { args })
}

fn parse_aggregate_line(
    cx: &ParseCtx<'_>,
    node: &rules::aggregate_relation<'_>,
) -> Result<StandardRelationLine, MessageParseError> {
    let mut group_by = Vec::new();
    if let Some((first, rest)) = node.aggregate_group_by().reference() {
        let refs = collect_first_rest(first, rest);
        for reference in refs {
            group_by.push(FieldIndex::try_from(reference)?);
        }
    }

    let mut outputs = Vec::new();
    if let Some((first, rest)) = node.aggregate_output().aggregate_output_item() {
        let items = collect_first_rest(first, rest);
        for item in items {
            if let Some(reference) = item.reference() {
                outputs.push(AggregateOutputItem::Reference(FieldIndex::try_from(
                    reference,
                )?));
                continue;
            }
            if let Some(aggregate_measure) = item.aggregate_measure() {
                let measure = aggregate_measure.lower(cx)?;
                let aggregate = measure.measure.ok_or_else(|| {
                    MessageParseError::invalid(
                        "AggregateRel",
                        item.span(),
                        "Aggregate measure is missing function payload",
                    )
                })?;
                outputs.push(AggregateOutputItem::Measure(aggregate));
                continue;
            }
            // Grammar guarantees aggregate_output_item is reference or aggregate_measure.
            unreachable!("aggregate_output_item must be reference or aggregate_measure");
        }
    }

    Ok(StandardRelationLine::Aggregate { group_by, outputs })
}

fn parse_sort_direction(
    node: &rules::sort_direction<'_>,
) -> Result<SortDirection, MessageParseError> {
    match node.span.as_str().trim_start_matches('&') {
        "AscNullsFirst" => Ok(SortDirection::AscNullsFirst),
        "AscNullsLast" => Ok(SortDirection::AscNullsLast),
        "DescNullsFirst" => Ok(SortDirection::DescNullsFirst),
        "DescNullsLast" => Ok(SortDirection::DescNullsLast),
        other => Err(MessageParseError::invalid(
            "SortDirection",
            node.span(),
            format!("Unknown sort direction: {other}"),
        )),
    }
}

fn parse_sort_line(
    _cx: &ParseCtx<'_>,
    node: &rules::sort_relation<'_>,
) -> Result<StandardRelationLine, MessageParseError> {
    let mut fields = Vec::new();
    if let Some((first, rest)) = node.sort_field_list().sort_field() {
        let items = collect_first_rest(first, rest);
        for item in items {
            fields.push(SortFieldSpec {
                field: FieldIndex::try_from(item.reference())?,
                direction: parse_sort_direction(item.sort_direction())?,
            });
        }
    }

    Ok(StandardRelationLine::Sort {
        fields,
        emit: parse_reference_emit(node.reference_list())?,
    })
}

fn parse_fetch_value(
    cx: &ParseCtx<'_>,
    node: &rules::fetch_value<'_>,
) -> Result<FetchValue, MessageParseError> {
    if let Some(integer) = node.integer() {
        let value = integer.span.as_str().parse::<i64>().map_err(|error| {
            MessageParseError::invalid(
                "FetchValue",
                integer.span(),
                format!("Invalid integer: {error}"),
            )
        })?;
        return Ok(FetchValue::Integer(value));
    }

    if let Some(expression) = node.expression() {
        return Ok(FetchValue::Expression(Box::new(expression.lower(cx)?)));
    }

    Err(MessageParseError::invalid(
        "FetchValue",
        node.span(),
        "Unexpected value for Fetch argument",
    ))
}

fn parse_fetch_line(
    cx: &ParseCtx<'_>,
    node: &rules::fetch_relation<'_>,
) -> Result<StandardRelationLine, MessageParseError> {
    let mut named_values: HashMap<String, (&rules::fetch_value<'_>, pest_typed::Span<'_>)> =
        HashMap::new();

    if let Some(named_arg_list) = node.fetch_args().fetch_named_arg_list() {
        let (first, rest) = named_arg_list.fetch_named_arg();
        let args = collect_first_rest(first, rest);

        for arg in args {
            let key = arg.fetch_arg_name().span.as_str().to_string();
            if named_values.contains_key(&key) {
                return Err(MessageParseError::invalid(
                    "FetchRel",
                    arg.fetch_arg_name().span(),
                    format!("Duplicate argument: {}", arg.fetch_arg_name().span.as_str()),
                ));
            }
            named_values.insert(key, (arg.fetch_value(), arg.fetch_arg_name().span()));
        }
    }

    let limit = match named_values.remove("limit") {
        Some((value, span)) => {
            let parsed = parse_fetch_value(cx, value)?;
            if let FetchValue::Integer(v) = parsed {
                if v < 0 {
                    return Err(MessageParseError::invalid(
                        "CountMode",
                        span,
                        format!("Fetch limit must be non-negative, got: {v}"),
                    ));
                }
                Some(FetchValue::Integer(v))
            } else {
                Some(parsed)
            }
        }
        None => None,
    };

    let offset = match named_values.remove("offset") {
        Some((value, span)) => {
            let parsed = parse_fetch_value(cx, value)?;
            if let FetchValue::Integer(v) = parsed {
                if v < 0 {
                    return Err(MessageParseError::invalid(
                        "OffsetMode",
                        span,
                        format!("Fetch offset must be non-negative, got: {v}"),
                    ));
                }
                Some(FetchValue::Integer(v))
            } else {
                Some(parsed)
            }
        }
        None => None,
    };

    if let Some((unknown_name, (_, span))) = named_values.into_iter().next() {
        return Err(MessageParseError::invalid(
            "FetchRel",
            span,
            format!("Unknown argument: {unknown_name}"),
        ));
    }

    Ok(StandardRelationLine::Fetch {
        limit,
        offset,
        emit: parse_reference_emit(node.reference_list())?,
    })
}

fn parse_join_type(node: &rules::join_type<'_>) -> Result<join_rel::JoinType, MessageParseError> {
    match node.span.as_str().trim_start_matches('&') {
        "Inner" => Ok(join_rel::JoinType::Inner),
        "Left" => Ok(join_rel::JoinType::Left),
        "Right" => Ok(join_rel::JoinType::Right),
        "Outer" => Ok(join_rel::JoinType::Outer),
        "LeftSemi" => Ok(join_rel::JoinType::LeftSemi),
        "RightSemi" => Ok(join_rel::JoinType::RightSemi),
        "LeftAnti" => Ok(join_rel::JoinType::LeftAnti),
        "RightAnti" => Ok(join_rel::JoinType::RightAnti),
        "LeftSingle" => Ok(join_rel::JoinType::LeftSingle),
        "RightSingle" => Ok(join_rel::JoinType::RightSingle),
        "LeftMark" => Ok(join_rel::JoinType::LeftMark),
        "RightMark" => Ok(join_rel::JoinType::RightMark),
        other => Err(MessageParseError::invalid(
            "JoinType",
            node.span(),
            format!("Unknown join type: {other}"),
        )),
    }
}

fn parse_join_line(
    cx: &ParseCtx<'_>,
    node: &rules::join_relation<'_>,
) -> Result<StandardRelationLine, MessageParseError> {
    Ok(StandardRelationLine::Join {
        join_type: parse_join_type(node.join_type())?,
        expression: Box::new(node.expression().lower(cx)?),
        emit: parse_reference_emit(node.reference_list())?,
    })
}

pub(crate) fn parse_standard_relation(
    cx: &ParseCtx<'_>,
    relation: &rules::relation<'_>,
) -> Result<StandardRelationLine, MessageParseError> {
    if let Some(read_relation) = relation.read_relation() {
        return parse_read_line(cx, read_relation);
    }
    if let Some(filter_relation) = relation.filter_relation() {
        return parse_filter_line(cx, filter_relation);
    }
    if let Some(project_relation) = relation.project_relation() {
        return parse_project_line(cx, project_relation);
    }
    if let Some(aggregate_relation) = relation.aggregate_relation() {
        return parse_aggregate_line(cx, aggregate_relation);
    }
    if let Some(sort_relation) = relation.sort_relation() {
        return parse_sort_line(cx, sort_relation);
    }
    if let Some(fetch_relation) = relation.fetch_relation() {
        return parse_fetch_line(cx, fetch_relation);
    }
    if let Some(join_relation) = relation.join_relation() {
        return parse_join_line(cx, join_relation);
    }
    if relation.extension_relation().is_some() {
        // Structural parsing routes extension relations before calling this helper.
        unreachable!("parse_standard_relation called with extension relation")
    }

    // Grammar guarantees relation matches one known relation variant.
    unreachable!("relation must be a known relation type")
}

#[cfg(test)]
pub(crate) fn parse_standard_relation_line(
    cx: &ParseCtx<'_>,
    line: &str,
) -> Result<StandardRelationLine, MessageParseError> {
    let relation = crate::parser::common::parse_typed::<rules::relation<'_>>(line, "relation")?;
    parse_standard_relation(cx, &relation)
}

fn emit_from(indices: Vec<RelationOutputIndex>) -> EmitKind {
    EmitKind::Emit(Emit {
        output_mapping: indices.into_iter().map(i32::from).collect(),
    })
}

#[allow(clippy::vec_box)]
fn expect_one_child(
    message: &'static str,
    line: &str,
    mut input_children: Vec<Box<Rel>>,
) -> Result<Box<Rel>, MessageParseError> {
    match input_children.len() {
        0 => Err(MessageParseError::invalid_line(
            message,
            line,
            format!("{message} missing child"),
        )),
        1 => Ok(input_children.pop().expect("length checked above")),
        n => Err(MessageParseError::invalid_line(
            message,
            line,
            format!("{message} should have 1 input child, got {n}"),
        )),
    }
}

#[allow(clippy::vec_box)]
pub(crate) fn lower_standard_relation_line(
    line: StandardRelationLine,
    line_text: &str,
    input_children: Vec<Box<Rel>>,
    input_field_count: usize,
) -> Result<Rel, MessageParseError> {
    match line {
        StandardRelationLine::Read { table, columns } => {
            if !input_children.is_empty() {
                return Err(MessageParseError::invalid_line(
                    "ReadRel",
                    line_text,
                    "ReadRel should have no input children",
                ));
            }
            if input_field_count != 0 {
                return Err(MessageParseError::invalid_line(
                    "ReadRel",
                    line_text,
                    "ReadRel should have 0 input fields",
                ));
            }

            let (names, types): (Vec<_>, Vec<_>) =
                columns.into_iter().map(|c| (c.name.0, c.typ)).unzip();

            let struct_ = r#type::Struct {
                types,
                type_variation_reference: 0,
                nullability: r#type::Nullability::Required as i32,
            };

            Ok(Rel {
                rel_type: Some(RelType::Read(Box::new(ReadRel {
                    base_schema: Some(NamedStruct {
                        names,
                        r#struct: Some(struct_),
                    }),
                    read_type: Some(read_rel::ReadType::NamedTable(read_rel::NamedTable {
                        names: table.0.into_iter().map(|name| name.0).collect(),
                        advanced_extension: None,
                    })),
                    ..Default::default()
                }))),
            })
        }
        StandardRelationLine::Filter { condition, emit } => {
            let input = expect_one_child("FilterRel", line_text, input_children)?;
            Ok(Rel {
                rel_type: Some(RelType::Filter(Box::new(FilterRel {
                    input: Some(input),
                    condition: Some(Box::new(condition)),
                    common: Some(RelCommon {
                        emit_kind: Some(emit_from(emit)),
                        ..Default::default()
                    }),
                    advanced_extension: None,
                }))),
            })
        }
        StandardRelationLine::Project { args } => {
            let input = expect_one_child("ProjectRel", line_text, input_children)?;

            let mut expressions = Vec::new();
            let mut output_mapping = Vec::new();
            for arg in args {
                match arg {
                    ProjectArgument::Reference(reference) => output_mapping.push(reference.0),
                    ProjectArgument::Expression(expression) => {
                        expressions.push(*expression);
                        output_mapping
                            .push(input_field_count as i32 + (expressions.len() as i32 - 1));
                    }
                }
            }

            Ok(Rel {
                rel_type: Some(RelType::Project(Box::new(ProjectRel {
                    input: Some(input),
                    expressions,
                    common: Some(RelCommon {
                        emit_kind: Some(EmitKind::Emit(Emit { output_mapping })),
                        ..Default::default()
                    }),
                    advanced_extension: None,
                }))),
            })
        }
        StandardRelationLine::Aggregate { group_by, outputs } => {
            let input = expect_one_child("AggregateRel", line_text, input_children)?;

            let mut grouping_expressions = Vec::new();
            for field in group_by {
                grouping_expressions.push(Expression {
                    rex_type: Some(RexType::Selection(Box::new(field.into()))),
                });
            }

            let mut measures = Vec::new();
            let mut output_mapping = Vec::new();
            let group_by_count = grouping_expressions.len();
            let mut measure_count = 0;

            for output_item in outputs {
                match output_item {
                    AggregateOutputItem::Reference(reference) => output_mapping.push(reference.0),
                    AggregateOutputItem::Measure(function) => {
                        measures.push(substrait::proto::aggregate_rel::Measure {
                            measure: Some(function),
                            filter: None,
                        });
                        output_mapping.push(group_by_count as i32 + measure_count);
                        measure_count += 1;
                    }
                }
            }

            Ok(Rel {
                rel_type: Some(RelType::Aggregate(Box::new(AggregateRel {
                    input: Some(input),
                    grouping_expressions,
                    groupings: vec![],
                    measures,
                    common: Some(RelCommon {
                        emit_kind: Some(EmitKind::Emit(Emit { output_mapping })),
                        ..Default::default()
                    }),
                    advanced_extension: None,
                }))),
            })
        }
        StandardRelationLine::Sort { fields, emit } => {
            let input = expect_one_child("SortRel", line_text, input_children)?;
            let sorts = fields
                .into_iter()
                .map(|field| SortField {
                    expr: Some(Expression {
                        rex_type: Some(RexType::Selection(Box::new(field.field.into()))),
                    }),
                    sort_kind: Some(substrait::proto::sort_field::SortKind::Direction(
                        field.direction as i32,
                    )),
                })
                .collect();

            Ok(Rel {
                rel_type: Some(RelType::Sort(Box::new(SortRel {
                    input: Some(input),
                    sorts,
                    common: Some(RelCommon {
                        emit_kind: Some(emit_from(emit)),
                        ..Default::default()
                    }),
                    advanced_extension: None,
                }))),
            })
        }
        StandardRelationLine::Fetch {
            limit,
            offset,
            emit,
        } => {
            let input = expect_one_child("FetchRel", line_text, input_children)?;
            let count_mode: Option<CountMode> = limit.map(FetchValue::into_count_mode);
            let offset_mode: Option<OffsetMode> = offset.map(FetchValue::into_offset_mode);

            Ok(Rel {
                rel_type: Some(RelType::Fetch(Box::new(FetchRel {
                    input: Some(input),
                    common: Some(RelCommon {
                        emit_kind: Some(emit_from(emit)),
                        ..Default::default()
                    }),
                    advanced_extension: None,
                    offset_mode,
                    count_mode,
                }))),
            })
        }
        StandardRelationLine::Join {
            join_type,
            expression,
            emit,
        } => {
            if input_children.len() != 2 {
                return Err(MessageParseError::invalid_line(
                    "JoinRel",
                    line_text,
                    format!(
                        "JoinRel should have exactly 2 input children, got {}",
                        input_children.len()
                    ),
                ));
            }

            let mut children_iter = input_children.into_iter();
            let left = children_iter.next().expect("len checked above");
            let right = children_iter.next().expect("len checked above");

            Ok(Rel {
                rel_type: Some(RelType::Join(Box::new(JoinRel {
                    common: Some(RelCommon {
                        emit_kind: Some(emit_from(emit)),
                        ..Default::default()
                    }),
                    left: Some(left),
                    right: Some(right),
                    expression: Some(expression),
                    post_join_filter: None,
                    r#type: join_type as i32,
                    advanced_extension: None,
                }))),
            })
        }
    }
}

#[allow(clippy::vec_box)]
pub(crate) struct LowerStandardInput<'a> {
    /// Source relation line being lowered.
    pub line_text: &'a str,
    /// Already-lowered input children for this relation.
    pub input_children: Vec<Box<Rel>>,
    /// Total input field count visible to this relation.
    pub input_field_count: usize,
}

impl<'a> LowerWith<LowerStandardInput<'a>> for StandardRelationLine {
    type Output = Rel;

    fn lower_with(
        &self,
        _cx: &ParseCtx<'_>,
        input: LowerStandardInput<'a>,
    ) -> Result<Self::Output, MessageParseError> {
        lower_standard_relation_line(
            self.clone(),
            input.line_text,
            input.input_children,
            input.input_field_count,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::extensions::SimpleExtensions;

    fn cx() -> ParseCtx<'static> {
        static EXTENSIONS: std::sync::LazyLock<SimpleExtensions> =
            std::sync::LazyLock::new(SimpleExtensions::new);
        ParseCtx {
            extensions: &EXTENSIONS,
        }
    }

    #[test]
    fn parse_read_line() {
        let line = "Read[ab.cd.ef => a:i32, b:string?]";
        let parsed = parse_standard_relation_line(&cx(), line).unwrap();
        match parsed {
            StandardRelationLine::Read { table, columns } => {
                assert_eq!(table.0.len(), 3);
                assert_eq!(columns.len(), 2);
            }
            other => panic!("Expected read, got {other:?}"),
        }
    }

    #[test]
    fn parse_fetch_negative_limit_errors() {
        let line = "Fetch[limit=-5 => $0]";
        let err = parse_standard_relation_line(&cx(), line).unwrap_err();
        assert!(err.to_string().contains("non-negative"));
    }

    #[test]
    fn lower_join_requires_two_children() {
        let parsed = parse_standard_relation_line(&cx(), "Join[&Inner, true => $0]").unwrap();
        let line = "Join[&Inner, true => $0]";
        let err = lower_standard_relation_line(parsed, line, vec![], 0).unwrap_err();
        assert!(err.to_string().contains("exactly 2 input children"));
    }
}
