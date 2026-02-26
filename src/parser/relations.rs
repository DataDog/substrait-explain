use std::collections::HashMap;

use pest_typed::Spanned;
use substrait::proto::expression::literal::LiteralType;
use substrait::proto::expression::{Literal, RexType};
use substrait::proto::fetch_rel::{CountMode, OffsetMode};
use substrait::proto::rel::RelType;
use substrait::proto::rel_common::{Emit, EmitKind};
use substrait::proto::sort_field::{SortDirection, SortKind};
use substrait::proto::{
    AggregateRel, Expression, FetchRel, FilterRel, JoinRel, NamedStruct, ProjectRel, ReadRel, Rel,
    RelCommon, SortField, SortRel, Type, join_rel, read_rel, r#type,
};

use super::common::{MessageParseError, parse_typed, rules, typed_to_pest_span};
use crate::extensions::any::Any;
use crate::extensions::registry::ExtensionError;
use crate::extensions::{ExtensionArgs, ExtensionRegistry, SimpleExtensions};
use crate::parser::errors::{ParseContext, ParseError};
use crate::parser::expressions::{
    parse_expression_node, parse_field_index_node, parse_measure_node, parse_name_node,
};
use crate::parser::types::parse_type_node;

/// Parsing context for relations that includes extensions, registry, and optional warning collection
pub(crate) struct RelationParsingContext<'a> {
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

#[derive(Debug, Clone)]
struct Column {
    name: String,
    typ: Type,
}

fn parse_table_name_node(node: &rules::table_name<'_>) -> Vec<String> {
    let (first, rest) = node.name();
    let mut names = Vec::with_capacity(rest.len() + 1);
    names.push(parse_name_node(first).0);
    for name in rest {
        names.push(parse_name_node(name).0);
    }
    names
}

fn parse_named_column_node(
    extensions: &SimpleExtensions,
    node: &rules::named_column<'_>,
) -> Result<Column, MessageParseError> {
    Ok(Column {
        name: parse_name_node(node.name()).0,
        typ: parse_type_node(extensions, node.r#type())?,
    })
}

fn parse_named_column_list_node(
    extensions: &SimpleExtensions,
    node: &rules::named_column_list<'_>,
) -> Result<Vec<Column>, MessageParseError> {
    let mut columns = Vec::new();
    if let Some((first, rest)) = node.named_column() {
        columns.push(parse_named_column_node(extensions, first)?);
        for col in rest {
            columns.push(parse_named_column_node(extensions, col)?);
        }
    }
    Ok(columns)
}

#[allow(clippy::vec_box)]
fn expect_one_child(
    message: &'static str,
    span: pest_typed::Span<'_>,
    mut input_children: Vec<Box<Rel>>,
) -> Result<Box<Rel>, MessageParseError> {
    match input_children.len() {
        0 => Err(MessageParseError::invalid(
            message,
            typed_to_pest_span(span),
            format!("{message} missing child"),
        )),
        1 => Ok(input_children.pop().unwrap()),
        n => Err(MessageParseError::invalid(
            message,
            typed_to_pest_span(span),
            format!("{message} should have 1 input child, got {n}"),
        )),
    }
}

fn parse_reference_emit(node: &rules::reference_list<'_>) -> EmitKind {
    let mut output_mapping = Vec::new();
    if let Some((first, rest)) = node.reference() {
        output_mapping.push(parse_field_index_node(first).0);
        for reference in rest {
            output_mapping.push(parse_field_index_node(reference).0);
        }
    }
    EmitKind::Emit(Emit { output_mapping })
}

#[allow(clippy::vec_box)]
fn parse_read_rel_node(
    extensions: &SimpleExtensions,
    node: &rules::read_relation<'_>,
    input_children: Vec<Box<Rel>>,
    input_field_count: usize,
) -> Result<ReadRel, MessageParseError> {
    if !input_children.is_empty() {
        return Err(MessageParseError::invalid(
            "ReadRel",
            typed_to_pest_span(node.span()),
            "ReadRel should have no input children",
        ));
    }
    if input_field_count != 0 {
        return Err(MessageParseError::invalid(
            "ReadRel",
            typed_to_pest_span(node.span()),
            "ReadRel should have 0 input fields",
        ));
    }

    let table = parse_table_name_node(node.table_name());
    let columns = parse_named_column_list_node(extensions, node.named_column_list())?;

    let (names, types): (Vec<_>, Vec<_>) = columns.into_iter().map(|c| (c.name, c.typ)).unzip();
    let struct_ = r#type::Struct {
        types,
        type_variation_reference: 0,
        nullability: r#type::Nullability::Required as i32,
    };

    Ok(ReadRel {
        base_schema: Some(NamedStruct {
            names,
            r#struct: Some(struct_),
        }),
        read_type: Some(read_rel::ReadType::NamedTable(read_rel::NamedTable {
            names: table,
            advanced_extension: None,
        })),
        ..Default::default()
    })
}

#[allow(clippy::vec_box)]
fn parse_filter_rel_node(
    extensions: &SimpleExtensions,
    node: &rules::filter_relation<'_>,
    input_children: Vec<Box<Rel>>,
) -> Result<FilterRel, MessageParseError> {
    let input = expect_one_child("FilterRel", node.span(), input_children)?;
    let condition = parse_expression_node(extensions, node.expression())?;

    let common = RelCommon {
        emit_kind: Some(parse_reference_emit(node.reference_list())),
        ..Default::default()
    };

    Ok(FilterRel {
        input: Some(input),
        condition: Some(Box::new(condition)),
        common: Some(common),
        advanced_extension: None,
    })
}

#[allow(clippy::vec_box)]
fn parse_project_rel_node(
    extensions: &SimpleExtensions,
    node: &rules::project_relation<'_>,
    input_children: Vec<Box<Rel>>,
    input_field_count: usize,
) -> Result<ProjectRel, MessageParseError> {
    let input = expect_one_child("ProjectRel", node.span(), input_children)?;

    let mut expressions = Vec::new();
    let mut output_mapping = Vec::new();

    if let Some((first, rest)) = node.project_argument_list().project_argument() {
        let mut args = Vec::with_capacity(rest.len() + 1);
        args.push(first);
        args.extend(rest);

        for arg in args {
            if let Some(reference) = arg.reference() {
                output_mapping.push(parse_field_index_node(reference).0);
                continue;
            }

            if let Some(expression) = arg.expression() {
                expressions.push(parse_expression_node(extensions, expression)?);
                output_mapping.push(input_field_count as i32 + (expressions.len() as i32 - 1));
                continue;
            }

            unreachable!("project_argument must be reference or expression");
        }
    }

    let common = RelCommon {
        emit_kind: Some(EmitKind::Emit(Emit { output_mapping })),
        ..Default::default()
    };

    Ok(ProjectRel {
        input: Some(input),
        expressions,
        common: Some(common),
        advanced_extension: None,
    })
}

#[allow(clippy::vec_box)]
fn parse_aggregate_rel_node(
    extensions: &SimpleExtensions,
    node: &rules::aggregate_relation<'_>,
    input_children: Vec<Box<Rel>>,
) -> Result<AggregateRel, MessageParseError> {
    let input = expect_one_child("AggregateRel", node.span(), input_children)?;

    let mut grouping_expressions = Vec::new();
    if let Some((first, rest)) = node.aggregate_group_by().reference() {
        let mut refs = Vec::with_capacity(rest.len() + 1);
        refs.push(first);
        refs.extend(rest);
        for reference in refs {
            let field_index = parse_field_index_node(reference);
            grouping_expressions.push(Expression {
                rex_type: Some(substrait::proto::expression::RexType::Selection(Box::new(
                    field_index.to_field_reference(),
                ))),
            });
        }
    }

    let mut measures = Vec::new();
    let mut output_mapping = Vec::new();
    let group_by_count = grouping_expressions.len();
    let mut measure_count = 0;

    if let Some((first, rest)) = node.aggregate_output().aggregate_output_item() {
        let mut items = Vec::with_capacity(rest.len() + 1);
        items.push(first);
        items.extend(rest);

        for output_item in items {
            if let Some(reference) = output_item.reference() {
                output_mapping.push(parse_field_index_node(reference).0);
                continue;
            }
            if let Some(aggregate_measure) = output_item.aggregate_measure() {
                measures.push(parse_measure_node(extensions, aggregate_measure)?);
                output_mapping.push(group_by_count as i32 + measure_count);
                measure_count += 1;
                continue;
            }
            unreachable!("aggregate_output_item must be reference or aggregate_measure");
        }
    }

    let common = RelCommon {
        emit_kind: Some(EmitKind::Emit(Emit { output_mapping })),
        ..Default::default()
    };

    Ok(AggregateRel {
        input: Some(input),
        grouping_expressions,
        groupings: vec![],
        measures,
        common: Some(common),
        advanced_extension: None,
    })
}

fn parse_sort_field_node(
    extensions: &SimpleExtensions,
    node: &rules::sort_field<'_>,
) -> Result<SortField, MessageParseError> {
    let field_index = parse_field_index_node(node.reference());
    let direction = match node.sort_direction().span.as_str().trim_start_matches('&') {
        "AscNullsFirst" => SortDirection::AscNullsFirst,
        "AscNullsLast" => SortDirection::AscNullsLast,
        "DescNullsFirst" => SortDirection::DescNullsFirst,
        "DescNullsLast" => SortDirection::DescNullsLast,
        other => {
            return Err(MessageParseError::invalid(
                "SortDirection",
                typed_to_pest_span(node.sort_direction().span()),
                format!("Unknown sort direction: {other}"),
            ));
        }
    };

    let _ = extensions;
    Ok(SortField {
        expr: Some(Expression {
            rex_type: Some(substrait::proto::expression::RexType::Selection(Box::new(
                field_index.to_field_reference(),
            ))),
        }),
        sort_kind: Some(SortKind::Direction(direction as i32)),
    })
}

#[allow(clippy::vec_box)]
fn parse_sort_rel_node(
    extensions: &SimpleExtensions,
    node: &rules::sort_relation<'_>,
    input_children: Vec<Box<Rel>>,
) -> Result<SortRel, MessageParseError> {
    let input = expect_one_child("SortRel", node.span(), input_children)?;

    let mut sorts = Vec::new();
    if let Some((first, rest)) = node.sort_field_list().sort_field() {
        sorts.push(parse_sort_field_node(extensions, first)?);
        for sort_field in rest {
            sorts.push(parse_sort_field_node(extensions, sort_field)?);
        }
    }

    let common = RelCommon {
        emit_kind: Some(parse_reference_emit(node.reference_list())),
        ..Default::default()
    };

    Ok(SortRel {
        input: Some(input),
        sorts,
        common: Some(common),
        advanced_extension: None,
    })
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

fn parse_count_mode_node(
    extensions: &SimpleExtensions,
    node: &rules::fetch_value<'_>,
) -> Result<CountMode, MessageParseError> {
    if let Some(integer) = node.integer() {
        let value = integer.span.as_str().parse::<i64>().map_err(|e| {
            MessageParseError::invalid(
                "CountMode",
                typed_to_pest_span(integer.span()),
                format!("Invalid integer: {e}"),
            )
        })?;
        if value < 0 {
            return Err(MessageParseError::invalid(
                "CountMode",
                typed_to_pest_span(integer.span()),
                format!("Fetch limit must be non-negative, got: {value}"),
            ));
        }
        return Ok(CountMode::CountExpr(i64_literal_expr(value)));
    }

    if let Some(expression) = node.expression() {
        return Ok(CountMode::CountExpr(Box::new(parse_expression_node(
            extensions, expression,
        )?)));
    }

    Err(MessageParseError::invalid(
        "CountMode",
        typed_to_pest_span(node.span()),
        "Unexpected value for CountMode",
    ))
}

fn parse_offset_mode_node(
    extensions: &SimpleExtensions,
    node: &rules::fetch_value<'_>,
) -> Result<OffsetMode, MessageParseError> {
    if let Some(integer) = node.integer() {
        let value = integer.span.as_str().parse::<i64>().map_err(|e| {
            MessageParseError::invalid(
                "OffsetMode",
                typed_to_pest_span(integer.span()),
                format!("Invalid integer: {e}"),
            )
        })?;
        if value < 0 {
            return Err(MessageParseError::invalid(
                "OffsetMode",
                typed_to_pest_span(integer.span()),
                format!("Fetch offset must be non-negative, got: {value}"),
            ));
        }
        return Ok(OffsetMode::OffsetExpr(i64_literal_expr(value)));
    }

    if let Some(expression) = node.expression() {
        return Ok(OffsetMode::OffsetExpr(Box::new(parse_expression_node(
            extensions, expression,
        )?)));
    }

    Err(MessageParseError::invalid(
        "OffsetMode",
        typed_to_pest_span(node.span()),
        "Unexpected value for OffsetMode",
    ))
}

#[allow(clippy::vec_box)]
fn parse_fetch_rel_node(
    extensions: &SimpleExtensions,
    node: &rules::fetch_relation<'_>,
    input_children: Vec<Box<Rel>>,
) -> Result<FetchRel, MessageParseError> {
    let input = expect_one_child("FetchRel", node.span(), input_children)?;

    let mut named_values: HashMap<String, &rules::fetch_value<'_>> = HashMap::new();
    if let Some(named_arg_list) = node.fetch_args().fetch_named_arg_list() {
        let (first, rest) = named_arg_list.fetch_named_arg();
        let mut args = Vec::with_capacity(rest.len() + 1);
        args.push(first);
        args.extend(rest);

        for arg in args {
            let key = arg.fetch_arg_name().span.as_str().to_string();
            if named_values.contains_key(&key) {
                return Err(MessageParseError::invalid(
                    "NamedArg",
                    typed_to_pest_span(arg.fetch_arg_name().span()),
                    format!("Duplicate argument: {}", arg.fetch_arg_name().span.as_str()),
                ));
            }
            named_values.insert(key, arg.fetch_value());
        }
    }

    let limit_value = named_values.remove("limit");
    let offset_value = named_values.remove("offset");

    if let Some((unknown_name, unknown_value)) = named_values.into_iter().next() {
        return Err(MessageParseError::invalid(
            "NamedArgExtractor",
            typed_to_pest_span(unknown_value.span()),
            format!("Unknown argument: {unknown_name}"),
        ));
    }

    let common = RelCommon {
        emit_kind: Some(parse_reference_emit(node.reference_list())),
        ..Default::default()
    };

    let count_mode = limit_value
        .map(|value| parse_count_mode_node(extensions, value))
        .transpose()?;
    let offset_mode = offset_value
        .map(|value| parse_offset_mode_node(extensions, value))
        .transpose()?;

    Ok(FetchRel {
        input: Some(input),
        common: Some(common),
        advanced_extension: None,
        offset_mode,
        count_mode,
    })
}

fn parse_join_type(node: &rules::join_type<'_>) -> join_rel::JoinType {
    match node.span.as_str().trim_start_matches('&') {
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
        other => panic!("Unknown join type: {other} (this should be caught by grammar)"),
    }
}

#[allow(clippy::vec_box)]
fn parse_join_rel_node(
    extensions: &SimpleExtensions,
    node: &rules::join_relation<'_>,
    input_children: Vec<Box<Rel>>,
) -> Result<JoinRel, MessageParseError> {
    if input_children.len() != 2 {
        return Err(MessageParseError::invalid(
            "JoinRel",
            typed_to_pest_span(node.span()),
            format!(
                "JoinRel should have exactly 2 input children, got {}",
                input_children.len()
            ),
        ));
    }

    let mut children_iter = input_children.into_iter();
    let left = children_iter.next().unwrap();
    let right = children_iter.next().unwrap();

    let common = RelCommon {
        emit_kind: Some(parse_reference_emit(node.reference_list())),
        ..Default::default()
    };

    Ok(JoinRel {
        common: Some(common),
        left: Some(left),
        right: Some(right),
        expression: Some(Box::new(parse_expression_node(
            extensions,
            node.expression(),
        )?)),
        post_join_filter: None,
        r#type: parse_join_type(node.join_type()) as i32,
        advanced_extension: None,
    })
}

/// Parse a non-extension relation. Returns `Ok(None)` if the line is an extension relation.
#[allow(clippy::vec_box)]
pub(crate) fn parse_standard_relation_from_line(
    extensions: &SimpleExtensions,
    line: &str,
    input_children: Vec<Box<Rel>>,
    input_field_count: usize,
) -> Result<Option<Rel>, MessageParseError> {
    let relation = parse_typed::<rules::relation<'_>>(line, "relation")?;

    if let Some(read_relation) = relation.read_relation() {
        return Ok(Some(Rel {
            rel_type: Some(RelType::Read(Box::new(parse_read_rel_node(
                extensions,
                read_relation,
                input_children,
                input_field_count,
            )?))),
        }));
    }

    if let Some(filter_relation) = relation.filter_relation() {
        return Ok(Some(Rel {
            rel_type: Some(RelType::Filter(Box::new(parse_filter_rel_node(
                extensions,
                filter_relation,
                input_children,
            )?))),
        }));
    }

    if let Some(project_relation) = relation.project_relation() {
        return Ok(Some(Rel {
            rel_type: Some(RelType::Project(Box::new(parse_project_rel_node(
                extensions,
                project_relation,
                input_children,
                input_field_count,
            )?))),
        }));
    }

    if let Some(aggregate_relation) = relation.aggregate_relation() {
        return Ok(Some(Rel {
            rel_type: Some(RelType::Aggregate(Box::new(parse_aggregate_rel_node(
                extensions,
                aggregate_relation,
                input_children,
            )?))),
        }));
    }

    if let Some(sort_relation) = relation.sort_relation() {
        return Ok(Some(Rel {
            rel_type: Some(RelType::Sort(Box::new(parse_sort_rel_node(
                extensions,
                sort_relation,
                input_children,
            )?))),
        }));
    }

    if let Some(fetch_relation) = relation.fetch_relation() {
        return Ok(Some(Rel {
            rel_type: Some(RelType::Fetch(Box::new(parse_fetch_rel_node(
                extensions,
                fetch_relation,
                input_children,
            )?))),
        }));
    }

    if let Some(join_relation) = relation.join_relation() {
        return Ok(Some(Rel {
            rel_type: Some(RelType::Join(Box::new(parse_join_rel_node(
                extensions,
                join_relation,
                input_children,
            )?))),
        }));
    }

    if relation.extension_relation().is_some() {
        return Ok(None);
    }

    unreachable!("relation must be a known relation type")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixtures::TestContext;
    use crate::parser::common::parse_typed;

    fn read_rel() -> ReadRel {
        let extensions = SimpleExtensions::default();
        let parsed = parse_typed::<rules::read_relation<'_>>(
            "Read[ab.cd.ef => a:i32, b:string?, c:i64]",
            "read_relation",
        )
        .unwrap();
        parse_read_rel_node(&extensions, &parsed, vec![], 0).unwrap()
    }

    #[test]
    fn test_parse_read_relation() {
        let extensions = SimpleExtensions::default();
        let parsed = parse_typed::<rules::read_relation<'_>>(
            "Read[ab.cd.ef => a:i32, b:string?]",
            "read_relation",
        )
        .unwrap();
        let read = parse_read_rel_node(&extensions, &parsed, vec![], 0).unwrap();

        let names = match &read.read_type {
            Some(read_rel::ReadType::NamedTable(table)) => &table.names,
            _ => panic!("Expected NamedTable"),
        };
        assert_eq!(names, &["ab", "cd", "ef"]);
    }

    #[test]
    fn test_parse_filter_relation() {
        let extensions = SimpleExtensions::default();
        let parsed = parse_typed::<rules::filter_relation<'_>>(
            "Filter[$1 => $0, $1, $2]",
            "filter_relation",
        )
        .unwrap();
        let filter = parse_filter_rel_node(
            &extensions,
            &parsed,
            vec![Box::new(Rel {
                rel_type: Some(RelType::Read(Box::new(read_rel()))),
            })],
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
        let parsed =
            parse_typed::<rules::project_relation<'_>>("Project[$0, $1, 42]", "project_relation")
                .unwrap();
        let project = parse_project_rel_node(
            &extensions,
            &parsed,
            vec![Box::new(Rel {
                rel_type: Some(RelType::Read(Box::new(read_rel()))),
            })],
            3,
        )
        .unwrap();

        assert_eq!(project.expressions.len(), 1);

        let emit_kind = &project.common.as_ref().unwrap().emit_kind.as_ref().unwrap();
        let emit = match emit_kind {
            EmitKind::Emit(emit) => &emit.output_mapping,
            _ => panic!("Expected EmitKind::Emit, got {emit_kind:?}"),
        };
        assert_eq!(emit, &[0, 1, 3]);
    }

    #[test]
    fn test_fetch_relation_positive_values() {
        let extensions = SimpleExtensions::default();
        let parsed = parse_typed::<rules::fetch_relation<'_>>(
            "Fetch[limit=10, offset=5 => $0]",
            "fetch_relation",
        )
        .unwrap();

        let fetch_rel = parse_fetch_rel_node(
            &extensions,
            &parsed,
            vec![Box::new(Rel {
                rel_type: Some(RelType::Read(Box::new(read_rel()))),
            })],
        )
        .unwrap();

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
        let parsed_result =
            parse_typed::<rules::fetch_relation<'_>>("Fetch[limit=-5 => $0]", "fetch_relation");
        if let Ok(parsed) = parsed_result {
            let result = parse_fetch_rel_node(
                &extensions,
                &parsed,
                vec![Box::new(Rel {
                    rel_type: Some(RelType::Read(Box::new(read_rel()))),
                })],
            );
            assert!(result.is_err());
            assert!(
                result
                    .unwrap_err()
                    .to_string()
                    .contains("Fetch limit must be non-negative")
            );
        }
    }

    #[test]
    fn test_fetch_relation_negative_offset_rejected() {
        let extensions = SimpleExtensions::default();
        let parsed_result =
            parse_typed::<rules::fetch_relation<'_>>("Fetch[offset=-10 => $0]", "fetch_relation");
        if let Ok(parsed) = parsed_result {
            let result = parse_fetch_rel_node(
                &extensions,
                &parsed,
                vec![Box::new(Rel {
                    rel_type: Some(RelType::Read(Box::new(read_rel()))),
                })],
            );
            assert!(result.is_err());
            assert!(
                result
                    .unwrap_err()
                    .to_string()
                    .contains("Fetch offset must be non-negative")
            );
        }
    }

    #[test]
    fn test_parse_join_relation_requires_two_children() {
        let extensions = TestContext::new()
            .with_urn(1, "https://github.com/substrait-io/substrait/blob/main/extensions/functions_comparison.yaml")
            .with_function(1, 10, "eq")
            .extensions;

        let parsed = parse_typed::<rules::join_relation<'_>>(
            "Join[&Inner, eq($0, $1) => $0, $1]",
            "join_relation",
        )
        .unwrap();

        let result = parse_join_rel_node(&extensions, &parsed, vec![]);
        assert!(result.is_err());

        let result = parse_join_rel_node(
            &extensions,
            &parsed,
            vec![Box::new(Rel {
                rel_type: Some(RelType::Read(Box::new(read_rel()))),
            })],
        );
        assert!(result.is_err());
    }
}
