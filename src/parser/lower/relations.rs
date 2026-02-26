//! Relation-specific lowering logic.

use substrait::proto::aggregate_rel::Measure;
use substrait::proto::fetch_rel::{CountMode, OffsetMode};
use substrait::proto::rel::RelType;
use substrait::proto::rel_common::{Emit, EmitKind};
use substrait::proto::sort_field::SortKind;
use substrait::proto::r#type::{self as ptype, Nullability};
use substrait::proto::{
    AggregateFunction, AggregateRel, FetchRel, FilterRel, JoinRel, NamedStruct, ProjectRel,
    ReadRel, Rel, RelCommon, SortField, SortRel, read_rel,
};

use super::expr::{fetch_value_expression, field_ref_expression, lower_arg_as_expression};
use super::validate::{
    ensure_exact_child_count, ensure_exact_positional_count, ensure_no_children,
    ensure_no_named_args, expect_one_child, output_mapping_from_args, parse_join_type,
    parse_sort_direction,
};
use super::{Lower, LowerCtx};
use crate::parser::ast;
use crate::parser::errors::ParseError;

#[allow(clippy::vec_box)]
pub(crate) fn lower_standard(
    ctx: &LowerCtx<'_>,
    relation: &ast::Relation,
    name: &str,
    child_relations: Vec<Box<Rel>>,
    input_field_count: usize,
) -> Result<Rel, ParseError> {
    match name {
        "Read" => lower_read(ctx, relation, child_relations),
        "Filter" => lower_filter(ctx, relation, child_relations),
        "Project" => lower_project(ctx, relation, child_relations, input_field_count),
        "Aggregate" => lower_aggregate(ctx, relation, child_relations),
        "Sort" => lower_sort(ctx, relation, child_relations),
        "Fetch" => lower_fetch(ctx, relation, child_relations),
        "Join" => lower_join(ctx, relation, child_relations),
        _ => Err(ParseError::RelationParse(
            ctx.parse_context(),
            format!("unsupported relation '{name}'"),
        )),
    }
}

#[allow(clippy::vec_box)]
fn lower_read(
    ctx: &LowerCtx<'_>,
    relation: &ast::Relation,
    child_relations: Vec<Box<Rel>>,
) -> Result<Rel, ParseError> {
    ensure_no_named_args(ctx, relation, "Read")?;
    ensure_no_children(ctx, child_relations.len(), "Read")?;

    ensure_exact_positional_count(
        ctx,
        relation.args.positional.len(),
        1,
        "Read",
        "Read expects exactly one positional argument (table name)",
    )?;

    let table_name = match &relation.args.positional[0] {
        ast::Arg::Expr(ast::Expr::Identifier(name)) => name,
        arg => {
            return ctx.invalid(
                "Read",
                format!("Read table name must be an identifier, got '{arg}'"),
            );
        }
    };

    let names = table_name
        .split('.')
        .map(ToString::to_string)
        .collect::<Vec<_>>();

    let mut column_names = Vec::with_capacity(relation.outputs.len());
    let mut column_types = Vec::with_capacity(relation.outputs.len());
    for col in &relation.outputs {
        match col {
            ast::Arg::NamedColumn(name, typ) => {
                column_names.push(name.clone());
                column_types.push(typ.lower(ctx, "Read")?);
            }
            _ => {
                return ctx.invalid("Read", "Read outputs must be named columns, e.g. col:i32");
            }
        }
    }

    let struct_ = ptype::Struct {
        types: column_types,
        type_variation_reference: 0,
        nullability: Nullability::Required as i32,
    };

    let read = ReadRel {
        base_schema: Some(NamedStruct {
            names: column_names,
            r#struct: Some(struct_),
        }),
        read_type: Some(read_rel::ReadType::NamedTable(read_rel::NamedTable {
            names,
            advanced_extension: None,
        })),
        ..Default::default()
    };

    Ok(Rel {
        rel_type: Some(RelType::Read(Box::new(read))),
    })
}

#[allow(clippy::vec_box)]
fn lower_filter(
    ctx: &LowerCtx<'_>,
    relation: &ast::Relation,
    mut child_relations: Vec<Box<Rel>>,
) -> Result<Rel, ParseError> {
    ensure_no_named_args(ctx, relation, "Filter")?;
    let input = expect_one_child(ctx, &mut child_relations, "Filter")?;

    ensure_exact_positional_count(
        ctx,
        relation.args.positional.len(),
        1,
        "Filter",
        "Filter expects one positional condition argument",
    )?;

    let condition = lower_arg_as_expression(ctx, &relation.args.positional[0], "Filter")?;

    let emit = EmitKind::Emit(Emit {
        output_mapping: output_mapping_from_args(ctx, &relation.outputs, "Filter")?,
    });

    Ok(Rel {
        rel_type: Some(RelType::Filter(Box::new(FilterRel {
            input: Some(input),
            condition: Some(Box::new(condition)),
            common: Some(RelCommon {
                emit_kind: Some(emit),
                ..Default::default()
            }),
            advanced_extension: None,
        }))),
    })
}

#[allow(clippy::vec_box)]
fn lower_project(
    ctx: &LowerCtx<'_>,
    relation: &ast::Relation,
    mut child_relations: Vec<Box<Rel>>,
    input_field_count: usize,
) -> Result<Rel, ParseError> {
    ensure_no_named_args(ctx, relation, "Project")?;
    let input = expect_one_child(ctx, &mut child_relations, "Project")?;

    if !relation.outputs.is_empty() {
        return ctx.invalid(
            "Project",
            "Project does not accept output columns after '=>'; use positional arguments only",
        );
    }

    let mut expressions = Vec::new();
    let mut output_mapping = Vec::new();

    for arg in &relation.args.positional {
        match arg {
            ast::Arg::Expr(ast::Expr::FieldRef(index)) => output_mapping.push(*index),
            _ => {
                let expr = lower_arg_as_expression(ctx, arg, "Project")?;
                expressions.push(expr);
                // Computed expressions are appended after input fields in the
                // projected schema.
                output_mapping.push(input_field_count as i32 + (expressions.len() as i32 - 1));
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

#[allow(clippy::vec_box)]
fn lower_aggregate(
    ctx: &LowerCtx<'_>,
    relation: &ast::Relation,
    mut child_relations: Vec<Box<Rel>>,
) -> Result<Rel, ParseError> {
    ensure_no_named_args(ctx, relation, "Aggregate")?;
    let input = expect_one_child(ctx, &mut child_relations, "Aggregate")?;

    let mut grouping_expressions = Vec::new();
    for arg in &relation.args.positional {
        match arg {
            ast::Arg::Wildcard => {}
            ast::Arg::Expr(ast::Expr::FieldRef(index)) => {
                grouping_expressions.push(field_ref_expression(*index));
            }
            _ => {
                return ctx.invalid(
                    "Aggregate",
                    format!(
                        "Aggregate grouping argument must be field reference or '_', got '{arg}'"
                    ),
                );
            }
        }
    }

    let group_by_count = grouping_expressions.len();
    let mut measures = Vec::new();
    let mut output_mapping = Vec::new();

    for arg in &relation.outputs {
        match arg {
            ast::Arg::Expr(ast::Expr::FieldRef(index)) => output_mapping.push(*index),
            ast::Arg::Expr(ast::Expr::FunctionCall(call)) => {
                let scalar = call.lower(ctx, "Aggregate")?;
                measures.push(Measure {
                    measure: Some(AggregateFunction {
                        function_reference: scalar.function_reference,
                        arguments: scalar.arguments,
                        options: scalar.options,
                        output_type: scalar.output_type,
                        invocation: 0,
                        phase: 0,
                        sorts: vec![],
                        #[allow(deprecated)]
                        args: scalar.args,
                    }),
                    filter: None,
                });
                // Aggregate outputs are grouped fields followed by measures.
                output_mapping.push(group_by_count as i32 + (measures.len() as i32 - 1));
            }
            _ => {
                return ctx.invalid(
                    "Aggregate",
                    format!("Aggregate output must be field reference or aggregate function, got '{arg}'"),
                );
            }
        }
    }

    Ok(Rel {
        rel_type: Some(RelType::Aggregate(Box::new(AggregateRel {
            input: Some(input),
            grouping_expressions,
            // We use `grouping_expressions` (newer API) and leave legacy
            // `groupings` empty for compatibility with current text format.
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

#[allow(clippy::vec_box)]
fn lower_sort(
    ctx: &LowerCtx<'_>,
    relation: &ast::Relation,
    mut child_relations: Vec<Box<Rel>>,
) -> Result<Rel, ParseError> {
    ensure_no_named_args(ctx, relation, "Sort")?;
    let input = expect_one_child(ctx, &mut child_relations, "Sort")?;

    let mut sorts = Vec::with_capacity(relation.args.positional.len());
    for arg in &relation.args.positional {
        let ast::Arg::Tuple(values) = arg else {
            return ctx.invalid(
                "Sort",
                "Sort fields must be tuples like ($0, &AscNullsFirst)",
            );
        };
        if values.len() != 2 {
            return ctx.invalid("Sort", "Sort tuple must contain exactly 2 items");
        }

        let field = match &values[0] {
            ast::Arg::Expr(ast::Expr::FieldRef(idx)) => *idx,
            _ => {
                return ctx.invalid("Sort", "Sort tuple first item must be a field reference");
            }
        };

        let direction = match &values[1] {
            ast::Arg::Enum(v) => parse_sort_direction(ctx, v)?,
            _ => {
                return ctx.invalid(
                    "Sort",
                    "Sort tuple second item must be a sort direction enum",
                );
            }
        };

        sorts.push(SortField {
            expr: Some(field_ref_expression(field)),
            sort_kind: Some(SortKind::Direction(direction as i32)),
        });
    }

    let output_mapping = output_mapping_from_args(ctx, &relation.outputs, "Sort")?;

    Ok(Rel {
        rel_type: Some(RelType::Sort(Box::new(SortRel {
            input: Some(input),
            sorts,
            common: Some(RelCommon {
                emit_kind: Some(EmitKind::Emit(Emit { output_mapping })),
                ..Default::default()
            }),
            advanced_extension: None,
        }))),
    })
}

#[allow(clippy::vec_box)]
fn lower_fetch(
    ctx: &LowerCtx<'_>,
    relation: &ast::Relation,
    mut child_relations: Vec<Box<Rel>>,
) -> Result<Rel, ParseError> {
    let input = expect_one_child(ctx, &mut child_relations, "Fetch")?;

    let mut count_mode = None;
    let mut offset_mode = None;

    if relation.args.positional.len() == 1
        && matches!(relation.args.positional[0], ast::Arg::Wildcard)
    {
        if !relation.args.named.is_empty() {
            return ctx.invalid("Fetch", "Fetch cannot mix '_' with named arguments");
        }
    } else {
        if !relation.args.positional.is_empty() {
            return ctx.invalid(
                "Fetch",
                "Fetch accepts only named arguments (limit/offset) or '_'",
            );
        }

        for named in &relation.args.named {
            match named.name.as_str() {
                "limit" => {
                    if count_mode.is_some() {
                        return ctx.invalid("Fetch", "Duplicate limit argument");
                    }
                    count_mode = Some(CountMode::CountExpr(Box::new(fetch_value_expression(
                        ctx,
                        &named.value,
                        "limit",
                    )?)));
                }
                "offset" => {
                    if offset_mode.is_some() {
                        return ctx.invalid("Fetch", "Duplicate offset argument");
                    }
                    offset_mode = Some(OffsetMode::OffsetExpr(Box::new(fetch_value_expression(
                        ctx,
                        &named.value,
                        "offset",
                    )?)));
                }
                other => {
                    return ctx.invalid("Fetch", format!("Unknown named argument '{other}'"));
                }
            }
        }
    }

    let output_mapping = output_mapping_from_args(ctx, &relation.outputs, "Fetch")?;

    Ok(Rel {
        rel_type: Some(RelType::Fetch(Box::new(FetchRel {
            input: Some(input),
            common: Some(RelCommon {
                emit_kind: Some(EmitKind::Emit(Emit { output_mapping })),
                ..Default::default()
            }),
            advanced_extension: None,
            offset_mode,
            count_mode,
        }))),
    })
}

#[allow(clippy::vec_box)]
fn lower_join(
    ctx: &LowerCtx<'_>,
    relation: &ast::Relation,
    mut child_relations: Vec<Box<Rel>>,
) -> Result<Rel, ParseError> {
    ensure_no_named_args(ctx, relation, "Join")?;

    ensure_exact_child_count(ctx, child_relations.len(), 2, "Join")?;

    ensure_exact_positional_count(
        ctx,
        relation.args.positional.len(),
        2,
        "Join",
        "Join expects two positional args: join type and condition",
    )?;

    let join_type = match &relation.args.positional[0] {
        ast::Arg::Enum(value) => parse_join_type(ctx, value)?,
        _ => {
            return ctx.invalid("Join", "Join first argument must be enum (e.g. &Inner)");
        }
    };

    let condition = lower_arg_as_expression(ctx, &relation.args.positional[1], "Join")?;

    // Structural parsing preserves child order: first child is left input, second is right.
    let left = child_relations.remove(0);
    let right = child_relations.remove(0);

    let output_mapping = output_mapping_from_args(ctx, &relation.outputs, "Join")?;

    Ok(Rel {
        rel_type: Some(RelType::Join(Box::new(JoinRel {
            common: Some(RelCommon {
                emit_kind: Some(EmitKind::Emit(Emit { output_mapping })),
                ..Default::default()
            }),
            left: Some(left),
            right: Some(right),
            expression: Some(Box::new(condition)),
            post_join_filter: None,
            r#type: join_type as i32,
            advanced_extension: None,
        }))),
    })
}
