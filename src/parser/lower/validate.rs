//! Shared relation-level validation helpers used by lowering.

use std::collections::HashSet;

use substrait::proto::sort_field::SortDirection;
use substrait::proto::{Rel, join_rel};

use super::LowerCtx;
use crate::parser::ast;

pub(crate) fn ensure_no_named_args(
    ctx: &LowerCtx<'_>,
    relation: &ast::Relation,
    message: &'static str,
) -> Result<(), crate::parser::errors::ParseError> {
    if relation.args.named.is_empty() {
        return Ok(());
    }
    ctx.invalid(message, "named arguments are not allowed here")
}

pub(crate) fn ensure_no_children(
    ctx: &LowerCtx<'_>,
    child_count: usize,
    message: &'static str,
) -> Result<(), crate::parser::errors::ParseError> {
    if child_count == 0 {
        return Ok(());
    }
    ctx.invalid(
        message,
        format!("{message} should have no input children, found {child_count}"),
    )
}

pub(crate) fn ensure_exact_child_count(
    ctx: &LowerCtx<'_>,
    child_count: usize,
    expected: usize,
    message: &'static str,
) -> Result<(), crate::parser::errors::ParseError> {
    if child_count == expected {
        return Ok(());
    }

    let suffix = if expected == 1 { "" } else { "ren" };
    ctx.invalid(
        message,
        format!(
            "{message} should have exactly {expected} input child{suffix}, found {child_count}"
        ),
    )
}

#[allow(clippy::vec_box)]
pub(crate) fn expect_one_child(
    ctx: &LowerCtx<'_>,
    child_relations: &mut Vec<Box<Rel>>,
    message: &'static str,
) -> Result<Box<Rel>, crate::parser::errors::ParseError> {
    match child_relations.len() {
        1 => Ok(child_relations.remove(0)),
        n => ctx.invalid(
            message,
            format!("{message} should have exactly one input child, found {n}"),
        ),
    }
}

pub(crate) fn ensure_exact_positional_count(
    ctx: &LowerCtx<'_>,
    positional_count: usize,
    expected: usize,
    message: &'static str,
    detail: &'static str,
) -> Result<(), crate::parser::errors::ParseError> {
    if positional_count == expected {
        return Ok(());
    }
    ctx.invalid(message, detail)
}

pub(crate) fn ensure_unique_named_arg(
    ctx: &LowerCtx<'_>,
    seen: &mut HashSet<String>,
    name: &str,
    message: &'static str,
) -> Result<(), crate::parser::errors::ParseError> {
    if seen.insert(name.to_string()) {
        return Ok(());
    }

    ctx.invalid(message, format!("duplicate named argument '{name}'"))
}

pub(crate) fn output_mapping_from_args(
    ctx: &LowerCtx<'_>,
    args: &[ast::Arg],
    message: &'static str,
) -> Result<Vec<i32>, crate::parser::errors::ParseError> {
    // Output mappings are index-only in the text format and refer to field
    // positions after relation-specific projection/aggregation semantics.
    let mut mapping = Vec::with_capacity(args.len());
    for arg in args {
        match arg {
            ast::Arg::Expr(ast::Expr::FieldRef(index)) => mapping.push(*index),
            _ => {
                return ctx.invalid(
                    message,
                    format!("output mapping expects field references, got '{arg}'"),
                );
            }
        }
    }
    Ok(mapping)
}

pub(crate) fn parse_sort_direction(
    ctx: &LowerCtx<'_>,
    value: &str,
) -> Result<SortDirection, crate::parser::errors::ParseError> {
    let direction = match value {
        "AscNullsFirst" => SortDirection::AscNullsFirst,
        "AscNullsLast" => SortDirection::AscNullsLast,
        "DescNullsFirst" => SortDirection::DescNullsFirst,
        "DescNullsLast" => SortDirection::DescNullsLast,
        _ => {
            return ctx.invalid("Sort", format!("unknown sort direction '&{value}'"));
        }
    };
    Ok(direction)
}

pub(crate) fn parse_join_type(
    ctx: &LowerCtx<'_>,
    value: &str,
) -> Result<join_rel::JoinType, crate::parser::errors::ParseError> {
    let join_type = match value {
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
        _ => {
            return ctx.invalid("Join", format!("unknown join type '&{value}'"));
        }
    };
    Ok(join_type)
}
