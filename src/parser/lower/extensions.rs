//! Extension-relation lowering and AST -> `ExtensionArgs` conversion.

use std::collections::HashSet;

use substrait::proto::rel::RelType;
use substrait::proto::{ExtensionLeafRel, ExtensionMultiRel, ExtensionSingleRel, Rel};

use super::LowerCtx;
use super::validate::ensure_unique_named_arg;
use crate::extensions::any::Any;
use crate::extensions::{
    ExtensionArgs, ExtensionColumn, ExtensionRelationType, ExtensionValue, RawExpression,
};
use crate::parser::ast;
use crate::parser::errors::{MessageParseError, ParseError};
use crate::parser::extensions::resolve_extension_detail;

impl From<ast::ExtensionType> for ExtensionRelationType {
    fn from(value: ast::ExtensionType) -> Self {
        match value {
            ast::ExtensionType::Leaf => ExtensionRelationType::Leaf,
            ast::ExtensionType::Single => ExtensionRelationType::Single,
            ast::ExtensionType::Multi => ExtensionRelationType::Multi,
        }
    }
}

#[allow(clippy::vec_box)]
pub(crate) fn lower_extension(
    ctx: &LowerCtx<'_>,
    relation: &ast::Relation,
    extension_type: ast::ExtensionType,
    extension_name: &str,
    child_relations: Vec<Box<Rel>>,
) -> Result<Rel, ParseError> {
    let relation_type: ExtensionRelationType = extension_type.into();
    let args = build_extension_args_from_relation(ctx, relation, relation_type)?;

    relation_type
        .validate_child_count(child_relations.len())
        .map_err(|e| plan_error(ctx, MessageParseError::invalid("extension_relation", e)))?;

    let detail =
        resolve_extension_detail(ctx.registry, ctx.line_no, ctx.line, extension_name, &args)?;

    Ok(build_extension_relation(
        relation_type,
        detail,
        child_relations,
    ))
}

pub(crate) fn build_extension_args_from_relation(
    ctx: &LowerCtx<'_>,
    relation: &ast::Relation,
    relation_type: ExtensionRelationType,
) -> Result<ExtensionArgs, ParseError> {
    let mut args = ExtensionArgs::new(relation_type);
    let mut seen_named = HashSet::new();

    for arg in &relation.args.positional {
        args.add_positional_arg(extension_value_from_arg(arg).map_err(|e| plan_error(ctx, e))?);
    }

    for named in &relation.args.named {
        ensure_unique_named_arg(ctx, &mut seen_named, &named.name, "extension_relation")?;
        args.add_named_arg(
            named.name.clone(),
            extension_value_from_arg(&named.value).map_err(|e| plan_error(ctx, e))?,
        );
    }

    for arg in &relation.outputs {
        args.add_output_column(extension_column_from_arg(arg).map_err(|e| plan_error(ctx, e))?);
    }

    Ok(args)
}

pub(crate) fn extension_value_from_arg(
    arg: &ast::Arg,
) -> Result<ExtensionValue, MessageParseError> {
    match arg {
        ast::Arg::Expr(ast::Expr::FieldRef(idx)) => Ok(ExtensionValue::Reference(*idx)),
        ast::Arg::Expr(ast::Expr::Literal(lit)) => match (&lit.value, &lit.typ) {
            (ast::LiteralValue::String(value), None) => Ok(ExtensionValue::String(value.clone())),
            (ast::LiteralValue::Integer(value), None) => Ok(ExtensionValue::Integer(*value)),
            (ast::LiteralValue::Float(value), None) => Ok(ExtensionValue::Float(*value)),
            (ast::LiteralValue::Boolean(value), None) => Ok(ExtensionValue::Boolean(*value)),
            _ => Ok(ExtensionValue::Expression(RawExpression::new(
                lit.to_string(),
            ))),
        },
        ast::Arg::Expr(expr) => Ok(ExtensionValue::Expression(RawExpression::new(
            expr.to_string(),
        ))),
        ast::Arg::NamedColumn(_, _)
        | ast::Arg::Enum(_)
        | ast::Arg::Tuple(_)
        | ast::Arg::Wildcard => Ok(ExtensionValue::Expression(RawExpression::new(
            arg.to_string(),
        ))),
    }
}

pub(crate) fn extension_column_from_arg(
    arg: &ast::Arg,
) -> Result<ExtensionColumn, MessageParseError> {
    match arg {
        ast::Arg::NamedColumn(name, typ) => Ok(ExtensionColumn::Named {
            name: name.clone(),
            type_spec: typ.to_string(),
        }),
        ast::Arg::Expr(ast::Expr::FieldRef(idx)) => Ok(ExtensionColumn::Reference(*idx)),
        ast::Arg::Expr(expr) => Ok(ExtensionColumn::Expression(RawExpression::new(
            expr.to_string(),
        ))),
        ast::Arg::Enum(_) | ast::Arg::Tuple(_) | ast::Arg::Wildcard => Ok(
            ExtensionColumn::Expression(RawExpression::new(arg.to_string())),
        ),
    }
}

#[allow(clippy::vec_box)]
fn build_extension_relation(
    relation_type: ExtensionRelationType,
    detail: Option<Any>,
    children: Vec<Box<Rel>>,
) -> Rel {
    debug_assert!(
        relation_type.validate_child_count(children.len()).is_ok(),
        "child count should be validated before relation construction"
    );

    let rel_type = match relation_type {
        ExtensionRelationType::Leaf => RelType::ExtensionLeaf(ExtensionLeafRel {
            common: None,
            detail: detail.map(Into::into),
        }),
        ExtensionRelationType::Single => {
            let input = children.into_iter().next().map(|child| *child);
            RelType::ExtensionSingle(Box::new(ExtensionSingleRel {
                common: None,
                detail: detail.map(Into::into),
                input: input.map(Box::new),
            }))
        }
        ExtensionRelationType::Multi => {
            let inputs = children.into_iter().map(|child| *child).collect();
            RelType::ExtensionMulti(ExtensionMultiRel {
                common: None,
                detail: detail.map(Into::into),
                inputs,
            })
        }
    };

    Rel {
        rel_type: Some(rel_type),
    }
}

fn plan_error(ctx: &LowerCtx<'_>, error: MessageParseError) -> ParseError {
    ParseError::Plan(ctx.parse_context(), error)
}
