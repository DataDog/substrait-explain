//! Type lowering and extension-anchor resolution.

use substrait::proto::Type;
use substrait::proto::r#type::{self as ptype, Kind, Nullability, Parameter};

use super::{Lower, LowerCtx};
use crate::extensions::simple::ExtensionKind;
use crate::parser::ast;
use crate::parser::errors::{ErrorKind, MessageParseError, ParseError};

impl Lower for ast::TypeExpr {
    type Output = Type;

    fn lower(&self, ctx: &LowerCtx<'_>, message: &'static str) -> Result<Self::Output, ParseError> {
        Ok(Type {
            kind: Some(lower_type_kind(ctx, self, message)?),
        })
    }
}

pub(crate) fn lower_type_kind(
    ctx: &LowerCtx<'_>,
    typ: &ast::TypeExpr,
    message: &'static str,
) -> Result<Kind, ParseError> {
    match typ {
        ast::TypeExpr::Simple { name, nullability } => {
            lower_simple_type(ctx, name, *nullability, message)
        }
        ast::TypeExpr::List { nullability, inner } => Ok(Kind::List(Box::new(ptype::List {
            nullability: to_nullability(*nullability) as i32,
            r#type: Some(Box::new(inner.lower(ctx, message)?)),
            type_variation_reference: 0,
        }))),
        ast::TypeExpr::UserDefined {
            name,
            anchor,
            urn_anchor: _,
            nullability,
            parameters,
        } => {
            let type_reference = resolve_anchor(ctx, ExtensionKind::Type, *anchor, name, message)?;
            let mut type_parameters = Vec::with_capacity(parameters.len());
            for parameter in parameters {
                type_parameters.push(Parameter {
                    parameter: Some(ptype::parameter::Parameter::DataType(
                        parameter.lower(ctx, message)?,
                    )),
                });
            }
            Ok(Kind::UserDefined(ptype::UserDefined {
                type_reference,
                nullability: to_nullability(*nullability) as i32,
                type_parameters,
                type_variation_reference: 0,
            }))
        }
    }
}

pub(crate) fn lower_type_kind_no_box(
    ctx: &LowerCtx<'_>,
    typ: &ast::TypeExpr,
    message: &'static str,
) -> Result<Kind, ParseError> {
    // Type annotations in literals currently only support simple types.
    match typ {
        ast::TypeExpr::Simple { name, nullability } => {
            lower_simple_type(ctx, name, *nullability, message)
        }
        _ => ctx.invalid(
            message,
            format!("literal type annotation must be simple type, got '{typ}'"),
        ),
    }
}

pub(crate) fn lower_simple_type(
    ctx: &LowerCtx<'_>,
    name: &str,
    nullability: ast::Nullability,
    message: &'static str,
) -> Result<Kind, ParseError> {
    let nullability = to_nullability(nullability) as i32;
    let kind = match name {
        "boolean" => Kind::Bool(ptype::Boolean {
            nullability,
            type_variation_reference: 0,
        }),
        "i8" => Kind::I8(ptype::I8 {
            nullability,
            type_variation_reference: 0,
        }),
        "i16" => Kind::I16(ptype::I16 {
            nullability,
            type_variation_reference: 0,
        }),
        "i32" => Kind::I32(ptype::I32 {
            nullability,
            type_variation_reference: 0,
        }),
        "i64" => Kind::I64(ptype::I64 {
            nullability,
            type_variation_reference: 0,
        }),
        "fp32" => Kind::Fp32(ptype::Fp32 {
            nullability,
            type_variation_reference: 0,
        }),
        "fp64" => Kind::Fp64(ptype::Fp64 {
            nullability,
            type_variation_reference: 0,
        }),
        "string" => Kind::String(ptype::String {
            nullability,
            type_variation_reference: 0,
        }),
        "binary" => Kind::Binary(ptype::Binary {
            nullability,
            type_variation_reference: 0,
        }),
        #[allow(deprecated)]
        "timestamp" => Kind::Timestamp(ptype::Timestamp {
            nullability,
            type_variation_reference: 0,
        }),
        #[allow(deprecated)]
        "timestamp_tz" => Kind::TimestampTz(ptype::TimestampTz {
            nullability,
            type_variation_reference: 0,
        }),
        "date" => Kind::Date(ptype::Date {
            nullability,
            type_variation_reference: 0,
        }),
        "time" => Kind::Time(ptype::Time {
            nullability,
            type_variation_reference: 0,
        }),
        "interval_year" => Kind::IntervalYear(ptype::IntervalYear {
            nullability,
            type_variation_reference: 0,
        }),
        "uuid" => Kind::Uuid(ptype::Uuid {
            nullability,
            type_variation_reference: 0,
        }),
        // Unsupported parity-first simple types; keep explicit validation error.
        "u8" | "u16" | "u32" | "u64" => {
            return ctx.invalid(message, format!("unsupported type '{name}'"));
        }
        _ => {
            return ctx.invalid(message, format!("unknown type '{name}'"));
        }
    };
    Ok(kind)
}

pub(crate) fn resolve_anchor(
    ctx: &LowerCtx<'_>,
    kind: ExtensionKind,
    anchor: Option<u32>,
    name: &str,
    message: &'static str,
) -> Result<u32, ParseError> {
    let resolved = match anchor {
        // When an explicit anchor is present, validate that it matches the
        // requested name and extension kind.
        Some(anchor) => ctx
            .extensions
            .is_name_unique(kind, anchor, name)
            .map(|_| anchor),
        None => ctx.extensions.find_by_name(kind, name),
    };

    resolved.map_err(|missing| {
        ParseError::Plan(
            ctx.parse_context(),
            MessageParseError::new(
                message,
                ErrorKind::Lookup(missing),
                format!("unable to resolve {kind} '{name}'"),
            ),
        )
    })
}

pub(crate) fn to_nullability(n: ast::Nullability) -> Nullability {
    match n {
        ast::Nullability::Required => Nullability::Required,
        ast::Nullability::Nullable => Nullability::Nullable,
        ast::Nullability::Unspecified => Nullability::Unspecified,
    }
}
