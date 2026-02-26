//! Literal lowering and typed literal coercion.

use chrono::{DateTime, NaiveDate, NaiveDateTime, NaiveTime};
use substrait::proto::expression::Literal;
use substrait::proto::expression::literal::LiteralType;
use substrait::proto::r#type::{self as ptype, Kind, Nullability};

use super::types::lower_type_kind_no_box;
use super::{Lower, LowerCtx};
use crate::parser::ast;
use crate::parser::errors::ParseError;

impl Lower for ast::Literal {
    type Output = Literal;

    fn lower(&self, ctx: &LowerCtx<'_>, message: &'static str) -> Result<Self::Output, ParseError> {
        match (&self.value, &self.typ) {
            (ast::LiteralValue::Integer(value), typ) => {
                lower_int_literal(ctx, *value, typ.as_ref(), message)
            }
            (ast::LiteralValue::Float(value), typ) => {
                lower_float_literal(ctx, *value, typ.as_ref(), message)
            }
            (ast::LiteralValue::Boolean(value), _typ) => Ok(Literal {
                literal_type: Some(LiteralType::Boolean(*value)),
                nullable: false,
                type_variation_reference: 0,
            }),
            (ast::LiteralValue::String(value), typ) => {
                lower_string_literal(ctx, value, typ.as_ref(), message)
            }
        }
    }
}

fn lower_int_literal(
    ctx: &LowerCtx<'_>,
    parsed_value: i64,
    typ: Option<&ast::TypeExpr>,
    message: &'static str,
) -> Result<Literal, ParseError> {
    let kind = match typ {
        None => Kind::I64(ptype::I64 {
            type_variation_reference: 0,
            nullability: Nullability::Required as i32,
        }),
        Some(typ) => lower_type_kind_no_box(ctx, typ, message)?,
    };

    let (lit, nullability, tvar) = match kind {
        // TODO(parser-hardening): validate numeric bounds before narrowing.
        // Values like `1000:i8` currently cast via `as` and silently truncate.
        Kind::I8(i) => (
            LiteralType::I8(parsed_value as i32),
            i.nullability,
            i.type_variation_reference,
        ),
        Kind::I16(i) => (
            LiteralType::I16(parsed_value as i32),
            i.nullability,
            i.type_variation_reference,
        ),
        Kind::I32(i) => (
            LiteralType::I32(parsed_value as i32),
            i.nullability,
            i.type_variation_reference,
        ),
        Kind::I64(i) => (
            LiteralType::I64(parsed_value),
            i.nullability,
            i.type_variation_reference,
        ),
        other => {
            return ctx.invalid(
                message,
                format!("Invalid type for integer literal: {other:?}"),
            );
        }
    };

    Ok(Literal {
        literal_type: Some(lit),
        nullable: nullability != Nullability::Required as i32,
        type_variation_reference: tvar,
    })
}

fn lower_float_literal(
    ctx: &LowerCtx<'_>,
    parsed_value: f64,
    typ: Option<&ast::TypeExpr>,
    message: &'static str,
) -> Result<Literal, ParseError> {
    let kind = match typ {
        None => Kind::Fp64(ptype::Fp64 {
            type_variation_reference: 0,
            nullability: Nullability::Required as i32,
        }),
        Some(typ) => lower_type_kind_no_box(ctx, typ, message)?,
    };

    let (lit, nullability, tvar) = match kind {
        Kind::Fp32(f) => (
            LiteralType::Fp32(parsed_value as f32),
            f.nullability,
            f.type_variation_reference,
        ),
        Kind::Fp64(f) => (
            LiteralType::Fp64(parsed_value),
            f.nullability,
            f.type_variation_reference,
        ),
        other => {
            return ctx.invalid(
                message,
                format!("Invalid type for float literal: {other:?}"),
            );
        }
    };

    Ok(Literal {
        literal_type: Some(lit),
        nullable: nullability != Nullability::Required as i32,
        type_variation_reference: tvar,
    })
}

fn lower_string_literal(
    ctx: &LowerCtx<'_>,
    string_value: &str,
    typ: Option<&ast::TypeExpr>,
    message: &'static str,
) -> Result<Literal, ParseError> {
    let Some(typ) = typ else {
        return Ok(Literal {
            literal_type: Some(LiteralType::String(string_value.to_string())),
            nullable: false,
            type_variation_reference: 0,
        });
    };

    let kind = lower_type_kind_no_box(ctx, typ, message)?;
    match kind {
        Kind::String(s) => Ok(Literal {
            literal_type: Some(LiteralType::String(string_value.to_string())),
            nullable: s.nullability != Nullability::Required as i32,
            type_variation_reference: s.type_variation_reference,
        }),
        Kind::Date(d) => {
            let date_days = parse_date_to_days(ctx, string_value, message)?;
            Ok(Literal {
                literal_type: Some(LiteralType::Date(date_days)),
                nullable: d.nullability != Nullability::Required as i32,
                type_variation_reference: d.type_variation_reference,
            })
        }
        Kind::Time(t) => {
            let micros = parse_time_to_microseconds(ctx, string_value, message)?;
            Ok(Literal {
                literal_type: Some(LiteralType::Time(micros)),
                nullable: t.nullability != Nullability::Required as i32,
                type_variation_reference: t.type_variation_reference,
            })
        }
        #[allow(deprecated)]
        Kind::Timestamp(ts) => {
            let micros = parse_timestamp_to_microseconds(ctx, string_value, message)?;
            Ok(Literal {
                literal_type: Some(LiteralType::Timestamp(micros)),
                nullable: ts.nullability != Nullability::Required as i32,
                type_variation_reference: ts.type_variation_reference,
            })
        }
        other => ctx.invalid(
            message,
            format!("Invalid type for string literal: {other:?}"),
        ),
    }
}

fn parse_date_to_days(
    ctx: &LowerCtx<'_>,
    date_str: &str,
    message: &'static str,
) -> Result<i32, ParseError> {
    let formats = ["%Y-%m-%d", "%Y/%m/%d"];

    for format in &formats {
        if let Ok(date) = NaiveDate::parse_from_str(date_str, format) {
            // Constant date; construction cannot fail.
            let epoch = NaiveDate::from_ymd_opt(1970, 1, 1).expect("epoch date");
            return Ok(date.signed_duration_since(epoch).num_days() as i32);
        }
    }

    ctx.invalid(
        message,
        format!("Invalid date format: '{date_str}'. Expected YYYY-MM-DD or YYYY/MM/DD"),
    )
}

fn parse_time_to_microseconds(
    ctx: &LowerCtx<'_>,
    time_str: &str,
    message: &'static str,
) -> Result<i64, ParseError> {
    let formats = ["%H:%M:%S%.f", "%H:%M:%S"];

    for format in &formats {
        if let Ok(time) = NaiveTime::parse_from_str(time_str, format) {
            // Constant time; construction cannot fail.
            let midnight = NaiveTime::from_hms_opt(0, 0, 0).expect("midnight");
            let duration = time.signed_duration_since(midnight);
            return Ok(duration.num_microseconds().unwrap_or(0));
        }
    }

    ctx.invalid(
        message,
        format!("Invalid time format: '{time_str}'. Expected HH:MM:SS or HH:MM:SS.fff"),
    )
}

fn parse_timestamp_to_microseconds(
    ctx: &LowerCtx<'_>,
    timestamp_str: &str,
    message: &'static str,
) -> Result<i64, ParseError> {
    let formats = [
        "%Y-%m-%dT%H:%M:%S%.f",
        "%Y-%m-%dT%H:%M:%S",
        "%Y-%m-%d %H:%M:%S%.f",
        "%Y-%m-%d %H:%M:%S",
        "%Y/%m/%dT%H:%M:%S%.f",
        "%Y/%m/%dT%H:%M:%S",
        "%Y/%m/%d %H:%M:%S%.f",
        "%Y/%m/%d %H:%M:%S",
    ];

    for format in &formats {
        if let Ok(datetime) = NaiveDateTime::parse_from_str(timestamp_str, format) {
            // Constant timestamp; construction cannot fail.
            let epoch = DateTime::from_timestamp(0, 0)
                .expect("epoch timestamp")
                .naive_utc();
            let duration = datetime.signed_duration_since(epoch);
            return Ok(duration.num_microseconds().unwrap_or(0));
        }
    }

    ctx.invalid(
        message,
        format!(
            "Invalid timestamp format: '{timestamp_str}'. Expected YYYY-MM-DDTHH:MM:SS or YYYY-MM-DD HH:MM:SS"
        ),
    )
}
