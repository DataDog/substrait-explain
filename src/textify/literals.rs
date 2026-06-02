use std::fmt;

use chrono::{DateTime, NaiveDate};
use substrait::proto::expression::literal::LiteralType;
use substrait::proto::r#type::{self as ptype, Kind, Nullability};
use substrait::proto::{Type, expression as expr};

use super::{PlanError, Scope, Textify, Visibility};
use crate::textify::types::escaped;

pub fn textify_binary<S: Scope, W: fmt::Write>(items: &[u8], ctx: &S, w: &mut W) -> fmt::Result {
    if ctx.options().show_literal_binaries {
        write!(w, "0x")?;
        for &n in items {
            write!(w, "{n:02x}")?;
        }
    } else {
        write!(w, "{{binary}}")?;
    }
    Ok(())
}

fn unimplemented_literal<S: Scope, W: fmt::Write>(
    variant: &'static str,
    ctx: &S,
    w: &mut W,
) -> fmt::Result {
    write!(
        w,
        "{}",
        ctx.failure(PlanError::unimplemented(
            "LiteralType",
            Some(variant),
            format!("{variant} literal textification not implemented"),
        ))
    )
}

fn days_to_date_string(days: i32) -> String {
    let epoch = NaiveDate::from_ymd_opt(1970, 1, 1).unwrap();
    let date = epoch + chrono::Duration::days(days as i64);
    date.format("%Y-%m-%d").to_string()
}

fn format_fraction(units: i64, precision: i32) -> String {
    if precision == 0 || units == 0 {
        return String::new();
    }
    let mut fraction = format!("{:0width$}", units, width = precision as usize);
    while fraction.ends_with('0') {
        fraction.pop();
    }
    if fraction.is_empty() {
        String::new()
    } else {
        format!(".{fraction}")
    }
}

fn precision_scale(precision: i32) -> Option<i64> {
    (0..=12)
        .contains(&precision)
        .then(|| 10_i64.pow(precision as u32))
}

fn precision_time_to_string(value: i64, precision: i32) -> Option<String> {
    let scale = precision_scale(precision)?;
    let total_seconds = value.div_euclid(scale);
    let fraction_units = value.rem_euclid(scale);
    let hours = total_seconds / 3600;
    let minutes = (total_seconds % 3600) / 60;
    let seconds = total_seconds % 60;
    Some(format!(
        "{hours:02}:{minutes:02}:{seconds:02}{}",
        format_fraction(fraction_units, precision)
    ))
}

fn microseconds_to_time_string(microseconds: i64) -> String {
    precision_time_to_string(microseconds, 6).expect("precision 6 is valid")
}

fn microseconds_to_timestamp_string(microseconds: i64) -> String {
    let epoch = DateTime::from_timestamp(0, 0).unwrap().naive_utc();
    let duration = chrono::Duration::microseconds(microseconds);
    let datetime = epoch + duration;
    let formatted = datetime.format("%Y-%m-%dT%H:%M:%S%.f").to_string();
    if formatted.contains('.') {
        formatted
            .trim_end_matches('0')
            .trim_end_matches('.')
            .to_string()
    } else {
        formatted
    }
}

fn precision_timestamp_to_string(value: i64, precision: i32) -> Option<String> {
    let scale = precision_scale(precision)?;
    let seconds = value.div_euclid(scale);
    let fraction_units = value.rem_euclid(scale);
    let datetime = DateTime::from_timestamp(seconds, 0)?.naive_utc();
    Some(format!(
        "{}{}",
        datetime.format("%Y-%m-%dT%H:%M:%S"),
        format_fraction(fraction_units, precision)
    ))
}

fn write_literal_value<S: Scope, W: fmt::Write>(
    lit: &LiteralType,
    ctx: &S,
    w: &mut W,
) -> fmt::Result {
    match lit {
        LiteralType::Boolean(b) => write!(w, "{b}"),
        LiteralType::I8(i) | LiteralType::I16(i) | LiteralType::I32(i) => write!(w, "{i}"),
        LiteralType::I64(i) => write!(w, "{i}"),
        LiteralType::Fp32(f) => write!(w, "{f}"),
        LiteralType::Fp64(f) => write!(w, "{f}"),
        LiteralType::String(s) => write!(w, "'{}'", s.escape_debug()),
        LiteralType::Binary(items) => textify_binary(items, ctx, w),
        LiteralType::Date(days) => write!(w, "'{}'", escaped(&days_to_date_string(*days))),
        #[allow(deprecated)]
        LiteralType::Time(microseconds) => {
            write!(
                w,
                "'{}'",
                escaped(&microseconds_to_time_string(*microseconds))
            )
        }
        #[allow(deprecated)]
        LiteralType::Timestamp(microseconds) => {
            write!(
                w,
                "'{}'",
                escaped(&microseconds_to_timestamp_string(*microseconds))
            )
        }
        LiteralType::IntervalYearToMonth(_) => unimplemented_literal("IntervalYearToMonth", ctx, w),
        LiteralType::IntervalDayToSecond(_) => unimplemented_literal("IntervalDayToSecond", ctx, w),
        LiteralType::IntervalCompound(_) => unimplemented_literal("IntervalCompound", ctx, w),
        LiteralType::FixedChar(_) => unimplemented_literal("FixedChar", ctx, w),
        LiteralType::VarChar(_) => unimplemented_literal("VarChar", ctx, w),
        LiteralType::FixedBinary(_) => unimplemented_literal("FixedBinary", ctx, w),
        LiteralType::Decimal(_) => unimplemented_literal("Decimal", ctx, w),
        LiteralType::PrecisionTime(value) => {
            let Some(formatted) = precision_time_to_string(value.value, value.precision) else {
                return invalid_literal(
                    "PrecisionTime",
                    format!(
                        "invalid precision {} for precision time literal",
                        value.precision
                    ),
                    ctx,
                    w,
                );
            };
            write!(w, "'{}'", escaped(&formatted))
        }
        LiteralType::PrecisionTimestamp(value) => {
            let Some(formatted) = precision_timestamp_to_string(value.value, value.precision)
            else {
                return invalid_literal(
                    "PrecisionTimestamp",
                    format!(
                        "invalid precision {} or out-of-range value {} for precision timestamp literal",
                        value.precision, value.value
                    ),
                    ctx,
                    w,
                );
            };
            write!(w, "'{}'", escaped(&formatted))
        }
        LiteralType::PrecisionTimestampTz(value) => {
            let Some(formatted) = precision_timestamp_to_string(value.value, value.precision)
            else {
                return invalid_literal(
                    "PrecisionTimestampTz",
                    format!(
                        "invalid precision {} or out-of-range value {} for precision timestamp tz literal",
                        value.precision, value.value
                    ),
                    ctx,
                    w,
                );
            };
            write!(w, "'{}'", escaped(&formatted))
        }
        LiteralType::Struct(_) => unimplemented_literal("Struct", ctx, w),
        LiteralType::Map(_) => unimplemented_literal("Map", ctx, w),
        #[allow(deprecated)]
        LiteralType::TimestampTz(_) => unimplemented_literal("TimestampTz", ctx, w),
        LiteralType::Uuid(_) => unimplemented_literal("Uuid", ctx, w),
        LiteralType::Null(_) => write!(w, "null"),
        LiteralType::List(_) => unimplemented_literal("List", ctx, w),
        LiteralType::EmptyList(_) => unimplemented_literal("EmptyList", ctx, w),
        LiteralType::EmptyMap(_) => unimplemented_literal("EmptyMap", ctx, w),
        LiteralType::UserDefined(_) => unimplemented_literal("UserDefined", ctx, w),
    }
}

fn invalid_literal<S: Scope, W: fmt::Write>(
    variant: &'static str,
    description: impl Into<std::borrow::Cow<'static, str>>,
    ctx: &S,
    w: &mut W,
) -> fmt::Result {
    write!(
        w,
        "{}",
        ctx.failure(PlanError::invalid(
            "LiteralType",
            Some(variant),
            description
        ))
    )
}

fn literal_type(literal: &expr::Literal) -> Option<Type> {
    let lit = literal.literal_type.as_ref()?;
    let nullability = if literal.nullable {
        Nullability::Nullable as i32
    } else {
        Nullability::Required as i32
    };
    let type_variation_reference = literal.type_variation_reference;
    let kind = match lit {
        LiteralType::Boolean(_) => Kind::Bool(ptype::Boolean {
            nullability,
            type_variation_reference,
        }),
        LiteralType::I8(_) => Kind::I8(ptype::I8 {
            nullability,
            type_variation_reference,
        }),
        LiteralType::I16(_) => Kind::I16(ptype::I16 {
            nullability,
            type_variation_reference,
        }),
        LiteralType::I32(_) => Kind::I32(ptype::I32 {
            nullability,
            type_variation_reference,
        }),
        LiteralType::I64(_) => Kind::I64(ptype::I64 {
            nullability,
            type_variation_reference,
        }),
        LiteralType::Fp32(_) => Kind::Fp32(ptype::Fp32 {
            nullability,
            type_variation_reference,
        }),
        LiteralType::Fp64(_) => Kind::Fp64(ptype::Fp64 {
            nullability,
            type_variation_reference,
        }),
        LiteralType::String(_) => Kind::String(ptype::String {
            nullability,
            type_variation_reference,
        }),
        LiteralType::Binary(_) => Kind::Binary(ptype::Binary {
            nullability,
            type_variation_reference,
        }),
        LiteralType::Date(_) => Kind::Date(ptype::Date {
            nullability,
            type_variation_reference,
        }),
        #[allow(deprecated)]
        LiteralType::Time(_) => Kind::Time(ptype::Time {
            nullability,
            type_variation_reference,
        }),
        #[allow(deprecated)]
        LiteralType::Timestamp(_) => Kind::Timestamp(ptype::Timestamp {
            nullability,
            type_variation_reference,
        }),
        LiteralType::PrecisionTime(value) => Kind::PrecisionTime(ptype::PrecisionTime {
            precision: value.precision,
            nullability,
            type_variation_reference,
        }),
        LiteralType::PrecisionTimestamp(value) => {
            Kind::PrecisionTimestamp(ptype::PrecisionTimestamp {
                precision: value.precision,
                nullability,
                type_variation_reference,
            })
        }
        LiteralType::PrecisionTimestampTz(value) => {
            Kind::PrecisionTimestampTz(ptype::PrecisionTimestampTz {
                precision: value.precision,
                nullability,
                type_variation_reference,
            })
        }
        _ => return None,
    };
    Some(Type { kind: Some(kind) })
}

fn is_default_for_syntax(lit: &LiteralType) -> bool {
    matches!(
        lit,
        LiteralType::Boolean(_)
            | LiteralType::String(_)
            | LiteralType::Binary(_)
            | LiteralType::I64(_)
            | LiteralType::Fp64(_)
    )
}

pub(crate) fn textify_literal<S: Scope, W: fmt::Write>(
    literal: &expr::Literal,
    ctx: &S,
    w: &mut W,
) -> fmt::Result {
    let Some(lit) = literal.literal_type.as_ref() else {
        return write!(
            w,
            "{}",
            ctx.failure(PlanError::invalid(
                "Literal",
                Some("literal_type"),
                "missing literal_type",
            ))
        );
    };
    write_literal_value(lit, ctx, w)?;
    if let LiteralType::Null(typ) = lit {
        write!(w, ":{}", ctx.display(typ))?;
        return Ok(());
    }
    let show_suffix = match ctx.options().literal_types {
        Visibility::Never => false,
        Visibility::Always => true,
        Visibility::Required => literal.nullable || !is_default_for_syntax(lit),
    };
    if show_suffix {
        if let Some(typ) = literal_type(literal) {
            write!(w, ":{}", ctx.display(&typ))?;
        } else if literal.nullable {
            write!(w, "?")?;
        }
    }
    Ok(())
}

impl Textify for expr::Literal {
    fn name() -> &'static str {
        "Literal"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        textify_literal(self, ctx, w)
    }
}
