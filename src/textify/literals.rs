use std::fmt;

use chrono::{DateTime, NaiveDate};
use substrait::proto::expression as expr;
use substrait::proto::expression::literal::LiteralType;

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

pub fn timestamp_to_string(t: i64) -> String {
    let ts = chrono::DateTime::from_timestamp_nanos(t);
    ts.to_rfc3339()
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
        LiteralType::Date(days) => {
            write!(w, "'{}'", escaped(&days_to_date_string(*days)))
        }
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
        LiteralType::IntervalYearToMonth(_) => unimplemented_literal("IntervalYearToMonth", ctx, w),
        LiteralType::IntervalDayToSecond(_) => unimplemented_literal("IntervalDayToSecond", ctx, w),
        LiteralType::IntervalCompound(_) => unimplemented_literal("IntervalCompound", ctx, w),
        LiteralType::FixedChar(_) => unimplemented_literal("FixedChar", ctx, w),
        LiteralType::VarChar(_) => unimplemented_literal("VarChar", ctx, w),
        LiteralType::FixedBinary(_) => unimplemented_literal("FixedBinary", ctx, w),
        LiteralType::Decimal(_) => unimplemented_literal("Decimal", ctx, w),
        LiteralType::Struct(_) => unimplemented_literal("Struct", ctx, w),
        LiteralType::Map(_) => unimplemented_literal("Map", ctx, w),
        #[allow(deprecated)]
        LiteralType::TimestampTz(_) => unimplemented_literal("TimestampTz", ctx, w),
        LiteralType::Uuid(_) => unimplemented_literal("Uuid", ctx, w),
        LiteralType::Null(_) => unimplemented_literal("Null", ctx, w),
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

fn literal_type_suffix(lit: &LiteralType, nullable: bool) -> Option<String> {
    let nullable_marker = if nullable { "?" } else { "" };
    match lit {
        LiteralType::Boolean(_) => Some(format!("boolean{nullable_marker}")),
        LiteralType::I8(_) => Some(format!("i8{nullable_marker}")),
        LiteralType::I16(_) => Some(format!("i16{nullable_marker}")),
        LiteralType::I32(_) => Some(format!("i32{nullable_marker}")),
        LiteralType::I64(_) => Some(format!("i64{nullable_marker}")),
        LiteralType::Fp32(_) => Some(format!("fp32{nullable_marker}")),
        LiteralType::Fp64(_) => Some(format!("fp64{nullable_marker}")),
        LiteralType::String(_) => Some(format!("string{nullable_marker}")),
        LiteralType::Binary(_) => Some(format!("binary{nullable_marker}")),
        LiteralType::Date(_) => Some(format!("date{nullable_marker}")),
        #[allow(deprecated)]
        LiteralType::Time(_) => Some(format!("time{nullable_marker}")),
        #[allow(deprecated)]
        LiteralType::Timestamp(_) => Some(format!("timestamp{nullable_marker}")),
        LiteralType::PrecisionTime(value) => Some(format!(
            "precisiontime{nullable_marker}<{}>",
            value.precision
        )),
        LiteralType::PrecisionTimestamp(value) => Some(format!(
            "precisiontimestamp{nullable_marker}<{}>",
            value.precision
        )),
        LiteralType::PrecisionTimestampTz(value) => Some(format!(
            "precisiontimestamptz{nullable_marker}<{}>",
            value.precision
        )),
        _ => None,
    }
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
    let show_suffix = match ctx.options().literal_types {
        Visibility::Never => false,
        Visibility::Always => true,
        Visibility::Required => literal.nullable || !is_default_for_syntax(lit),
    };
    if show_suffix {
        if let Some(suffix) = literal_type_suffix(lit, literal.nullable) {
            write!(w, ":{suffix}")?;
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

#[cfg(test)]
mod tests {
    use substrait::proto::expression as expr;
    use substrait::proto::expression::literal::{LiteralType, PrecisionTimestamp};

    use crate::fixtures::TestContext;

    fn nullable_literal(lit: LiteralType) -> expr::Literal {
        expr::Literal {
            nullable: true,
            type_variation_reference: 0,
            literal_type: Some(lit),
        }
    }

    #[test]
    fn nullable_precision_timestamp_suffix_places_nullability_before_parameters() {
        let ctx = TestContext::new();
        let literal = nullable_literal(LiteralType::PrecisionTimestamp(PrecisionTimestamp {
            precision: 3,
            value: 1_123,
        }));

        assert_eq!(
            ctx.textify_no_errors(&literal),
            "'1970-01-01T00:00:01.123':precisiontimestamp?<3>"
        );
    }

    #[test]
    fn invalid_precision_timestamp_precision_reports_format_error() {
        let ctx = TestContext::new();
        let literal = nullable_literal(LiteralType::PrecisionTimestamp(PrecisionTimestamp {
            precision: 13,
            value: 1_123,
        }));

        let (output, errors) = ctx.textify(&literal);

        assert_eq!(output, "!{LiteralType}:precisiontimestamp?<13>");
        assert_eq!(errors.0.len(), 1);
        assert!(
            errors
                .to_string()
                .contains("invalid precision 13 or out-of-range value 1123")
        );
    }
}
