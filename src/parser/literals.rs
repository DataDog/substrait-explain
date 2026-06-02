use chrono::{DateTime, NaiveDate, NaiveDateTime, NaiveTime, Timelike};
use substrait::proto::Type;
use substrait::proto::expression::Literal;
use substrait::proto::expression::literal::{LiteralType, PrecisionTime, PrecisionTimestamp};
use substrait::proto::r#type::{Fp64, I64, Kind, Nullability};

use super::{MessageParseError, Rule, ScopedParsePair, unescape_string};
use crate::extensions::SimpleExtensions;

fn invalid_literal(span: pest::Span, message: impl ToString) -> MessageParseError {
    MessageParseError::invalid("literal", span, message)
}

fn to_int_literal(
    value: pest::iterators::Pair<Rule>,
    typ: Option<Type>,
) -> Result<Literal, MessageParseError> {
    assert_eq!(value.as_rule(), Rule::integer);
    let parsed_value: i64 = value.as_str().parse().unwrap();

    const DEFAULT_KIND: Kind = Kind::I64(I64 {
        type_variation_reference: 0,
        nullability: Nullability::Required as i32,
    });

    let kind = typ.and_then(|t| t.kind).unwrap_or(DEFAULT_KIND);

    let (lit, nullability, tvar) = match &kind {
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
        k => {
            return Err(MessageParseError::invalid(
                "int_literal_type",
                value.as_span(),
                format!("Invalid type for integer literal: {k:?}"),
            ));
        }
    };

    Ok(Literal {
        literal_type: Some(lit),
        nullable: nullability != Nullability::Required as i32,
        type_variation_reference: tvar,
    })
}

fn to_float_literal(
    value: pest::iterators::Pair<Rule>,
    typ: Option<Type>,
) -> Result<Literal, MessageParseError> {
    assert_eq!(value.as_rule(), Rule::float);
    let parsed_value: f64 = value.as_str().parse().unwrap();

    const DEFAULT_KIND: Kind = Kind::Fp64(Fp64 {
        type_variation_reference: 0,
        nullability: Nullability::Required as i32,
    });

    let kind = typ.and_then(|t| t.kind).unwrap_or(DEFAULT_KIND);

    let (lit, nullability, tvar) = match &kind {
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
        k => {
            return Err(MessageParseError::invalid(
                "float_literal_type",
                value.as_span(),
                format!("Invalid type for float literal: {k:?}"),
            ));
        }
    };

    Ok(Literal {
        literal_type: Some(lit),
        nullable: nullability != Nullability::Required as i32,
        type_variation_reference: tvar,
    })
}

fn to_boolean_literal(
    value: pest::iterators::Pair<Rule>,
    typ: Option<Type>,
) -> Result<Literal, MessageParseError> {
    assert_eq!(value.as_rule(), Rule::boolean);
    let parsed_value: bool = value.as_str().parse().unwrap();

    let (nullable, tvar) = match typ.and_then(|t| t.kind) {
        Some(Kind::Bool(b)) => (
            b.nullability != Nullability::Required as i32,
            b.type_variation_reference,
        ),
        None => (false, 0),
        Some(k) => {
            return Err(MessageParseError::invalid(
                "bool_literal_type",
                value.as_span(),
                format!("Invalid type for boolean literal: {k:?}"),
            ));
        }
    };

    Ok(Literal {
        literal_type: Some(LiteralType::Boolean(parsed_value)),
        nullable,
        type_variation_reference: tvar,
    })
}

fn to_string_literal(
    value: pest::iterators::Pair<Rule>,
    typ: Option<Type>,
) -> Result<Literal, MessageParseError> {
    assert_eq!(value.as_rule(), Rule::string_literal);
    let string_value = unescape_string(value.clone());

    let Some(typ) = typ else {
        return Ok(Literal {
            literal_type: Some(LiteralType::String(string_value)),
            nullable: false,
            type_variation_reference: 0,
        });
    };

    let Some(kind) = typ.kind else {
        return Ok(Literal {
            literal_type: Some(LiteralType::String(string_value)),
            nullable: false,
            type_variation_reference: 0,
        });
    };

    match &kind {
        Kind::Date(d) => {
            let date_days = parse_date_to_days(&string_value, value.as_span())?;
            Ok(Literal {
                literal_type: Some(LiteralType::Date(date_days)),
                nullable: d.nullability != Nullability::Required as i32,
                type_variation_reference: d.type_variation_reference,
            })
        }
        #[allow(deprecated)]
        Kind::Time(t) => {
            let time_microseconds = parse_time_to_microseconds(&string_value, value.as_span())?;
            Ok(Literal {
                literal_type: Some(LiteralType::Time(time_microseconds)),
                nullable: t.nullability != Nullability::Required as i32,
                type_variation_reference: t.type_variation_reference,
            })
        }
        #[allow(deprecated)]
        Kind::Timestamp(ts) => {
            let timestamp_microseconds =
                parse_timestamp_to_microseconds(&string_value, value.as_span())?;
            Ok(Literal {
                literal_type: Some(LiteralType::Timestamp(timestamp_microseconds)),
                nullable: ts.nullability != Nullability::Required as i32,
                type_variation_reference: ts.type_variation_reference,
            })
        }
        Kind::PrecisionTime(t) => Ok(Literal {
            literal_type: Some(LiteralType::PrecisionTime(PrecisionTime {
                precision: t.precision,
                value: parse_time_to_precision_units(&string_value, t.precision, value.as_span())?,
            })),
            nullable: t.nullability != Nullability::Required as i32,
            type_variation_reference: t.type_variation_reference,
        }),
        Kind::PrecisionTimestamp(t) => Ok(Literal {
            literal_type: Some(LiteralType::PrecisionTimestamp(PrecisionTimestamp {
                precision: t.precision,
                value: parse_timestamp_to_precision_units(
                    &string_value,
                    t.precision,
                    value.as_span(),
                )?,
            })),
            nullable: t.nullability != Nullability::Required as i32,
            type_variation_reference: t.type_variation_reference,
        }),
        Kind::PrecisionTimestampTz(t) => Ok(Literal {
            literal_type: Some(LiteralType::PrecisionTimestampTz(PrecisionTimestamp {
                precision: t.precision,
                value: parse_timestamp_to_precision_units(
                    &string_value,
                    t.precision,
                    value.as_span(),
                )?,
            })),
            nullable: t.nullability != Nullability::Required as i32,
            type_variation_reference: t.type_variation_reference,
        }),
        _ => Ok(Literal {
            literal_type: Some(LiteralType::String(string_value)),
            nullable: false,
            type_variation_reference: 0,
        }),
    }
}

fn parse_date_to_days(date_str: &str, span: pest::Span) -> Result<i32, MessageParseError> {
    let formats = ["%Y-%m-%d", "%Y/%m/%d"];

    for format in &formats {
        if let Ok(date) = NaiveDate::parse_from_str(date_str, format) {
            let epoch = NaiveDate::from_ymd_opt(1970, 1, 1).unwrap();
            let days = date.signed_duration_since(epoch).num_days();
            return Ok(days as i32);
        }
    }

    Err(invalid_literal(
        span,
        format!("Invalid date format: '{date_str}'. Expected YYYY-MM-DD or YYYY/MM/DD"),
    ))
}

fn parse_time_to_microseconds(time_str: &str, span: pest::Span) -> Result<i64, MessageParseError> {
    let formats = ["%H:%M:%S%.f", "%H:%M:%S"];

    for format in &formats {
        if let Ok(time) = NaiveTime::parse_from_str(time_str, format) {
            let midnight = NaiveTime::from_hms_opt(0, 0, 0).unwrap();
            let duration = time.signed_duration_since(midnight);
            return Ok(duration.num_microseconds().unwrap_or(0));
        }
    }

    Err(invalid_literal(
        span,
        format!("Invalid time format: '{time_str}'. Expected HH:MM:SS or HH:MM:SS.fff"),
    ))
}

fn parse_timestamp_to_microseconds(
    timestamp_str: &str,
    span: pest::Span,
) -> Result<i64, MessageParseError> {
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
            let epoch = DateTime::from_timestamp(0, 0).unwrap().naive_utc();
            let duration = datetime.signed_duration_since(epoch);
            return Ok(duration.num_microseconds().unwrap_or(0));
        }
    }

    Err(invalid_literal(
        span,
        format!(
            "Invalid timestamp format: '{timestamp_str}'. Expected YYYY-MM-DDTHH:MM:SS or YYYY-MM-DD HH:MM:SS"
        ),
    ))
}

fn precision_scale(precision: i32, span: pest::Span) -> Result<i64, MessageParseError> {
    if !(0..=12).contains(&precision) {
        return Err(invalid_literal(
            span,
            format!("Invalid temporal precision {precision}; expected 0 through 12"),
        ));
    }
    Ok(10_i64.pow(precision as u32))
}

fn parse_fraction_units(
    fraction: Option<&str>,
    precision: i32,
    span: pest::Span,
) -> Result<i64, MessageParseError> {
    let Some(fraction) = fraction else {
        return Ok(0);
    };
    if !fraction.chars().all(|c| c.is_ascii_digit()) {
        return Err(invalid_literal(span, "Fractional seconds must be digits"));
    }
    let precision = precision as usize;
    if fraction.len() > precision {
        return Err(invalid_literal(
            span,
            format!(
                "Fractional seconds have precision {}, but literal type allows precision {precision}",
                fraction.len()
            ),
        ));
    }
    let mut padded = fraction.to_owned();
    padded.extend(std::iter::repeat_n('0', precision - padded.len()));
    Ok(if padded.is_empty() {
        0
    } else {
        padded.parse::<i64>().unwrap()
    })
}

fn parse_time_parts<'a>(
    time_str: &'a str,
    span: pest::Span,
) -> Result<(NaiveTime, Option<&'a str>), MessageParseError> {
    let (base, fraction) = match time_str.split_once('.') {
        Some((base, fraction)) => (base, Some(fraction)),
        None => (time_str, None),
    };
    let time = NaiveTime::parse_from_str(base, "%H:%M:%S").map_err(|_| {
        invalid_literal(
            span,
            format!("Invalid time format: '{time_str}'. Expected HH:MM:SS or HH:MM:SS.fff"),
        )
    })?;
    Ok((time, fraction))
}

fn parse_time_to_precision_units(
    time_str: &str,
    precision: i32,
    span: pest::Span,
) -> Result<i64, MessageParseError> {
    let scale = precision_scale(precision, span)?;
    let (time, fraction) = parse_time_parts(time_str, span)?;
    let seconds = i64::from(time.num_seconds_from_midnight());
    let units = seconds
        .checked_mul(scale)
        .ok_or_else(|| invalid_literal(span, "Time literal overflow"))?;
    let fraction_units = parse_fraction_units(fraction, precision, span)?;
    units
        .checked_add(fraction_units)
        .ok_or_else(|| invalid_literal(span, "Time literal overflow"))
}

fn parse_timestamp_to_precision_units(
    timestamp_str: &str,
    precision: i32,
    span: pest::Span,
) -> Result<i64, MessageParseError> {
    let scale = precision_scale(precision, span)?;
    let (date_part, time_part) = timestamp_str
        .split_once('T')
        .or_else(|| timestamp_str.split_once(' '))
        .ok_or_else(|| {
            invalid_literal(
                span,
                format!(
                    "Invalid timestamp format: '{timestamp_str}'. Expected YYYY-MM-DDTHH:MM:SS or YYYY-MM-DD HH:MM:SS"
                ),
            )
        })?;
    let date = ["%Y-%m-%d", "%Y/%m/%d"]
        .iter()
        .find_map(|format| NaiveDate::parse_from_str(date_part, format).ok())
        .ok_or_else(|| {
            invalid_literal(
                span,
                format!("Invalid date format: '{date_part}'. Expected YYYY-MM-DD or YYYY/MM/DD"),
            )
        })?;
    let (time, fraction) = parse_time_parts(time_part, span)?;
    let datetime = date.and_time(time);
    let epoch = DateTime::from_timestamp(0, 0).unwrap().naive_utc();
    let seconds = datetime.signed_duration_since(epoch).num_seconds();
    let units = seconds
        .checked_mul(scale)
        .ok_or_else(|| invalid_literal(span, "Timestamp literal overflow"))?;
    let fraction_units = parse_fraction_units(fraction, precision, span)?;
    units
        .checked_add(fraction_units)
        .ok_or_else(|| invalid_literal(span, "Timestamp literal overflow"))
}

impl ScopedParsePair for Literal {
    fn rule() -> Rule {
        Rule::literal
    }

    fn message() -> &'static str {
        "Literal"
    }

    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: pest::iterators::Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        let mut pairs = pair.into_inner();
        let value = pairs.next().unwrap();
        let typ = pairs.next();
        assert!(pairs.next().is_none());
        let typ = match typ {
            Some(t) => Some(Type::parse_pair(extensions, t)?),
            None => None,
        };
        match value.as_rule() {
            Rule::integer => to_int_literal(value, typ),
            Rule::float => to_float_literal(value, typ),
            Rule::boolean => to_boolean_literal(value, typ),
            Rule::string_literal => to_string_literal(value, typ),
            _ => unreachable!("Literal unexpected rule: {:?}", value.as_rule()),
        }
    }
}
