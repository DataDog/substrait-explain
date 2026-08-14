use std::fmt::{self, Display, Formatter};
use std::mem::size_of;
use std::str::FromStr;

use chrono::{DateTime, NaiveDate, NaiveDateTime, NaiveTime};
use pest::Parser as PestParser;
use pest::iterators::Pair;
use substrait::proto::aggregate_rel::Measure;
use substrait::proto::expression::field_reference::{ReferenceType, RootReference, RootType};
use substrait::proto::expression::if_then::IfClause;
use substrait::proto::expression::literal::interval_day_to_second::PrecisionMode;
use substrait::proto::expression::literal::{
    IntervalDayToSecond, LiteralType, PrecisionTime as LitPrecisionTime,
    PrecisionTimestamp as LitPrecisionTimestamp,
};
use substrait::proto::expression::{
    Cast, FieldReference, IfThen, Literal, ReferenceSegment, RexType, ScalarFunction, cast,
    reference_segment,
};
use substrait::proto::function_argument::ArgType;
use substrait::proto::r#type::{Fp64, I64, Kind, Nullability};
use substrait::proto::{AggregateFunction, Expression, FunctionArgument, Type};

use super::types::get_and_validate_anchor;
use super::{
    ExpressionParser, MessageParseError, ParsePair, Rule, RuleIter, ScopedParsePair,
    unescape_string, unwrap_single_pair,
};
use crate::extensions::SimpleExtensions;
use crate::extensions::simple::{CompoundName, ExtensionKind};
use crate::precision::SupportedPrecision;

/// A field index (e.g., parsed from "$0" -> 0).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FieldIndex(pub i32);

impl FieldIndex {
    /// Convert this field index to a FieldReference for use in expressions.
    pub fn to_field_reference(self) -> FieldReference {
        // XXX: Why is it so many layers to make a struct field reference? This is
        // surprisingly complex
        FieldReference {
            reference_type: Some(ReferenceType::DirectReference(ReferenceSegment {
                reference_type: Some(reference_segment::ReferenceType::StructField(Box::new(
                    reference_segment::StructField {
                        field: self.0,
                        child: None,
                    },
                ))),
            })),
            root_type: Some(RootType::RootReference(RootReference {})),
        }
    }
}

impl ParsePair for FieldIndex {
    fn rule() -> Rule {
        Rule::reference
    }

    fn message() -> &'static str {
        "FieldIndex"
    }

    fn parse_pair(pair: Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());
        let inner = unwrap_single_pair(pair);
        let index: i32 = inner.as_str().parse().unwrap();
        FieldIndex(index)
    }
}

impl ParsePair for FieldReference {
    fn rule() -> Rule {
        Rule::reference
    }

    fn message() -> &'static str {
        "FieldReference"
    }

    fn parse_pair(pair: Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());

        // TODO: Other types of references.
        FieldIndex::parse_pair(pair).to_field_reference()
    }
}

const UNSIGNED_INT_KIND: Kind = Kind::I64(I64 {
    type_variation_reference: 0,
    nullability: Nullability::Required as i32,
});

fn to_int_literal(value: Pair<Rule>, typ: Option<Type>) -> Result<Literal, MessageParseError> {
    assert_eq!(value.as_rule(), Rule::integer);
    let parsed_value: i64 = value.as_str().parse().unwrap();

    // If no type is provided, we assume i64, Nullability::Required.
    let kind = typ.and_then(|t| t.kind).unwrap_or(UNSIGNED_INT_KIND);

    let (lit, nullability, tvar) = match &kind {
        // If no type is provided, we assume i64, Nullability::Required.
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

const UNSIGNED_FLOAT_KIND: Kind = Kind::Fp64(Fp64 {
    type_variation_reference: 0,
    nullability: Nullability::Required as i32,
});

fn to_float_literal(value: Pair<Rule>, typ: Option<Type>) -> Result<Literal, MessageParseError> {
    assert_eq!(value.as_rule(), Rule::float);
    let parsed_value: f64 = value.as_str().parse().unwrap();

    // If no type is provided, we assume fp64, Nullability::Required.
    let kind = typ.and_then(|t| t.kind).unwrap_or(UNSIGNED_FLOAT_KIND);

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

fn to_boolean_literal(value: Pair<Rule>, typ: Option<Type>) -> Result<Literal, MessageParseError> {
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

fn to_string_literal(value: Pair<Rule>, typ: Option<Type>) -> Result<Literal, MessageParseError> {
    assert_eq!(value.as_rule(), Rule::string_literal);
    let string_value = unescape_string(value.clone());

    // If no type is provided, default to string
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
            // Parse date in ISO 8601 format: YYYY-MM-DD
            let date_days = parse_date_to_days(&string_value, value.as_span())?;
            Ok(Literal {
                literal_type: Some(LiteralType::Date(date_days)),
                nullable: d.nullability != Nullability::Required as i32,
                type_variation_reference: d.type_variation_reference,
            })
        }
        Kind::IntervalDay(i) => {
            // Sub-second precision comes from the type ascription, as it does for
            // every other parameterized type: `'5d 100ns':interval_day<9>`. The
            // grammar requires the parameter, so it is always present for text
            // input; a caller-constructed type might not have it.
            //
            // Unlike the chrono-backed literals, picoseconds are representable
            // here: `subseconds` is a plain integer count.
            let precision = i
                .precision
                .and_then(SupportedPrecision::from_units)
                .ok_or_else(|| {
                    MessageParseError::invalid(
                        "interval_day_literal_type",
                        value.as_span(),
                        format!(
                            "Invalid precision {} for an interval_day literal; expected one of 0 (seconds), 3 (milliseconds), 6 (microseconds), 9 (nanoseconds), or 12 (picoseconds)",
                            i.precision.map_or("<unset>".to_string(), |p| p.to_string())
                        ),
                    )
                })?;
            let interval = parse_interval_day_duration(&string_value, precision, value.as_span())?;
            Ok(Literal {
                literal_type: Some(LiteralType::IntervalDayToSecond(interval)),
                nullable: i.nullability != Nullability::Required as i32,
                type_variation_reference: i.type_variation_reference,
            })
        }
        #[allow(deprecated)]
        Kind::Time(t) => {
            // Parse time in ISO 8601 format: HH:MM:SS[.fff]
            let time_microseconds = parse_time_to_microseconds(&string_value, value.as_span())?;
            Ok(Literal {
                literal_type: Some(LiteralType::Time(time_microseconds)),
                nullable: t.nullability != Nullability::Required as i32,
                type_variation_reference: t.type_variation_reference,
            })
        }
        #[allow(deprecated)]
        Kind::Timestamp(ts) => {
            // Parse timestamp in ISO 8601 format: YYYY-MM-DDTHH:MM:SS[.fff] or YYYY-MM-DD HH:MM:SS[.fff]
            let timestamp_microseconds =
                parse_timestamp_to_microseconds(&string_value, value.as_span())?;
            Ok(Literal {
                literal_type: Some(LiteralType::Timestamp(timestamp_microseconds)),
                nullable: ts.nullability != Nullability::Required as i32,
                type_variation_reference: ts.type_variation_reference,
            })
        }
        Kind::PrecisionTimestamp(pt) => {
            let precision = pt.precision;
            let timestamp_value = parse_timestamp_to_precision_units(
                &string_value,
                precision,
                "precisiontimestamp",
                value.as_span(),
            )?;
            Ok(Literal {
                literal_type: Some(LiteralType::PrecisionTimestamp(LitPrecisionTimestamp {
                    precision,
                    value: timestamp_value,
                })),
                nullable: pt.nullability != Nullability::Required as i32,
                type_variation_reference: pt.type_variation_reference,
            })
        }
        Kind::PrecisionTimestampTz(pt) => {
            let precision = pt.precision;
            let timestamp_value = parse_timestamp_to_precision_units(
                &string_value,
                precision,
                "precisiontimestamptz",
                value.as_span(),
            )?;
            Ok(Literal {
                literal_type: Some(LiteralType::PrecisionTimestampTz(LitPrecisionTimestamp {
                    precision,
                    value: timestamp_value,
                })),
                nullable: pt.nullability != Nullability::Required as i32,
                type_variation_reference: pt.type_variation_reference,
            })
        }
        Kind::PrecisionTime(pt) => {
            let precision = pt.precision;
            let time_value = parse_time_to_precision_units(
                &string_value,
                precision,
                "precisiontime",
                value.as_span(),
            )?;
            Ok(Literal {
                literal_type: Some(LiteralType::PrecisionTime(LitPrecisionTime {
                    precision,
                    value: time_value,
                })),
                nullable: pt.nullability != Nullability::Required as i32,
                type_variation_reference: pt.type_variation_reference,
            })
        }
        _ => {
            // For other types, treat as string
            Ok(Literal {
                literal_type: Some(LiteralType::String(string_value)),
                nullable: false,
                type_variation_reference: 0,
            })
        }
    }
}

fn to_null_literal(value: Pair<Rule>, typ: Option<Type>) -> Result<Literal, MessageParseError> {
    assert_eq!(value.as_rule(), Rule::null);
    let typ = typ.ok_or_else(|| {
        MessageParseError::invalid(
            "null_literal_type",
            value.as_span(),
            "Null literals require an explicit type annotation, e.g. null:i64?",
        )
    })?;

    Ok(Literal {
        literal_type: Some(LiteralType::Null(typ)),
        nullable: false,
        type_variation_reference: 0,
    })
}

/// Parse a date string using chrono to days since Unix epoch
fn parse_date_to_days(date_str: &str, span: pest::Span) -> Result<i32, MessageParseError> {
    // Try multiple date formats for flexibility
    let formats = ["%Y-%m-%d", "%Y/%m/%d"];

    for format in &formats {
        if let Ok(date) = NaiveDate::parse_from_str(date_str, format) {
            // Calculate days since Unix epoch (1970-01-01)
            let epoch = NaiveDate::from_ymd_opt(1970, 1, 1).unwrap();
            let days = date.signed_duration_since(epoch).num_days();
            return Ok(days as i32);
        }
    }

    Err(MessageParseError::invalid(
        "date_parse_format",
        span,
        format!("Invalid date format: '{date_str}'. Expected YYYY-MM-DD or YYYY/MM/DD"),
    ))
}

/// Parse a time string to microseconds(precision 6) since midnight.
fn parse_time_to_microseconds(time_str: &str, span: pest::Span) -> Result<i64, MessageParseError> {
    parse_time_to_precision_units(time_str, 6, "time", span)
}

/// Parse a timestamp string to microseconds since Unix epoch.
fn parse_timestamp_to_microseconds(
    timestamp_str: &str,
    span: pest::Span,
) -> Result<i64, MessageParseError> {
    parse_timestamp_to_precision_units(timestamp_str, 6, "timestamp", span)
}

/// Convert a `chrono::Duration` to the units implied by `precision`.
/// Errors if `duration` overflows the target unit's i64 range, or if `duration`
/// carries a fractional component finer than `precision` can represent (e.g.
/// `.999` seconds can't be represented exactly at precision 0).
fn duration_to_precision_units(
    duration: chrono::Duration,
    precision: SupportedPrecision,
    literal_kind: &'static str,
    span: pest::Span,
) -> Result<i64, MessageParseError> {
    let out_of_range = || {
        MessageParseError::invalid(
            "precision_literal_out_of_range",
            span,
            format!(
                "value is out of range for a {literal_kind} literal at precision {}",
                precision.units()
            ),
        )
    };
    let fractional_truncated = || {
        MessageParseError::invalid(
            "precision_literal_fractional_truncated",
            span,
            format!(
                "value has a fractional component finer than precision {} can represent for a {literal_kind} literal",
                precision.units()
            ),
        )
    };

    match precision {
        SupportedPrecision::Seconds => {
            let value = duration.num_seconds();
            if chrono::Duration::seconds(value) != duration {
                return Err(fractional_truncated());
            }
            Ok(value)
        }
        SupportedPrecision::Milliseconds => {
            let value = duration.num_milliseconds();
            if chrono::Duration::milliseconds(value) != duration {
                return Err(fractional_truncated());
            }
            Ok(value)
        }
        SupportedPrecision::Microseconds => {
            let value = duration.num_microseconds().ok_or_else(out_of_range)?;
            if chrono::Duration::microseconds(value) != duration {
                return Err(fractional_truncated());
            }
            Ok(value)
        }
        // Nanoseconds is the finest precision chrono can represent, so there's
        // no finer fractional component that could be silently dropped here.
        SupportedPrecision::Nanoseconds => duration.num_nanoseconds().ok_or_else(out_of_range),
        // `check_supported_precision` rejects picoseconds before we get here,
        // since chrono has no sub-nanosecond resolution to convert into.
        SupportedPrecision::Picoseconds => Err(MessageParseError::invalid(
            "precision_literal_unsupported_precision",
            span,
            format!(
                "precision 12 (picoseconds) is not supported for {literal_kind} literals; chrono only supports nanosecond (precision 9) resolution"
            ),
        )),
    }
}

fn check_supported_precision(
    precision: i32,
    literal_kind: &'static str,
    span: pest::Span,
) -> Result<SupportedPrecision, MessageParseError> {
    match SupportedPrecision::from_units(precision) {
        // chrono has no sub-nanosecond resolution, so the literals that go
        // through it can't represent picoseconds.
        Some(SupportedPrecision::Picoseconds) => Err(MessageParseError::invalid(
            "precision_literal_unsupported_precision",
            span,
            format!(
                "precision 12 (picoseconds) is not supported for {literal_kind} literals; chrono only supports nanosecond (precision 9) resolution"
            ),
        )),
        Some(precision) => Ok(precision),
        None => Err(MessageParseError::invalid(
            "precision_literal_invalid_precision",
            span,
            format!(
                "Invalid precision {precision} for a {literal_kind} literal; expected one of 0 (seconds), 3 (milliseconds), 6 (microseconds), or 9 (nanoseconds)"
            ),
        )),
    }
}

/// Parse a timestamp string using chrono to a value in the given precision's units since the Unix epoch.
fn parse_timestamp_to_precision_units(
    timestamp_str: &str,
    precision: i32,
    literal_kind: &'static str,
    span: pest::Span,
) -> Result<i64, MessageParseError> {
    let precision = check_supported_precision(precision, literal_kind, span)?;

    // Try multiple timestamp formats for flexibility
    let formats = [
        "%Y-%m-%dT%H:%M:%S%.f", // ISO 8601 with T and fractional seconds
        "%Y-%m-%dT%H:%M:%S",    // ISO 8601 with T
        "%Y-%m-%d %H:%M:%S%.f", // Space separator with fractional seconds
        "%Y-%m-%d %H:%M:%S",    // Space separator
        "%Y/%m/%dT%H:%M:%S%.f", // Alternative date format with T
        "%Y/%m/%dT%H:%M:%S",    // Alternative date format with T
        "%Y/%m/%d %H:%M:%S%.f", // Alternative date format with space
        "%Y/%m/%d %H:%M:%S",    // Alternative date format with space
    ];

    for format in &formats {
        if let Ok(datetime) = NaiveDateTime::parse_from_str(timestamp_str, format) {
            let epoch = DateTime::from_timestamp(0, 0).unwrap().naive_utc();
            let duration = datetime.signed_duration_since(epoch);
            return duration_to_precision_units(duration, precision, literal_kind, span);
        }
    }

    Err(MessageParseError::invalid(
        "timestamp_parse_format",
        span,
        format!(
            "Invalid timestamp format: '{timestamp_str}'. Expected YYYY-MM-DDTHH:MM:SS or YYYY-MM-DD HH:MM:SS"
        ),
    ))
}

/// Parse a time-of-day string using chrono to a value in the given precision's units since midnight.
fn parse_time_to_precision_units(
    time_str: &str,
    precision: i32,
    literal_kind: &'static str,
    span: pest::Span,
) -> Result<i64, MessageParseError> {
    let precision = check_supported_precision(precision, literal_kind, span)?;

    // Try multiple time formats for flexibility
    let formats = ["%H:%M:%S%.f", "%H:%M:%S"];

    for format in &formats {
        if let Ok(time) = NaiveTime::parse_from_str(time_str, format) {
            let midnight = NaiveTime::from_hms_opt(0, 0, 0).unwrap();
            let duration = time.signed_duration_since(midnight);
            return duration_to_precision_units(duration, precision, literal_kind, span);
        }
    }

    Err(MessageParseError::invalid(
        "time_parse_format",
        span,
        format!("Invalid time format: '{time_str}'. Expected HH:MM:SS or HH:MM:SS.fff"),
    ))
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum IntervalDayError {
    /// A term's number overflows the protobuf field that holds it: `days` and
    /// `seconds` are `i32`, `subseconds` is `i64`.
    TermOverflow {
        unit: &'static str,
        number: String,
        bits: usize,
    },
    /// A sub-second unit that disagrees with the ascribed precision.
    UnitPrecisionMismatch {
        unit: &'static str,
        unit_precision: SupportedPrecision,
        precision: SupportedPrecision,
    },
}

impl Display for IntervalDayError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            IntervalDayError::TermOverflow { unit, number, bits } => write!(
                f,
                "the '{unit}' term {number} does not fit in a {bits}-bit integer"
            ),
            IntervalDayError::UnitPrecisionMismatch {
                unit,
                unit_precision,
                precision,
            } => write!(
                f,
                "the sub-second unit '{unit}' means precision {unit_precision}, but the type is interval_day<{precision}>"
            ),
        }
    }
}

/// Parse the integer part of a duration term, e.g. the `-5` of `-5d`.
///
/// The grammar guarantees an optionally-signed run of digits, so the only way
/// this can fail is a value too large for the protobuf field it is stored in.
fn parse_duration_number<T: FromStr>(
    number: &str,
    unit: &'static str,
) -> Result<T, IntervalDayError> {
    number.parse().map_err(|_| IntervalDayError::TermOverflow {
        unit,
        number: number.to_string(),
        bits: size_of::<T>() * 8,
    })
}

/// Parse the contents of an `interval_day` literal string - e.g. "5d",
/// "4d 5s", "5d 3s 100ns", "-5d -3s" - into an `IntervalDayToSecond` with the
/// sub-second precision taken from the literal's type ascription.
fn parse_interval_day_duration(
    duration_str: &str,
    precision: SupportedPrecision,
    span: pest::Span,
) -> Result<IntervalDayToSecond, MessageParseError> {
    let mut pairs = ExpressionParser::parse(Rule::interval_day_duration, duration_str).map_err(
        |e| {
            MessageParseError::invalid(
                "interval_day_duration",
                span,
                format!(
                    "Invalid duration '{duration_str}': {}. Expected one to three terms, separated by single spaces, in the order days ('d'), seconds ('s'), sub-seconds ('ms', 'us', 'ns', or 'ps'); e.g. '5d', '4d 5s', '5d 3s 100ns'",
                    e.variant.message()
                ),
            )
        },
    )?;
    let pair = pairs.next().expect("interval_day_duration matched");

    interval_day_from_pair(pair, precision).map_err(|e| {
        MessageParseError::invalid(
            "interval_day_duration",
            span,
            format!("Invalid duration '{duration_str}': {e}"),
        )
    })
}

/// Convert a matched [`Rule::interval_day_duration`] into an
/// `IntervalDayToSecond` at `precision`.
fn interval_day_from_pair(
    pair: Pair<Rule>,
    precision: SupportedPrecision,
) -> Result<IntervalDayToSecond, IntervalDayError> {
    assert_eq!(pair.as_rule(), Rule::interval_day_duration);

    let mut interval = IntervalDayToSecond {
        days: 0,
        seconds: 0,
        subseconds: 0,
        precision_mode: Some(PrecisionMode::Precision(precision.units())),
    };

    for term in pair.into_inner() {
        match term.as_rule() {
            Rule::duration_days => {
                let number = unwrap_single_pair(term);
                interval.days = parse_duration_number(number.as_str(), "d")?;
            }
            Rule::duration_seconds => {
                let number = unwrap_single_pair(term);
                interval.seconds = parse_duration_number(number.as_str(), "s")?;
            }
            Rule::duration_subseconds => {
                let mut iter = RuleIter::from(term.into_inner());
                let number = iter.pop(Rule::integer);
                let unit = iter.pop(Rule::subsecond_unit);
                iter.done();

                let unit_precision = SupportedPrecision::from_subsecond_unit(unit.as_str())
                    .expect("the grammar restricts sub-second units to ms/us/ns/ps");
                let unit = unit_precision
                    .subsecond_unit()
                    .expect("a sub-second precision has a sub-second unit");
                // Precision has a single source - the type - so a unit that
                // disagrees with it is ambiguous rather than redundant.
                if unit_precision != precision {
                    return Err(IntervalDayError::UnitPrecisionMismatch {
                        unit,
                        unit_precision,
                        precision,
                    });
                }
                interval.subseconds = parse_duration_number(number.as_str(), unit)?;
            }
            // `interval_day_duration` is anchored with `EOI` so that trailing
            // input is a parse error rather than silently ignored.
            Rule::EOI => {}
            rule => unreachable!("unexpected rule in interval_day_duration: {rule:?}"),
        }
    }

    // TODO: Range validation. Substrait bounds `interval_day` to
    // [-3,650,000..3,650,000] days and defines `subseconds` as the fraction of a
    // second below `precision`, but this crate converts rather than validates:
    // out-of-range values have an unambiguous text form, so parse them and leave
    // range checking to consumers. The conversions above reject only what the
    // protobuf fields cannot hold.
    Ok(interval)
}

impl ScopedParsePair for Literal {
    fn rule() -> Rule {
        Rule::expression_literal
    }

    fn message() -> &'static str {
        "Literal"
    }

    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        let mut pairs = pair.into_inner();
        let value = pairs.next().unwrap(); // First item is always the value
        let typ = pairs.next(); // Second item is optional type
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
            Rule::null => to_null_literal(value, typ),
            _ => unreachable!("Literal unexpected rule: {:?}", value.as_rule()),
        }
    }
}

/// An unresolved reference to a function: its compound name plus an optional explicit anchor.
struct FunctionReference {
    name: CompoundName,
    anchor: Option<u32>,
}

impl ParsePair for FunctionReference {
    fn rule() -> Rule {
        Rule::function_reference
    }

    fn message() -> &'static str {
        "FunctionReference"
    }

    fn parse_pair(pair: Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());
        let mut iter = RuleIter::from(pair.into_inner());

        // Compound function name (required) — e.g. "equal" or "equal:any_any"
        let name = iter.parse_next::<CompoundName>();

        // Optional anchor (e.g., #1)
        let anchor = iter
            .try_pop(Rule::anchor)
            .map(|n| unwrap_single_pair(n).as_str().parse::<u32>().unwrap());

        // Optional URN anchor (e.g., @1); currently unused.
        let _urn_anchor = iter
            .try_pop(Rule::urn_anchor)
            .map(|n| unwrap_single_pair(n).as_str().parse::<u32>().unwrap());

        iter.done();
        FunctionReference { name, anchor }
    }
}

impl FunctionReference {
    /// Resolve this reference to a concrete function anchor against the
    /// extension registry.
    fn resolve(
        &self,
        extensions: &SimpleExtensions,
        span: pest::Span,
    ) -> Result<u32, MessageParseError> {
        get_and_validate_anchor(
            extensions,
            ExtensionKind::Function,
            self.anchor,
            self.name.full(),
            span,
        )
    }
}

/// The parenthesized arguments of a function call (`(expr, expr, ...)`), each
/// parsed as a value argument.
struct FunctionArguments(Vec<FunctionArgument>);

impl ScopedParsePair for FunctionArguments {
    fn rule() -> Rule {
        Rule::argument_list
    }

    fn message() -> &'static str {
        "FunctionArguments"
    }

    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        let mut arguments = Vec::new();
        for e in pair.into_inner() {
            arguments.push(FunctionArgument {
                arg_type: Some(ArgType::Value(Expression::parse_pair(extensions, e)?)),
            });
        }
        Ok(Self(arguments))
    }
}

impl ScopedParsePair for ScalarFunction {
    fn rule() -> Rule {
        Rule::function_call
    }

    fn message() -> &'static str {
        "ScalarFunction"
    }

    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        let span = pair.as_span();
        let mut iter = RuleIter::from(pair.into_inner());

        // Drain the iterator into raw pairs before any fallible parsing, so an
        // early return doesn't trip the RuleIter drop guard with pairs still
        // pending.
        let reference_pair = iter.pop(Rule::function_reference);
        let args_pair = iter.pop(Rule::argument_list);
        let type_pair = iter.pop(Rule::r#type);
        iter.done();

        let reference = FunctionReference::parse_pair(reference_pair);
        let FunctionArguments(arguments) = FunctionArguments::parse_pair(extensions, args_pair)?;
        // Required output type (e.g., :i64); the grammar guarantees its presence.
        let output_type = Type::parse_pair(extensions, type_pair)?;

        // Resolve the function reference against the registry last, once the
        // rest of the call has parsed cleanly.
        let function_reference = reference.resolve(extensions, span)?;
        Ok(ScalarFunction {
            function_reference,
            arguments,
            options: vec![], // TODO: Function Options
            output_type: Some(output_type),
            #[allow(deprecated)]
            args: vec![],
        })
    }
}

impl ScopedParsePair for Cast {
    fn rule() -> Rule {
        Rule::cast_expression
    }

    fn message() -> &'static str {
        "Cast"
    }

    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        let mut pairs = pair.into_inner();

        let expr_pair = pairs.next().unwrap();

        // Optional failure behavior prefix: ? = RETURN_NULL, ! = THROW_EXCEPTION
        let next = pairs.next().unwrap();
        let (failure_behavior, type_pair) = if next.as_rule() == Rule::cast_failure_behavior {
            let fb = match next.as_str() {
                "?" => cast::FailureBehavior::ReturnNull as i32,
                "!" => cast::FailureBehavior::ThrowException as i32,
                _ => unreachable!("Grammar guarantees cast_failure_behavior is ? or !"),
            };
            (fb, pairs.next().unwrap())
        } else {
            (cast::FailureBehavior::Unspecified as i32, next)
        };

        assert!(pairs.next().is_none());

        let input = Expression::parse_pair(extensions, expr_pair)?;
        let target_type = Type::parse_pair(extensions, type_pair)?;

        Ok(Cast {
            r#type: Some(target_type),
            input: Some(Box::new(input)),
            failure_behavior,
        })
    }
}

impl ScopedParsePair for Expression {
    fn rule() -> Rule {
        Rule::expression
    }

    fn message() -> &'static str {
        "Expression"
    }

    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        let inner = unwrap_single_pair(pair);
        match inner.as_rule() {
            Rule::expression_literal => Ok(Expression {
                rex_type: Some(RexType::Literal(Literal::parse_pair(extensions, inner)?)),
            }),
            Rule::function_call => Ok(Expression {
                rex_type: Some(RexType::ScalarFunction(ScalarFunction::parse_pair(
                    extensions, inner,
                )?)),
            }),
            Rule::reference => Ok(Expression {
                rex_type: Some(RexType::Selection(Box::new(FieldReference::parse_pair(
                    inner,
                )))),
            }),
            Rule::if_then => Ok(Expression {
                rex_type: Some(RexType::IfThen(Box::new(IfThen::parse_pair(
                    extensions, inner,
                )?))),
            }),
            Rule::cast_expression => Ok(Expression {
                rex_type: Some(RexType::Cast(Box::new(Cast::parse_pair(
                    extensions, inner,
                )?))),
            }),
            _ => unreachable!(
                "Grammar guarantees expression can only be expression_literal, function_call, reference, if_then, or cast_expression, got: {:?}",
                inner.as_rule()
            ),
        }
    }
}

impl ScopedParsePair for IfClause {
    fn rule() -> Rule {
        Rule::if_clause
    }

    fn message() -> &'static str {
        "IfClause"
    }

    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());
        let mut pairs = pair.into_inner(); // should have 2 children, 2 expressions

        let condition = pairs.next().unwrap();
        let result = pairs.next().unwrap();
        assert!(pairs.next().is_none());

        let ex1 = Some(Expression::parse_pair(extensions, condition)?);
        let ex2 = Some(Expression::parse_pair(extensions, result)?);

        Ok(IfClause {
            r#if: ex1,
            then: ex2,
        })
    }
}

impl ScopedParsePair for IfThen {
    fn rule() -> Rule {
        Rule::if_then
    }
    fn message() -> &'static str {
        "IfThen"
    }

    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());

        let mut iter = RuleIter::from(pair.into_inner()); // should have 2 or more children

        let mut ifs: Vec<IfClause> = Vec::new();

        // gets all of the if clauses
        while let Some(p) = iter.try_pop(Rule::if_clause) {
            let if_clause = IfClause::parse_pair(extensions, p)?;
            ifs.push(if_clause);
        }

        let pair = iter.try_pop(Rule::expression).unwrap(); // should be else expression
        iter.done();
        let else_clause = Some(Box::new(Expression::parse_pair(extensions, pair)?));

        Ok(IfThen {
            ifs,
            r#else: else_clause,
        })
    }
}
pub struct Name(pub String);

impl ParsePair for Name {
    fn rule() -> Rule {
        Rule::name
    }

    fn message() -> &'static str {
        "Name"
    }

    fn parse_pair(pair: Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());
        let inner = unwrap_single_pair(pair);
        match inner.as_rule() {
            Rule::identifier => Name(inner.as_str().to_string()),
            Rule::quoted_name => Name(unescape_string(inner)),
            _ => unreachable!("Name unexpected rule: {:?}", inner.as_rule()),
        }
    }
}

impl ParsePair for CompoundName {
    fn rule() -> Rule {
        Rule::function_signature
    }

    fn message() -> &'static str {
        "CompoundName"
    }

    fn parse_pair(pair: Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());
        CompoundName::new(pair.as_str())
    }
}

impl ScopedParsePair for Measure {
    fn rule() -> Rule {
        Rule::function_call
    }

    fn message() -> &'static str {
        "Measure"
    }

    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Self::rule());

        // Parse as ScalarFunction, then convert to AggregateFunction
        let scalar = ScalarFunction::parse_pair(extensions, pair)?;
        Ok(Measure {
            measure: Some(AggregateFunction {
                function_reference: scalar.function_reference,
                arguments: scalar.arguments,
                options: scalar.options,
                output_type: scalar.output_type,
                invocation: 0, // TODO: support invocation (ALL, DISTINCT, etc.)
                phase: 0, // TODO: support phase (INITIAL_TO_RESULT, PARTIAL_TO_INTERMEDIATE, etc.)
                sorts: vec![], // TODO: support sorts for ordered aggregates
                #[allow(deprecated)]
                args: scalar.args,
            }),
            filter: None, // TODO: support filter conditions on aggregate measures
        })
    }
}

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use pest::Parser as PestParser;

    use super::*;
    use crate::parser::ExpressionParser;

    fn parse_exact(rule: Rule, input: &'_ str) -> Pair<'_, Rule> {
        let mut pairs = ExpressionParser::parse(rule, input).unwrap();
        assert_eq!(pairs.as_str(), input);
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);
        pair
    }

    fn assert_parses_to<T: ParsePair + PartialEq + Debug>(input: &str, expected: T) {
        let pair = parse_exact(T::rule(), input);
        let actual = T::parse_pair(pair);
        assert_eq!(actual, expected);
    }

    fn assert_parses_with<T: ScopedParsePair + PartialEq + Debug>(
        ext: &SimpleExtensions,
        input: &str,
        expected: T,
    ) {
        let pair = parse_exact(T::rule(), input);
        let actual = T::parse_pair(ext, pair).unwrap();
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_parse_field_reference() {
        assert_parses_to("$1", FieldIndex(1).to_field_reference());
    }

    #[test]
    fn test_parse_integer_literal() {
        let extensions = SimpleExtensions::default();
        let expected = Literal {
            literal_type: Some(LiteralType::I64(1)),
            nullable: false,
            type_variation_reference: 0,
        };
        assert_parses_with(&extensions, "1", expected);
    }

    #[test]
    fn test_parse_float_literal() {
        // First test that the grammar can parse floats
        let pairs = ExpressionParser::parse(Rule::float, "3.82").unwrap();
        let parsed_text = pairs.as_str();
        assert_eq!(parsed_text, "3.82");

        let extensions = SimpleExtensions::default();
        let expected = Literal {
            literal_type: Some(LiteralType::Fp64(3.82)),
            nullable: false,
            type_variation_reference: 0,
        };
        assert_parses_with(&extensions, "3.82", expected);
    }

    #[test]
    fn test_parse_negative_float_literal() {
        let extensions = SimpleExtensions::default();
        let expected = Literal {
            literal_type: Some(LiteralType::Fp64(-2.5)),
            nullable: false,
            type_variation_reference: 0,
        };
        assert_parses_with(&extensions, "-2.5", expected);
    }

    #[test]
    fn test_parse_boolean_true_literal() {
        let extensions = SimpleExtensions::default();
        let expected = Literal {
            literal_type: Some(LiteralType::Boolean(true)),
            nullable: false,
            type_variation_reference: 0,
        };
        assert_parses_with(&extensions, "true", expected);
    }

    #[test]
    fn test_parse_boolean_false_literal() {
        let extensions = SimpleExtensions::default();
        let expected = Literal {
            literal_type: Some(LiteralType::Boolean(false)),
            nullable: false,
            type_variation_reference: 0,
        };
        assert_parses_with(&extensions, "false", expected);
    }

    #[test]
    fn test_parse_nullable_boolean_literal() {
        let extensions = SimpleExtensions::default();
        let expected_true = Literal {
            literal_type: Some(LiteralType::Boolean(true)),
            nullable: true,
            type_variation_reference: 0,
        };
        let expected_false = Literal {
            literal_type: Some(LiteralType::Boolean(false)),
            nullable: true,
            type_variation_reference: 0,
        };
        assert_parses_with(&extensions, "true:boolean?", expected_true);
        assert_parses_with(&extensions, "false:boolean?", expected_false);
    }

    #[test]
    fn test_parse_nullable_integer_literal() {
        let extensions = SimpleExtensions::default();
        let expected_i32 = Literal {
            literal_type: Some(LiteralType::I32(78)),
            nullable: true,
            type_variation_reference: 0,
        };
        let expected_i64 = Literal {
            literal_type: Some(LiteralType::I64(42)),
            nullable: true,
            type_variation_reference: 0,
        };
        assert_parses_with(&extensions, "78:i32?", expected_i32);
        assert_parses_with(&extensions, "42:i64?", expected_i64);
    }

    #[test]
    fn test_parse_nullable_float_literal() {
        let extensions = SimpleExtensions::default();
        let expected_fp64 = Literal {
            literal_type: Some(LiteralType::Fp64(3.19)),
            nullable: true,
            type_variation_reference: 0,
        };
        assert_parses_with(&extensions, "3.19:fp64?", expected_fp64);
    }

    #[test]
    fn test_parse_float_literal_with_fp32_type() {
        let extensions = SimpleExtensions::default();
        let pair = parse_exact(Rule::expression_literal, "3.82:fp32");
        let result = Literal::parse_pair(&extensions, pair).unwrap();

        match result.literal_type {
            Some(LiteralType::Fp32(val)) => assert!((val - 3.82).abs() < f32::EPSILON),
            _ => panic!("Expected Fp32 literal type"),
        }
    }

    #[test]
    fn test_parse_date_literal() {
        let extensions = SimpleExtensions::default();
        let pair = parse_exact(Rule::expression_literal, "'2023-12-25':date");
        let result = Literal::parse_pair(&extensions, pair).unwrap();

        match result.literal_type {
            Some(LiteralType::Date(days)) => {
                // 2023-12-25 should be a positive number of days since 1970-01-01
                assert!(
                    days > 0,
                    "Expected positive days since epoch, got: {}",
                    days
                );
            }
            _ => panic!("Expected Date literal type, got: {:?}", result.literal_type),
        }
    }

    #[test]
    fn test_parse_time_literal() {
        let extensions = SimpleExtensions::default();
        let pair = parse_exact(Rule::expression_literal, "'14:30:45':time");
        let result = Literal::parse_pair(&extensions, pair).unwrap();

        match result.literal_type {
            #[allow(deprecated)]
            Some(LiteralType::Time(microseconds)) => {
                // 14:30:45 = (14*3600 + 30*60 + 45) * 1_000_000 microseconds
                let expected = (14 * 3600 + 30 * 60 + 45) * 1_000_000;
                assert_eq!(microseconds, expected);
            }
            _ => panic!("Expected Time literal type, got: {:?}", result.literal_type),
        }
    }

    #[test]
    fn test_parse_timestamp_literal_with_t() {
        let extensions = SimpleExtensions::default();
        let pair = parse_exact(Rule::expression_literal, "'2023-01-01T12:00:00':timestamp");
        let result = Literal::parse_pair(&extensions, pair).unwrap();

        match result.literal_type {
            #[allow(deprecated)]
            Some(LiteralType::Timestamp(microseconds)) => {
                assert!(
                    microseconds > 0,
                    "Expected positive microseconds since epoch"
                );
            }
            _ => panic!(
                "Expected Timestamp literal type, got: {:?}",
                result.literal_type
            ),
        }
    }

    #[test]
    fn test_parse_timestamp_literal_with_space() {
        let extensions = SimpleExtensions::default();
        let pair = parse_exact(Rule::expression_literal, "'2023-01-01 12:00:00':timestamp");
        let result = Literal::parse_pair(&extensions, pair).unwrap();

        match result.literal_type {
            #[allow(deprecated)]
            Some(LiteralType::Timestamp(microseconds)) => {
                assert!(
                    microseconds > 0,
                    "Expected positive microseconds since epoch"
                );
            }
            _ => panic!(
                "Expected Timestamp literal type, got: {:?}",
                result.literal_type
            ),
        }
    }

    #[test]
    fn test_parse_precision_timestamp_literal() {
        let extensions = SimpleExtensions::default();
        let pair = parse_exact(
            Rule::expression_literal,
            "'2023-01-01T12:00:00.123456789':precisiontimestamp<9>",
        );
        let result = Literal::parse_pair(&extensions, pair).unwrap();

        match result.literal_type {
            Some(LiteralType::PrecisionTimestamp(p)) => {
                assert_eq!(p.precision, 9);
                assert!(p.value > 0, "Expected positive value since epoch");
                // p.value is total nanoseconds since epoch; mod 1e9 isolates just
                // the sub-second fraction, i.e. the ".123456789" part of the input.
                assert_eq!(p.value % 1_000_000_000, 123_456_789);
            }
            _ => panic!(
                "Expected PrecisionTimestamp literal type, got: {:?}",
                result.literal_type
            ),
        }
        assert!(!result.nullable);
    }

    #[test]
    fn test_parse_precision_timestamp_tz_literal_nullable() {
        let extensions = SimpleExtensions::default();
        let pair = parse_exact(
            Rule::expression_literal,
            "'2023-01-01T12:00:00.123':precisiontimestamptz?<3>",
        );
        let result = Literal::parse_pair(&extensions, pair).unwrap();

        match result.literal_type {
            Some(LiteralType::PrecisionTimestampTz(p)) => {
                assert_eq!(p.precision, 3);
                assert_eq!(p.value % 1000, 123);
            }
            _ => panic!(
                "Expected PrecisionTimestampTz literal type, got: {:?}",
                result.literal_type
            ),
        }
        assert!(result.nullable);
    }

    #[test]
    fn test_parse_precision_time_literal() {
        let extensions = SimpleExtensions::default();
        let pair = parse_exact(
            Rule::expression_literal,
            "'14:30:45.123456':precisiontime<6>",
        );
        let result = Literal::parse_pair(&extensions, pair).unwrap();

        match result.literal_type {
            Some(LiteralType::PrecisionTime(p)) => {
                assert_eq!(p.precision, 6);
                let expected = (14 * 3600 + 30 * 60 + 45) * 1_000_000 + 123_456;
                assert_eq!(p.value, expected);
            }
            _ => panic!(
                "Expected PrecisionTime literal type, got: {:?}",
                result.literal_type
            ),
        }
    }

    #[test]
    fn test_parse_precision_timestamp_literal_precision_12_unsupported() {
        let extensions = SimpleExtensions::default();
        let pair = parse_exact(
            Rule::expression_literal,
            "'2023-01-01T12:00:00':precisiontimestamp<12>",
        );
        let err = Literal::parse_pair(&extensions, pair).unwrap_err();
        assert!(err.to_string().contains("picoseconds"));
    }

    #[test]
    fn test_parse_precision_timestamp_literal_invalid_precision() {
        let extensions = SimpleExtensions::default();
        // 5 isn't a recognized precision for precisiontimestamp, so this should error.
        let pair = parse_exact(
            Rule::expression_literal,
            "'2023-01-01T12:00:00':precisiontimestamp<5>",
        );
        let err = Literal::parse_pair(&extensions, pair).unwrap_err();
        assert!(err.to_string().contains("Invalid precision 5"));
    }

    #[test]
    fn test_parse_precision_timestamp_literal_nullable() {
        let extensions = SimpleExtensions::default();
        let pair = parse_exact(
            Rule::expression_literal,
            "'2023-01-01T12:00:00.123456789':precisiontimestamp?<9>",
        );
        let result = Literal::parse_pair(&extensions, pair).unwrap();
        assert!(result.nullable);
    }

    #[test]
    fn test_parse_precision_time_literal_nullable() {
        let extensions = SimpleExtensions::default();
        let pair = parse_exact(
            Rule::expression_literal,
            "'14:30:45.123456':precisiontime?<6>",
        );
        let result = Literal::parse_pair(&extensions, pair).unwrap();
        assert!(result.nullable);
    }

    #[test]
    fn test_parse_precision_timestamp_literal_fractional_truncated() {
        let extensions = SimpleExtensions::default();
        // precisiontimestamp<0> declares second resolution, but the value has a
        // fractional second; this must error rather than silently drop the ".999".
        let pair = parse_exact(
            Rule::expression_literal,
            "'2023-01-01T12:00:00.999':precisiontimestamp<0>",
        );
        let err = Literal::parse_pair(&extensions, pair).unwrap_err();
        assert!(err.to_string().contains("fractional"));
    }

    #[test]
    fn test_parse_precision_time_literal_fractional_truncated() {
        let extensions = SimpleExtensions::default();
        // precisiontime<3> declares millisecond resolution, but the value has
        // more fractional digits than that; this must error, not truncate.
        let pair = parse_exact(
            Rule::expression_literal,
            "'14:30:45.123456':precisiontime<3>",
        );
        let err = Literal::parse_pair(&extensions, pair).unwrap_err();
        assert!(err.to_string().contains("fractional"));
    }

    #[test]
    fn test_parse_precision_timestamp_literal_nanosecond_overflow() {
        let extensions = SimpleExtensions::default();
        // 2300 is past chrono's ~292-year-around-1970 nanosecond range,
        // so this must error rather than silently parsing to some truncated or zeroed value.
        let pair = parse_exact(
            Rule::expression_literal,
            "'2300-01-01T00:00:00':precisiontimestamp<9>",
        );
        let err = Literal::parse_pair(&extensions, pair).unwrap_err();
        assert!(err.to_string().contains("out of range"));
    }

    fn parse_interval_day_literal(input: &str) -> Result<IntervalDayToSecond, MessageParseError> {
        let extensions = SimpleExtensions::default();
        let pair = parse_exact(Rule::expression_literal, input);
        let result = Literal::parse_pair(&extensions, pair)?;
        match result.literal_type {
            Some(LiteralType::IntervalDayToSecond(interval)) => Ok(interval),
            other => panic!("Expected IntervalDayToSecond literal type, got: {other:?}"),
        }
    }

    fn assert_interval_day(input: &str, days: i32, seconds: i32, subseconds: i64, precision: i32) {
        let interval = parse_interval_day_literal(input).unwrap();
        assert_eq!(
            interval,
            IntervalDayToSecond {
                days,
                seconds,
                subseconds,
                precision_mode: Some(PrecisionMode::Precision(precision)),
            },
            "input: {input}"
        );
    }

    #[test]
    fn test_parse_interval_day_literals() {
        for (input, days, seconds, subseconds, precision) in [
            // Precision comes from the type ascription, so a value with no
            // sub-second term can still carry a sub-second precision.
            ("'5d':interval_day<0>", 5, 0, 0, 0),
            ("'5d':interval_day<9>", 5, 0, 0, 9),
            ("'4d 5s':interval_day<0>", 4, 5, 0, 0),
            ("'123ms':interval_day<3>", 0, 0, 123, 3),
            ("'123456us':interval_day<6>", 0, 0, 123_456, 6),
            ("'123456789ns':interval_day<9>", 0, 0, 123_456_789, 9),
            ("'5d 3s 100ns':interval_day<9>", 5, 3, 100, 9),
            // Each term carries its own sign, matching the separate proto fields.
            ("'-5d 3s':interval_day<0>", -5, 3, 0, 0),
            ("'-500000000ns':interval_day<9>", 0, 0, -500_000_000, 9),
            // Nullability is written before the precision parameter.
            ("'5d':interval_day?<6>", 5, 0, 0, 6),
            // This crate converts rather than validates, so values outside the
            // Substrait ranges parse as long as the proto fields can hold them.
            ("'3650001d':interval_day<0>", 3_650_001, 0, 0, 0),
            ("'1000000000ns':interval_day<9>", 0, 0, 1_000_000_000, 9),
        ] {
            assert_interval_day(input, days, seconds, subseconds, precision);
        }
    }

    #[test]
    fn test_parse_interval_day_literal_errors() {
        for input in [
            // Shape errors, all caught by the interval_day_duration rule.
            "'':interval_day<0>",
            "'5x':interval_day<0>",
            "'5':interval_day<0>",
            // Each unit may appear at most once...
            "'5d 3d':interval_day<0>",
            "'5s 3s':interval_day<0>",
            "'3ms 5us':interval_day<3>",
            // ...and terms must be in descending order.
            "'3s 5d':interval_day<0>",
            "'100ns 5d 3s':interval_day<9>",
            // Terms are separated by exactly one space, with none around them.
            "'5d   3s':interval_day<0>",
            "'  5d 3s  ':interval_day<0>",
            "'5d\t3s':interval_day<0>",
            // The sub-second unit has to agree with the type's precision.
            "'100ns':interval_day<6>",
            // Precisions with no duration unit, so no value can be written at
            // them; 13 and -1 are also outside the type's own 0..=12 range.
            "'5d 3s':interval_day<4>",
            "'5d':interval_day<13>",
            "'5d':interval_day<-1>",
            // Values the proto fields cannot hold.
            "'2200000000s':interval_day<0>",
            "'99999999999d':interval_day<0>",
            "'99999999999999999999ns':interval_day<9>",
        ] {
            assert!(
                parse_interval_day_literal(input).is_err(),
                "expected {input} to fail"
            );
        }
    }

    #[test]
    fn test_parse_interval_day_literal_requires_precision() {
        // Bare `interval_day` has no precision, so it isn't a type name; it falls
        // through to the user-defined type rule and fails to resolve.
        let err = parse_interval_day_literal("'5d':interval_day")
            .expect_err("bare interval_day should not be a known type");
        assert!(
            err.to_string().contains("interval_day"),
            "unexpected error: {err}"
        );
    }

    #[test]
    fn test_parse_interval_day_literal_unit_precision_mismatch_message() {
        let err = parse_interval_day_literal("'100ns':interval_day<6>")
            .expect_err("a sub-second unit that disagrees with the type should fail");
        assert!(
            err.to_string().contains("means precision 9"),
            "unexpected error: {err}"
        );
    }

    /// Helper function to create a literal boolean expression
    fn make_literal_bool(value: bool) -> Expression {
        Expression {
            rex_type: Some(RexType::Literal(Literal {
                literal_type: Some(LiteralType::Boolean(value)),
                nullable: false,
                type_variation_reference: 0,
            })),
        }
    }

    #[test]
    fn test_parse_if_then_single_clause() {
        let extensions = SimpleExtensions::default();
        let input = "if_then(true -> 42, _ -> 0)";
        let pair = parse_exact(Rule::if_then, input);
        let result = IfThen::parse_pair(&extensions, pair).unwrap();

        assert_eq!(result.ifs.len(), 1);
        assert!(result.r#else.is_some());
    }

    #[test]
    fn test_parse_if_then_with_typed_literals() {
        let extensions = SimpleExtensions::default();
        let input = "if_then(true -> 100:i32, _ -> -100:i32)";
        let pair = parse_exact(Rule::if_then, input);
        let result = IfThen::parse_pair(&extensions, pair).unwrap();

        assert_eq!(result.ifs.len(), 1);
        assert!(result.r#else.is_some());
    }

    #[test]
    fn test_parse_if_then_with_date_literals() {
        let extensions = SimpleExtensions::default();
        let input = "if_then(true -> '2023-12-25':date, _ -> '1970-01-01':date)";
        let pair = parse_exact(Rule::if_then, input);
        let result = IfThen::parse_pair(&extensions, pair).unwrap();

        assert_eq!(result.ifs.len(), 1);
        assert!(result.r#else.is_some());
    }

    #[test]
    fn test_parse_if_then_with_time_literals() {
        let extensions = SimpleExtensions::default();
        let input = "if_then(true -> '14:30:45':time, _ -> '00:00:00':time)";
        let pair = parse_exact(Rule::if_then, input);
        let result = IfThen::parse_pair(&extensions, pair).unwrap();

        assert_eq!(result.ifs.len(), 1);
        assert!(result.r#else.is_some());
    }

    #[test]
    fn test_parse_if_then_with_timestamp_literals() {
        let extensions = SimpleExtensions::default();
        let input = "if_then(true -> '2023-01-01T12:00:00':timestamp, _ -> '1970-01-01T00:00:00':timestamp)";
        let pair = parse_exact(Rule::if_then, input);
        let result = IfThen::parse_pair(&extensions, pair).unwrap();

        assert_eq!(result.ifs.len(), 1);
        assert!(result.r#else.is_some());
    }

    #[test]
    fn test_parse_if_clause_with_whitespace_variations() {
        let extensions = SimpleExtensions::default();

        // Test with various whitespace patterns
        let inputs = vec!["true->false", "true -> false", "true  ->  false"];

        for input in inputs {
            let pair = parse_exact(Rule::if_clause, input);
            let result = IfClause::parse_pair(&extensions, pair).unwrap();
            assert!(result.r#if.is_some());
            assert!(result.then.is_some());
        }
    }

    #[test]
    fn test_if_clause_structure() {
        let extensions = SimpleExtensions::default();
        let pair = parse_exact(Rule::if_clause, "42 -> 100");
        let result = IfClause::parse_pair(&extensions, pair).unwrap();

        // Verify the if clause has both condition and result
        let if_expr = result.r#if.as_ref().unwrap();
        let then_expr = result.then.as_ref().unwrap();

        // Check that they are literal expressions
        match (&if_expr.rex_type, &then_expr.rex_type) {
            (Some(RexType::Literal(_)), Some(RexType::Literal(_))) => {
                // Success - both are literals as expected
            }
            _ => panic!("Expected both if and then to be literals"),
        }
    }

    #[test]
    fn test_if_then_structure() {
        let extensions = SimpleExtensions::default();
        let input = "if_then(true -> 1, false -> 2, _ -> 0)";
        let pair = parse_exact(Rule::if_then, input);
        let result = IfThen::parse_pair(&extensions, pair).unwrap();

        // Verify structure
        assert_eq!(result.ifs.len(), 2);

        // Check each if clause
        for clause in &result.ifs {
            assert!(clause.r#if.is_some(), "If clause condition should exist");
            assert!(clause.then.is_some(), "If clause result should exist");
        }

        // Check else clause
        assert!(result.r#else.is_some(), "Else clause should exist");
    }

    #[test]
    fn test_parse_if_then_mixed_types_in_conditions() {
        let extensions = SimpleExtensions::default();
        // Different types in conditions (not results)
        let input = "if_then(true -> 1, true -> 'yes', 'yes' -> true, 42 -> 2, $0 -> 3, _ -> 0)";
        let pair = parse_exact(Rule::if_then, input);
        let result = IfThen::parse_pair(&extensions, pair).unwrap();

        assert_eq!(result.ifs.len(), 5);
        assert!(result.r#else.is_some());
    }

    #[test]
    fn test_if_then_preserves_clause_order() {
        let extensions = SimpleExtensions::default();
        let input = "if_then(1 -> 10, 2 -> 20, 3 -> 30, _ -> 0)";
        let pair = parse_exact(Rule::if_then, input);
        let result = IfThen::parse_pair(&extensions, pair).unwrap();

        assert_eq!(result.ifs.len(), 3);

        // Verify the clauses are in order by checking the literal values
        for (i, clause) in result.ifs.iter().enumerate() {
            if let Some(Expression {
                rex_type: Some(RexType::Literal(lit)),
            }) = &clause.r#if
                && let Some(LiteralType::I64(val)) = &lit.literal_type
            {
                assert_eq!(*val, (i as i64) + 1);
            }
        }
    }

    #[test]
    fn test_parse_if_then() {
        let extensions = SimpleExtensions::default();

        let c1 = IfClause {
            r#if: Some(make_literal_bool(true)),
            then: Some(make_literal_bool(true)),
        };

        let c2 = IfClause {
            r#if: Some(make_literal_bool(false)),
            then: Some(make_literal_bool(false)),
        };

        let if_clause = IfThen {
            ifs: vec![c1, c2],
            r#else: Some(Box::new(make_literal_bool(false))),
        };
        assert_parses_with(
            &extensions,
            "if_then(true -> true , false -> false, _ -> false)",
            if_clause,
        );
    }

    // ---- Tests for function_signature grammar rule ----

    fn parse_function_signature(input: &str) -> CompoundName {
        let pair = parse_exact(Rule::function_signature, input);
        CompoundName::parse_pair(pair)
    }

    #[test]
    fn test_compound_name_plain() {
        assert_eq!(parse_function_signature("add").full(), "add");
    }

    #[test]
    fn test_compound_name_full_zero_arg_type_signature() {
        // A Full name whose type signature encodes zero argument types (nothing after the colon).
        let n = parse_function_signature("add:");
        assert_eq!(n.full(), "add:");
        assert_eq!(n.base(), "add");
        assert!(n.matches("add:"));
        assert!(!n.matches("add:i64_i64"));
        assert!(n.matches("add"));
    }

    #[test]
    fn test_compound_name_with_signature() {
        assert_eq!(
            parse_function_signature("equal:any_any").full(),
            "equal:any_any"
        );
        assert_eq!(
            parse_function_signature("regexp_match_substring:str_str_i64").full(),
            "regexp_match_substring:str_str_i64"
        );
        assert_eq!(
            parse_function_signature("add:i64_i64").full(),
            "add:i64_i64"
        );
    }

    #[test]
    fn test_compound_name_trailing_colon_grammar() {
        // "count:" (trailing colon, zero-arg type signature) parses as a compound name with
        // an empty signature suffix: base "count", has_signature true, full "count:".
        let name = parse_function_signature("count:");
        assert_eq!(name.base(), "count");
        assert_eq!(name.full(), "count:");
        assert!(
            name.has_signature(),
            "trailing colon must set has_signature"
        );
    }

    #[test]
    fn test_compound_name_stops_at_opening_paren() {
        // In a function call, the function_signature must stop before the '('.
        let pairs = ExpressionParser::parse(Rule::function_signature, "equal:any_any").unwrap();
        assert_eq!(pairs.as_str(), "equal:any_any");
    }

    #[test]
    fn test_parse_function_arguments() {
        // The argument list parses on its own, independent of a function call.
        let exts = SimpleExtensions::default();

        let pair = parse_exact(Rule::argument_list, "()");
        let FunctionArguments(args) = FunctionArguments::parse_pair(&exts, pair).unwrap();
        assert!(args.is_empty());

        let pair = parse_exact(Rule::argument_list, "($0, 1)");
        let FunctionArguments(args) = FunctionArguments::parse_pair(&exts, pair).unwrap();
        assert_eq!(
            args,
            vec![
                FunctionArgument {
                    arg_type: Some(ArgType::Value(Expression {
                        rex_type: Some(RexType::Selection(Box::new(
                            FieldIndex(0).to_field_reference()
                        ))),
                    })),
                },
                FunctionArgument {
                    arg_type: Some(ArgType::Value(Expression {
                        rex_type: Some(RexType::Literal(Literal {
                            literal_type: Some(LiteralType::I64(1)),
                            nullable: false,
                            type_variation_reference: 0,
                        })),
                    })),
                },
            ]
        );
    }

    fn make_extensions_for_fn_tests() -> SimpleExtensions {
        let mut exts = SimpleExtensions::default();
        exts.add_extension_urn("urn".to_string(), 1).unwrap();
        exts.add_extension(ExtensionKind::Function, 1, 1, "equal:any_any".to_string())
            .unwrap();
        exts.add_extension(ExtensionKind::Function, 1, 2, "equal:str_str".to_string())
            .unwrap();
        exts.add_extension(ExtensionKind::Function, 1, 3, "add:i64_i64".to_string())
            .unwrap();
        exts
    }

    #[test]
    fn test_scalar_function_full_compound_name() {
        // Full compound name without anchor
        let exts = make_extensions_for_fn_tests();
        let pair = parse_exact(Rule::function_call, "equal:any_any($0, $1):boolean");
        let f = ScalarFunction::parse_pair(&exts, pair).unwrap();
        assert_eq!(f.function_reference, 1);
        assert_eq!(f.arguments.len(), 2);
        assert!(
            f.output_type.is_some(),
            "output_type must be set after parsing"
        );
    }

    #[test]
    fn test_scalar_function_second_overload() {
        let exts = make_extensions_for_fn_tests();
        let pair = parse_exact(Rule::function_call, "equal:str_str($0, $1):boolean");
        let f = ScalarFunction::parse_pair(&exts, pair).unwrap();

        assert_eq!(f.arguments.len(), 2);
        assert_eq!(f.function_reference, 2);
    }

    #[test]
    fn test_scalar_function_base_name_unique_overload() {
        // "add" has only one overload; base-name lookup should succeed
        let exts = make_extensions_for_fn_tests();
        let pair = parse_exact(Rule::function_call, "add($0, $1):i64");
        let f = ScalarFunction::parse_pair(&exts, pair).unwrap();

        assert_eq!(f.arguments.len(), 2);
        assert_eq!(f.function_reference, 3);
        assert!(
            f.output_type.is_some(),
            "output_type must be set after parsing"
        );
    }

    #[test]
    fn test_scalar_function_base_name_ambiguous_fails() {
        // "equal" has two overloads; base-name lookup should fail
        let exts = make_extensions_for_fn_tests();
        let pair = parse_exact(Rule::function_call, "equal($0, $1):boolean");
        let result = ScalarFunction::parse_pair(&exts, pair);
        assert!(result.is_err(), "ambiguous base name should fail");
    }

    #[test]
    fn test_scalar_function_compound_name_with_anchor() {
        let exts = make_extensions_for_fn_tests();
        let pair = parse_exact(Rule::function_call, "equal:any_any#1($0, $1):boolean");
        let f = ScalarFunction::parse_pair(&exts, pair).unwrap();
        assert_eq!(f.function_reference, 1);
        assert_eq!(f.arguments.len(), 2);
    }

    #[test]
    fn test_scalar_function_base_name_with_anchor() {
        // Base name + explicit anchor should resolve (anchor 1 stores equal:any_any)
        let exts = make_extensions_for_fn_tests();
        let pair = parse_exact(Rule::function_call, "equal#1($0, $1):boolean");
        let f = ScalarFunction::parse_pair(&exts, pair).unwrap();
        assert_eq!(f.function_reference, 1);
        assert_eq!(f.arguments.len(), 2);
    }

    #[test]
    fn test_scalar_function_wrong_name_for_anchor_fails() {
        let exts = make_extensions_for_fn_tests();
        let pair = parse_exact(Rule::function_call, "like#1($0):boolean");
        let result = ScalarFunction::parse_pair(&exts, pair);
        assert!(result.is_err(), "mismatched name/anchor should fail");
    }

    #[test]
    fn test_scalar_function_user_defined_type_in_signature() {
        // u!-prefixed type segments in function signatures parse and resolve.
        let mut exts = SimpleExtensions::default();
        exts.add_extension_urn("urn".to_string(), 1).unwrap();
        exts.add_extension(
            ExtensionKind::Function,
            1,
            10,
            "json_extract_path:u!json_str".to_string(),
        )
        .unwrap();

        let pair = parse_exact(
            Rule::function_call,
            "json_extract_path:u!json_str($0, $1):string",
        );
        let f = ScalarFunction::parse_pair(&exts, pair).unwrap();
        assert_eq!(f.function_reference, 10);
        assert_eq!(f.arguments.len(), 2);
    }

    #[test]
    fn test_scalar_function_missing_type_fails_to_parse() {
        // The grammar requires a type annotation; "add($0, $1)" without ":i64" must fail.
        let result = ExpressionParser::parse(Rule::function_call, "add($0, $1)");
        assert!(
            result.is_err(),
            "function call without type annotation should fail to parse"
        );
    }

    #[test]
    fn test_parse_cast_expression_basic() {
        let extensions = SimpleExtensions::default();
        let pair = parse_exact(Rule::cast_expression, "(78:i32)::i16");
        let result = Cast::parse_pair(&extensions, pair).unwrap();

        // Input should be 78:i32
        let input = result.input.as_ref().unwrap();
        match &input.rex_type {
            Some(RexType::Literal(lit)) => match &lit.literal_type {
                Some(LiteralType::I32(v)) => assert_eq!(*v, 78),
                other => panic!("Expected I32 literal, got: {:?}", other),
            },
            other => panic!("Expected literal, got: {:?}", other),
        }

        // Target type should be i16
        let target = result.r#type.as_ref().unwrap();
        match &target.kind {
            Some(Kind::I16(_)) => {}
            other => panic!("Expected i16 type, got: {:?}", other),
        }

        assert_eq!(result.failure_behavior, 0);
    }

    #[test]
    fn test_parse_cast_expression_via_expression_rule() {
        let extensions = SimpleExtensions::default();
        let pair = parse_exact(Rule::expression, "(78:i32)::i16");
        let result = Expression::parse_pair(&extensions, pair).unwrap();

        match result.rex_type {
            Some(RexType::Cast(_)) => {}
            other => panic!("Expected Cast rex type, got: {:?}", other),
        }
    }

    #[test]
    fn test_parse_cast_expression_nested() {
        let extensions = SimpleExtensions::default();
        let pair = parse_exact(Rule::cast_expression, "((78:i32)::i16)::i32");
        let result = Cast::parse_pair(&extensions, pair).unwrap();

        // Input should itself be a Cast
        let input = result.input.as_ref().unwrap();
        match &input.rex_type {
            Some(RexType::Cast(inner)) => {
                let inner_input = inner.input.as_ref().unwrap();
                match &inner_input.rex_type {
                    Some(RexType::Literal(lit)) => match &lit.literal_type {
                        Some(LiteralType::I32(v)) => assert_eq!(*v, 78),
                        other => panic!("Expected I32 literal, got: {:?}", other),
                    },
                    other => panic!("Expected literal, got: {:?}", other),
                }
            }
            other => panic!("Expected inner Cast, got: {:?}", other),
        }

        match &result.r#type.as_ref().unwrap().kind {
            Some(Kind::I32(_)) => {}
            other => panic!("Expected i32 outer type, got: {:?}", other),
        }
    }

    #[test]
    fn test_parse_cast_expression_with_boolean() {
        let extensions = SimpleExtensions::default();
        let pair = parse_exact(Rule::cast_expression, "(true)::i32");
        let result = Cast::parse_pair(&extensions, pair).unwrap();

        let input = result.input.as_ref().unwrap();
        match &input.rex_type {
            Some(RexType::Literal(lit)) => match &lit.literal_type {
                Some(LiteralType::Boolean(v)) => assert!(*v),
                other => panic!("Expected Boolean literal, got: {:?}", other),
            },
            other => panic!("Expected literal, got: {:?}", other),
        }
    }

    #[test]
    fn test_parse_cast_expression_with_whitespace() {
        let extensions = SimpleExtensions::default();
        // Grammar allows optional whitespace around the expression and ::
        let pair = parse_exact(Rule::cast_expression, "( 78:i32 ) :: i16");
        let result = Cast::parse_pair(&extensions, pair).unwrap();
        assert!(result.input.is_some());
        assert!(result.r#type.is_some());
    }

    #[test]
    fn test_parse_cast_unspecified_failure_behavior() {
        let extensions = SimpleExtensions::default();
        let pair = parse_exact(Rule::cast_expression, "(78:i32)::i16");
        let result = Cast::parse_pair(&extensions, pair).unwrap();
        assert_eq!(
            result.failure_behavior,
            cast::FailureBehavior::Unspecified as i32
        );
    }

    #[test]
    fn test_parse_cast_return_null_failure_behavior() {
        let extensions = SimpleExtensions::default();
        let pair = parse_exact(Rule::cast_expression, "(78:i32)::?i16");
        let result = Cast::parse_pair(&extensions, pair).unwrap();
        assert_eq!(
            result.failure_behavior,
            cast::FailureBehavior::ReturnNull as i32
        );
    }

    #[test]
    fn test_parse_cast_throw_exception_failure_behavior() {
        let extensions = SimpleExtensions::default();
        let pair = parse_exact(Rule::cast_expression, "(78:i32)::!i16");
        let result = Cast::parse_pair(&extensions, pair).unwrap();
        assert_eq!(
            result.failure_behavior,
            cast::FailureBehavior::ThrowException as i32
        );
    }

    #[test]
    fn test_parse_cast_to_user_defined_type_with_u_prefix() {
        // Cast target type is a u!-prefixed UDT; exercises the user_defined_type rule in the cast path.
        let mut extensions = SimpleExtensions::default();
        extensions.add_extension_urn("urn".to_string(), 1).unwrap();
        extensions
            .add_extension(ExtensionKind::Type, 1, 5, "u!json".to_string())
            .unwrap();

        let pair = parse_exact(Rule::cast_expression, "($0)::u!json");
        let result = Cast::parse_pair(&extensions, pair).unwrap();
        match result.r#type.as_ref().unwrap().kind.as_ref().unwrap() {
            Kind::UserDefined(u) => {
                assert_eq!(u.type_reference, 5);
            }
            other => panic!("expected UserDefined, got {other:?}"),
        }
    }

    #[test]
    fn test_function_call_u_prefix_base_name_rejected() {
        // u! is not valid in a function call base name. The grammar's function_signature
        // rule uses `identifier` as the base, which cannot match "u!" + identifier.
        assert!(
            ExpressionParser::parse(Rule::function_call, "u!json_get($0)").is_err(),
            "u! prefix in function call base name must be rejected by the grammar"
        );
    }
}
