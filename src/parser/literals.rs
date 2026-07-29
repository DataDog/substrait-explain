use chrono::{DateTime, NaiveDate, NaiveDateTime, NaiveTime};
use substrait::proto::Type;
use substrait::proto::expression::Literal;
use substrait::proto::expression::literal::LiteralType;
use substrait::proto::r#type::{Kind, Nullability};

use super::{MessageParseError, Rule, ScopedParsePair, unescape_string};
use crate::extensions::SimpleExtensions;

#[derive(Debug)]
enum LiteralSyntax<'i> {
    Integer {
        source: &'i str,
        span: pest::Span<'i>,
    },
    Float {
        source: &'i str,
        span: pest::Span<'i>,
    },
    Boolean {
        value: bool,
        span: pest::Span<'i>,
    },
    String {
        value: String,
        span: pest::Span<'i>,
    },
    Null {
        span: pest::Span<'i>,
    },
}

impl<'i> LiteralSyntax<'i> {
    fn from_pair(pair: pest::iterators::Pair<'i, Rule>) -> Self {
        match pair.as_rule() {
            Rule::integer => Self::Integer {
                source: pair.as_str(),
                span: pair.as_span(),
            },
            Rule::float => Self::Float {
                source: pair.as_str(),
                span: pair.as_span(),
            },
            Rule::boolean => Self::Boolean {
                value: pair.as_str().parse().unwrap(),
                span: pair.as_span(),
            },
            Rule::string_literal => Self::String {
                value: unescape_string(pair.clone()),
                span: pair.as_span(),
            },
            Rule::null => Self::Null {
                span: pair.as_span(),
            },
            _ => unreachable!("Literal unexpected rule: {:?}", pair.as_rule()),
        }
    }

    fn default_target(&self) -> LiteralTarget {
        match self {
            Self::Integer { .. } => LiteralTarget::I64(LiteralAttrs::required()),
            Self::Float { .. } => LiteralTarget::Fp64(LiteralAttrs::required()),
            Self::Boolean { .. } => LiteralTarget::Boolean(LiteralAttrs::required()),
            Self::String { .. } => LiteralTarget::String(LiteralAttrs::required()),
            Self::Null { span } => {
                unreachable!(
                    "null literal without type should be rejected before this point: {span:?}"
                )
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct LiteralAttrs {
    nullable: bool,
    type_variation_reference: u32,
}

impl LiteralAttrs {
    fn required() -> Self {
        Self {
            nullable: false,
            type_variation_reference: 0,
        }
    }

    fn new(nullability: i32, type_variation_reference: u32) -> Self {
        Self {
            nullable: nullability != Nullability::Required as i32,
            type_variation_reference,
        }
    }
}

#[derive(Debug)]
enum LiteralTarget {
    Boolean(LiteralAttrs),
    I8(LiteralAttrs),
    I16(LiteralAttrs),
    I32(LiteralAttrs),
    I64(LiteralAttrs),
    Fp32(LiteralAttrs),
    Fp64(LiteralAttrs),
    String(LiteralAttrs),
    Date(LiteralAttrs),
    TimeMicros(LiteralAttrs),
    TimestampMicros(LiteralAttrs),
    Null(Type),
    UnsupportedStringFallback,
}

impl LiteralTarget {
    fn from_type<'i>(
        typ: Option<Type>,
        syntax: &LiteralSyntax<'i>,
    ) -> Result<Self, MessageParseError> {
        if let LiteralSyntax::Null { span } = syntax {
            let Some(typ) = typ else {
                return Err(MessageParseError::invalid(
                    "null_literal_type",
                    *span,
                    "Null literals require an explicit type annotation, e.g. null:i64?",
                ));
            };
            return Ok(Self::Null(typ));
        }
        let Some(kind) = typ.and_then(|t| t.kind) else {
            return Ok(syntax.default_target());
        };
        Ok(Self::from_kind(kind))
    }

    fn from_kind(kind: Kind) -> Self {
        match kind {
            Kind::Bool(k) => {
                Self::Boolean(LiteralAttrs::new(k.nullability, k.type_variation_reference))
            }
            Kind::I8(k) => Self::I8(LiteralAttrs::new(k.nullability, k.type_variation_reference)),
            Kind::I16(k) => Self::I16(LiteralAttrs::new(k.nullability, k.type_variation_reference)),
            Kind::I32(k) => Self::I32(LiteralAttrs::new(k.nullability, k.type_variation_reference)),
            Kind::I64(k) => Self::I64(LiteralAttrs::new(k.nullability, k.type_variation_reference)),
            Kind::Fp32(k) => {
                Self::Fp32(LiteralAttrs::new(k.nullability, k.type_variation_reference))
            }
            Kind::Fp64(k) => {
                Self::Fp64(LiteralAttrs::new(k.nullability, k.type_variation_reference))
            }
            Kind::String(k) => {
                Self::String(LiteralAttrs::new(k.nullability, k.type_variation_reference))
            }
            Kind::Date(k) => {
                Self::Date(LiteralAttrs::new(k.nullability, k.type_variation_reference))
            }
            #[allow(deprecated)]
            Kind::Time(k) => {
                Self::TimeMicros(LiteralAttrs::new(k.nullability, k.type_variation_reference))
            }
            #[allow(deprecated)]
            Kind::Timestamp(k) => {
                Self::TimestampMicros(LiteralAttrs::new(k.nullability, k.type_variation_reference))
            }
            other => {
                if matches!(
                    other,
                    Kind::Bool(_)
                        | Kind::I8(_)
                        | Kind::I16(_)
                        | Kind::I32(_)
                        | Kind::I64(_)
                        | Kind::Fp32(_)
                        | Kind::Fp64(_)
                ) {
                    unreachable!("handled scalar literal target")
                }
                Self::UnsupportedStringFallback
            }
        }
    }

    fn parse<'i>(self, syntax: LiteralSyntax<'i>) -> Result<Literal, MessageParseError> {
        match (syntax, self) {
            (LiteralSyntax::Integer { source, span }, target) => target.parse_integer(source, span),
            (LiteralSyntax::Float { source, span }, target) => target.parse_float(source, span),
            (LiteralSyntax::Boolean { value, span }, target) => target.parse_boolean(value, span),
            (LiteralSyntax::String { value, span }, target) => target.parse_string(value, span),
            (LiteralSyntax::Null { span }, target) => target.parse_null(span),
        }
    }

    fn parse_integer(self, source: &str, span: pest::Span) -> Result<Literal, MessageParseError> {
        let value: i64 = source.parse().unwrap();
        match self {
            Self::I8(attrs) => Ok(literal(LiteralType::I8(value as i32), attrs)),
            Self::I16(attrs) => Ok(literal(LiteralType::I16(value as i32), attrs)),
            Self::I32(attrs) => Ok(literal(LiteralType::I32(value as i32), attrs)),
            Self::I64(attrs) => Ok(literal(LiteralType::I64(value), attrs)),
            other => Err(invalid_literal(
                span,
                format!("Invalid type for integer literal: {other:?}"),
            )),
        }
    }

    fn parse_float(self, source: &str, span: pest::Span) -> Result<Literal, MessageParseError> {
        let value: f64 = source.parse().unwrap();
        match self {
            Self::Fp32(attrs) => Ok(literal(LiteralType::Fp32(value as f32), attrs)),
            Self::Fp64(attrs) => Ok(literal(LiteralType::Fp64(value), attrs)),
            other => Err(invalid_literal(
                span,
                format!("Invalid type for float literal: {other:?}"),
            )),
        }
    }

    fn parse_boolean(self, value: bool, span: pest::Span) -> Result<Literal, MessageParseError> {
        match self {
            Self::Boolean(attrs) => Ok(literal(LiteralType::Boolean(value), attrs)),
            other => Err(invalid_literal(
                span,
                format!("Invalid type for boolean literal: {other:?}"),
            )),
        }
    }

    #[allow(deprecated)]
    fn parse_string(self, value: String, span: pest::Span) -> Result<Literal, MessageParseError> {
        match self {
            Self::String(attrs) => Ok(literal(LiteralType::String(value), attrs)),
            Self::Date(attrs) => Ok(literal(
                LiteralType::Date(parse_date_to_days(&value, span)?),
                attrs,
            )),
            Self::TimeMicros(attrs) => Ok(literal(
                LiteralType::Time(parse_time_to_microseconds(&value, span)?),
                attrs,
            )),
            Self::TimestampMicros(attrs) => Ok(literal(
                LiteralType::Timestamp(parse_timestamp_to_microseconds(&value, span)?),
                attrs,
            )),
            Self::UnsupportedStringFallback => Ok(literal(
                LiteralType::String(value),
                LiteralAttrs::required(),
            )),
            other => Err(invalid_literal(
                span,
                format!("Invalid type for string literal: {other:?}"),
            )),
        }
    }

    fn parse_null(self, span: pest::Span) -> Result<Literal, MessageParseError> {
        match self {
            Self::Null(typ) => Ok(Literal {
                literal_type: Some(LiteralType::Null(typ)),
                nullable: false,
                type_variation_reference: 0,
            }),
            other => Err(invalid_literal(
                span,
                format!("Invalid type for null literal: {other:?}"),
            )),
        }
    }
}

fn literal(literal_type: LiteralType, attrs: LiteralAttrs) -> Literal {
    Literal {
        literal_type: Some(literal_type),
        nullable: attrs.nullable,
        type_variation_reference: attrs.type_variation_reference,
    }
}

fn invalid_literal(span: pest::Span, message: impl ToString) -> MessageParseError {
    MessageParseError::invalid("literal", span, message)
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

        let syntax = LiteralSyntax::from_pair(value);
        let typ = match typ {
            Some(t) => Some(Type::parse_pair(extensions, t)?),
            None => None,
        };
        let target = LiteralTarget::from_type(typ, &syntax)?;
        target.parse(syntax)
    }
}
