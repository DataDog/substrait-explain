use crate::textify::{OutputContext, SimpleExtensions, Textify, TextifyError};
use crate::types::ExpectedValue;

use substrait::proto::expression::literal::LiteralType;
use substrait::proto::r#type::{self as ptype, Kind, Nullability, Timestamp};
use substrait::proto::{Type, expression as expr};

use expr::RexType;

use std::{fmt, io};

pub fn textify_binary<W: fmt::Write>(
    items: &[u8],
    ctx: &OutputContext,
    w: &mut W,
) -> Result<(), TextifyError> {
    if ctx.options().show_literal_binaries {
        write!(w, "0x")?;
        for &n in items {
            write!(w, "{:02x}", n)?;
        }
    } else {
        write!(w, "{{binary}}")?;
    }
    Ok(())
}

/// Escapes a string for use in a literal.
pub fn as_escaped_string(s: &str) -> String {
    // This uses Rust escaping, is that fine?
    format!(
        "\"{}\"",
        s.chars().flat_map(|c| c.escape_debug()).collect::<String>()
    )
}

pub fn textify_literal_from_string<W: fmt::Write>(
    s: &str,
    t: Kind,
    ctx: &mut OutputContext,
    w: &mut W,
) -> Result<(), TextifyError> {
    let escaped = as_escaped_string(s);
    write!(w, "\"{}\":::", escaped)?;
    t.textify(ctx, w)?;
    Ok(())
}

pub fn timestamp_to_string(t: i64) -> String {
    let ts = chrono::DateTime::from_timestamp_nanos(t);
    ts.to_rfc3339()
}

trait Kinded {
    fn kind<E: SimpleExtensions>(&self, ctx: &E) -> Option<Kind>;
}

trait Typed: Kinded {
    fn type_lookup<E: SimpleExtensions>(&self, ctx: &E) -> Option<Type>;
}

impl Kinded for LiteralType {
    fn kind<E: SimpleExtensions>(&self, ctx: &E) -> Option<Kind> {
        match self {
            LiteralType::Boolean(_) => Some(Kind::Bool(ptype::Boolean {
                type_variation_reference: 0,
                nullability: Nullability::Required.into(),
            })),
            LiteralType::I8(_) => Some(Kind::I8(ptype::I8 {
                type_variation_reference: 0,
                nullability: Nullability::Required.into(),
            })),
            LiteralType::I16(_) => Some(Kind::I16(ptype::I16 {
                type_variation_reference: 0,
                nullability: Nullability::Required.into(),
            })),
            LiteralType::I32(_) => Some(Kind::I32(ptype::I32 {
                type_variation_reference: 0,
                nullability: Nullability::Required.into(),
            })),
            LiteralType::I64(_) => Some(Kind::I64(ptype::I64 {
                type_variation_reference: 0,
                nullability: Nullability::Required.into(),
            })),
            LiteralType::Fp32(_) => Some(Kind::Fp32(ptype::Fp32 {
                type_variation_reference: 0,
                nullability: Nullability::Required.into(),
            })),
            LiteralType::Fp64(_) => Some(Kind::Fp64(ptype::Fp64 {
                type_variation_reference: 0,
                nullability: Nullability::Required.into(),
            })),
            LiteralType::String(_) => Some(Kind::String(ptype::String {
                type_variation_reference: 0,
                nullability: Nullability::Required.into(),
            })),
            LiteralType::Binary(_) => Some(Kind::Binary(ptype::Binary {
                type_variation_reference: 0,
                nullability: Nullability::Required.into(),
            })),
            LiteralType::Timestamp(_) => Some(Kind::Timestamp(ptype::Timestamp {
                type_variation_reference: 0,
                nullability: Nullability::Required.into(),
            })),
            LiteralType::Date(_) => Some(Kind::Date(ptype::Date {
                type_variation_reference: 0,
                nullability: Nullability::Required.into(),
            })),
            LiteralType::Time(_) => Some(Kind::Time(ptype::Time {
                type_variation_reference: 0,
                nullability: Nullability::Required.into(),
            })),
            LiteralType::IntervalYearToMonth(_) => Some(Kind::IntervalYear(ptype::IntervalYear {
                type_variation_reference: 0,
                nullability: Nullability::Required.into(),
            })),
            LiteralType::IntervalDayToSecond(i) => {
                let precision = match i.precision_mode {
                    Some(expr::literal::interval_day_to_second::PrecisionMode::Microseconds(m)) => {
                        Some(6)
                    }
                    Some(expr::literal::interval_day_to_second::PrecisionMode::Precision(p)) => {
                        Some(p)
                    }
                    // Unset precision is 0; protobuf defaults to 0
                    None => None,
                };

                Some(Kind::IntervalDay(ptype::IntervalDay {
                    type_variation_reference: 0,
                    nullability: Nullability::Required.into(),
                    precision,
                }))
            }
            LiteralType::IntervalCompound(_) => todo!(),
            LiteralType::FixedChar(_) => todo!(),
            LiteralType::VarChar(_c) => todo!(),
            LiteralType::FixedBinary(_b) => todo!(),
            LiteralType::Decimal(_d) => todo!(),
            LiteralType::PrecisionTime(_t) => todo!(),
            LiteralType::PrecisionTimestamp(t) => {
                Some(Kind::PrecisionTimestamp(ptype::PrecisionTimestamp {
                    type_variation_reference: 0,
                    nullability: Nullability::Required.into(),
                    precision: t.precision,
                }))
            }
            LiteralType::PrecisionTimestampTz(_t) => todo!(),
            LiteralType::Struct(_s) => todo!(),
            LiteralType::Map(_m) => todo!(),
            LiteralType::TimestampTz(_t) => todo!(),
            LiteralType::Uuid(_u) => todo!(),
            LiteralType::Null(_n) => todo!(),
            LiteralType::List(_l) => todo!(),
            LiteralType::EmptyList(_l) => todo!(),
            LiteralType::EmptyMap(_m) => todo!(),
            LiteralType::UserDefined(_u) => todo!(),
        }
    }
}

impl Textify for LiteralType {
    fn textify<W: fmt::Write>(
        &self,
        ctx: &mut OutputContext,
        w: &mut W,
    ) -> Result<(), TextifyError> {
        match self {
            LiteralType::Boolean(true) => write!(w, "true")?,
            LiteralType::Boolean(false) => write!(w, "false")?,
            LiteralType::I8(i) => write!(w, "{}", i)?,
            LiteralType::I16(i) => write!(w, "{}", i)?,
            LiteralType::I32(i) => write!(w, "{}", i)?,
            LiteralType::I64(i) => write!(w, "{}", i)?,
            LiteralType::Fp32(f) => write!(w, "{}", f)?,
            LiteralType::Fp64(f) => write!(w, "{}", f)?,
            LiteralType::String(s) => write!(w, "\"{}\"", s.escape_default())?,
            LiteralType::Binary(items) => textify_binary(items, ctx, w)?,
            LiteralType::Timestamp(t) => {
                let k = match self.kind(ctx) {
                    Some(k) => k,
                    None => {
                        return Err(TextifyError::Internal(format!(
                            "No kind found for {:?}",
                            self
                        )));
                    }
                };
                let s = timestamp_to_string(*t);
                textify_literal_from_string(&s, k, ctx, w)?
            }
            LiteralType::Date(_) => todo!(),
            LiteralType::Time(_) => todo!(),
            LiteralType::IntervalYearToMonth(_i) => todo!(),
            LiteralType::IntervalDayToSecond(_i) => todo!(),
            LiteralType::IntervalCompound(_i) => todo!(),
            LiteralType::FixedChar(_c) => todo!(),
            LiteralType::VarChar(_c) => todo!(),
            LiteralType::FixedBinary(_i) => todo!(),
            LiteralType::Decimal(_d) => todo!(),
            LiteralType::PrecisionTime(_p) => todo!(),
            LiteralType::PrecisionTimestamp(_p) => todo!(),
            LiteralType::PrecisionTimestampTz(_p) => todo!(),
            LiteralType::Struct(_) => todo!(),
            LiteralType::Map(map) => todo!(),
            LiteralType::TimestampTz(_) => todo!(),
            LiteralType::Uuid(items) => todo!(),
            LiteralType::Null(_) => todo!(),
            LiteralType::List(list) => todo!(),
            LiteralType::EmptyList(list) => todo!(),
            LiteralType::EmptyMap(map) => todo!(),
            LiteralType::UserDefined(user_defined) => todo!(),
        }

        write!(w, "Literal")?;
        Ok(())
    }
}

impl Textify for expr::Literal {
    fn textify<W: fmt::Write>(
        &self,
        ctx: &mut OutputContext,
        w: &mut W,
    ) -> Result<(), TextifyError> {
        ExpectedValue::from_option(self.literal_type.clone(), "literal_type").textify(ctx, w)
    }
}

impl Textify for RexType {
    fn textify<W: fmt::Write>(
        &self,
        ctx: &mut OutputContext,
        w: &mut W,
    ) -> Result<(), TextifyError> {
        match self {
            RexType::Literal(literal) => literal.textify(ctx, w),
            // Expression::Literal(literal) => literal.textify(ctx, w),
            _ => todo!(),
        }
    }
}
