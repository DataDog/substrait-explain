use crate::textify::{OutputContext, Textify, TextifyError};
use crate::types::ExpectedValue;

use substrait::proto::expression as expr;
use substrait::proto::expression::literal::LiteralType;

use expr::RexType;

use std::fmt;

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
            LiteralType::Timestamp(_) => todo!(),
            LiteralType::Date(_) => todo!(),
            LiteralType::Time(_) => todo!(),
            LiteralType::IntervalYearToMonth(interval_year_to_month) => todo!(),
            LiteralType::IntervalDayToSecond(interval_day_to_second) => todo!(),
            LiteralType::IntervalCompound(interval_compound) => todo!(),
            LiteralType::FixedChar(_) => todo!(),
            LiteralType::VarChar(var_char) => todo!(),
            LiteralType::FixedBinary(items) => todo!(),
            LiteralType::Decimal(decimal) => todo!(),
            LiteralType::PrecisionTime(precision_time) => todo!(),
            LiteralType::PrecisionTimestamp(precision_timestamp) => todo!(),
            LiteralType::PrecisionTimestampTz(precision_timestamp) => todo!(),
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
