use crate::textify::{OutputContext, SimpleExtensions, Textify, TextifyError, unimplemented_err};
use crate::types::ExpectedValue;

use substrait::proto::expression::ScalarFunction;
use substrait::proto::expression::literal::LiteralType;
use substrait::proto::function_argument::ArgType;
use substrait::proto::r#type::{self as ptype, Kind, Nullability};
use substrait::proto::{Expression, FunctionArgument, FunctionOption, expression as expr};

use expr::RexType;

use std::fmt;

// (…) for function call
// […] for variant
// <…> for parameters
// {…} for URI
// !❬…❭
const UNKNOWN_FUNCTION: &str = "!❬unknown_scalar_function❭";

// …::… for cast
// …:::… for specifying type
// &… for enum

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

/// A valid identifier is a sequence of ASCII letters, digits, and underscores,
/// starting with a letter.
///
/// We could expand this at some point to include any valid Unicode identifier
/// (see <https://docs.rs/unicode-ident/latest/unicode_ident/>), but that seems
/// overboard for now.
pub fn is_identifer(s: &str) -> bool {
    let mut chars = s.chars();
    let first = match chars.next() {
        Some(c) => c,
        None => return false,
    };

    if !first.is_ascii_alphabetic() {
        return false;
    }

    for c in chars {
        if !c.is_ascii_alphanumeric() && c != '_' {
            return false;
        }
    }

    true
}

pub fn textify_identifier<W: fmt::Write>(
    s: &str,
    _ctx: &mut OutputContext,
    w: &mut W,
) -> Result<(), TextifyError> {
    if is_identifer(s) {
        write!(w, "{}", s)?;
        return Ok(());
    }

    let escaped = as_escaped_string(s);
    write!(w, "\'{}\'", escaped)?;
    Ok(())
}

/// Write an enum value. Enums are written as `&<identifier>`, if the string is
/// a valid identifier; otherwise, they are written as `&'<escaped_string>'`.
pub fn textify_enum<W: fmt::Write>(
    s: &str,
    ctx: &mut OutputContext,
    w: &mut W,
) -> Result<(), TextifyError> {
    write!(w, "&")?;
    textify_identifier(s, ctx, w)
}

pub fn timestamp_to_string(t: i64) -> String {
    let ts = chrono::DateTime::from_timestamp_nanos(t);
    ts.to_rfc3339()
}

trait Kinded {
    fn kind<E: SimpleExtensions>(&self, ctx: &E) -> Option<Kind>;
}

impl Kinded for LiteralType {
    fn kind<E: SimpleExtensions>(&self, _ctx: &E) -> Option<Kind> {
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
                    Some(expr::literal::interval_day_to_second::PrecisionMode::Microseconds(
                        _m,
                    )) => Some(6),
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
            LiteralType::Struct(_s) => todo!(),
            LiteralType::Map(_m) => todo!(),
            LiteralType::TimestampTz(_t) => todo!(),
            LiteralType::Uuid(_u) => todo!(),
            LiteralType::Null(_n) => todo!(),
            LiteralType::List(_l) => todo!(),
            LiteralType::EmptyList(_l) => todo!(),
            LiteralType::EmptyMap(_l) => todo!(),
            LiteralType::UserDefined(_u) => todo!(),
        }
        Ok(())
    }
}

impl Textify for expr::Literal {
    fn textify<W: fmt::Write>(
        &self,
        ctx: &mut OutputContext,
        w: &mut W,
    ) -> Result<(), TextifyError> {
        ExpectedValue::<LiteralType>::from_option(self.literal_type.as_ref(), "literal_type")
            .textify(ctx, w)
    }
}

impl Textify for ScalarFunction {
    fn textify<W: fmt::Write>(
        &self,
        ctx: &mut OutputContext,
        w: &mut W,
    ) -> Result<(), TextifyError> {
        let ext = ctx.find_function(self.function_reference);
        match (ext, ctx.options().strict) {
            (Some(ref extf), _) => {
                let show_anchor = ctx.options().show_simple_extension_anchors
                    || (ctx.find_functions(&extf.name).count() > 1);
                write!(w, "{}", &extf.name)?;
                if show_anchor {
                    write!(w, "#{}", extf.function_anchor)?;
                }
                if ctx.options().show_extension_uris {
                    write!(w, "@{}", extf.extension_uri_reference)?;
                }
            }
            (None, true) => {
                return Err(TextifyError::InvalidValue {
                    name: "function_not_found".to_string(),
                    context: format!(
                        "Function {} not found in extensions",
                        self.function_reference
                    ),
                });
            }
            (None, false) => {
                write!(w, "{}#{}", UNKNOWN_FUNCTION, self.function_reference)?;
            }
        };

        write!(w, "(")?;
        let mut first = true;
        for arg in self.arguments.iter() {
            if !first {
                write!(w, ", ")?;
                first = false;
            }
            arg.textify(ctx, w)?;
        }

        for opt in self.options.iter() {
            if !first {
                write!(w, ", ")?;
                first = false;
            }
            opt.textify(ctx, w)?;
        }

        write!(w, ")")?;
        Ok(())
    }
}

impl Textify for FunctionOption {
    fn textify<W: fmt::Write>(
        &self,
        _ctx: &mut OutputContext,
        w: &mut W,
    ) -> Result<(), TextifyError> {
        write!(w, "{}⇒[", self.name)?;
        let mut first = true;
        for pref in self.preference.iter() {
            if !first {
                write!(w, ", ")?;
                first = false;
            }
            write!(w, "{}", pref)?;
        }
        write!(w, "]")?;
        Ok(())
    }
}

impl Textify for FunctionArgument {
    fn textify<W: fmt::Write>(
        &self,
        ctx: &mut OutputContext,
        w: &mut W,
    ) -> Result<(), TextifyError> {
        ExpectedValue::from_option(self.arg_type.as_ref(), "argument").textify(ctx, w)
    }
}

impl Textify for ArgType {
    fn textify<W: fmt::Write>(
        &self,
        ctx: &mut OutputContext,
        w: &mut W,
    ) -> Result<(), TextifyError> {
        match self {
            ArgType::Type(t) => t.textify(ctx, w),
            ArgType::Value(v) => v.textify(ctx, w),
            ArgType::Enum(e) => textify_enum(e, ctx, w),
        }
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
            RexType::Selection(_f) => Err(unimplemented_err("FieldReference")),
            RexType::ScalarFunction(s) => s.textify(ctx, w),
            RexType::WindowFunction(_w) => Err(unimplemented_err("WindowFunction")),
            RexType::IfThen(_i) => Err(unimplemented_err("IfThen")),
            RexType::SwitchExpression(_s) => Err(unimplemented_err("SwitchExpression")),
            RexType::SingularOrList(_s) => Err(unimplemented_err("SingularOrList")),
            RexType::MultiOrList(_m) => Err(unimplemented_err("MultiOrList")),
            RexType::Cast(_c) => Err(unimplemented_err("Cast")),
            RexType::Subquery(_s) => Err(unimplemented_err("Subquery")),
            RexType::Nested(_n) => Err(unimplemented_err("Nested")),
            RexType::DynamicParameter(_d) => Err(unimplemented_err("DynamicParameter")),
            RexType::Enum(_) => Err(unimplemented_err("Enum")),
        }
    }
}

impl Textify for Expression {
    fn textify<W: fmt::Write>(
        &self,
        ctx: &mut OutputContext,
        w: &mut W,
    ) -> Result<(), TextifyError> {
        ExpectedValue::from_option(self.rex_type.as_ref(), "expression").textify(ctx, w)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::textify::{OutputContext, OutputOptions, test_ctx};

    use substrait::proto::expression::{self, RexType};

    #[test]
    fn test_literal_textify() {
        let mut ctx = OutputContext::default();
        let mut output = String::new();
        let literal = LiteralType::Boolean(true);
        literal.textify(&mut ctx, &mut output).unwrap();
        assert_eq!(output, "true");
    }

    #[test]
    fn test_expression_textify() {
        let mut ctx = OutputContext::default();
        let mut output = String::new();

        // Test empty expression
        let expr = Expression { rex_type: None };
        expr.textify(&mut ctx, &mut output).unwrap();
        assert_eq!(output, "!❬expression❭");

        // Test literal expression
        output.clear();
        let expr = Expression {
            rex_type: Some(RexType::Literal(expression::Literal {
                nullable: false,
                type_variation_reference: 0,
                literal_type: Some(expression::literal::LiteralType::Boolean(true)),
            })),
        };
        expr.textify(&mut ctx, &mut output).unwrap();
        assert_eq!(output, "true");
    }

    #[test]
    fn test_rextype_textify() {
        let options = OutputOptions {
            ..Default::default()
        };
        let mut ctx = test_ctx(options);
        let mut output = String::new();

        // Test unimplemented types return appropriate errors
        let rex = RexType::Selection(Default::default());
        assert!(matches!(
            rex.textify(&mut ctx, &mut output),
            Err(TextifyError::Unimplemented(_))
        ));

        output = String::new();
        let rex = RexType::WindowFunction(Default::default());
        assert!(matches!(
            rex.textify(&mut ctx, &mut output),
            Err(TextifyError::Unimplemented(_))
        ));

        output = String::new();
        let rex = RexType::ScalarFunction(ScalarFunction {
            function_reference: 1000,
            arguments: vec![],
            options: vec![],
            output_type: None,
            #[allow(deprecated)]
            args: vec![],
        });
        rex.textify(&mut ctx, &mut output).unwrap();
        assert_eq!(output, "!❬unknown_scalar_function❭#1000()");

        output = String::new();
        let rex = RexType::ScalarFunction(ScalarFunction {
            function_reference: 1,
            arguments: vec![],
            options: vec![],
            output_type: None,
            #[allow(deprecated)]
            args: vec![],
        });
        rex.textify(&mut ctx, &mut output).unwrap();
        assert_eq!(output, "first()");

        ctx = test_ctx(OutputOptions {
            show_simple_extension_anchors: false,
            ..Default::default()
        });
        ctx.add_extension_function(1, 231, "duplicated".to_string());
        ctx.add_extension_function(2, 232, "duplicated".to_string());
        output = String::new();
        let rex = RexType::ScalarFunction(ScalarFunction {
            function_reference: 231,
            arguments: vec![FunctionArgument {
                arg_type: Some(ArgType::Value(Expression {
                    rex_type: Some(RexType::Literal(expression::Literal {
                        nullable: false,
                        type_variation_reference: 0,
                        literal_type: Some(expression::literal::LiteralType::Boolean(true)),
                    })),
                })),
            }],
            options: vec![],
            output_type: None,
            #[allow(deprecated)]
            args: vec![],
        });
        rex.textify(&mut ctx, &mut output).unwrap();
        assert_eq!(output, "duplicated#231(true)");
    }
}
