use std::fmt;

use expr::RexType;
use substrait::proto::expression::field_reference::ReferenceType;
use substrait::proto::expression::literal::LiteralType;
use substrait::proto::expression::{FieldReference, ScalarFunction, reference_segment};
use substrait::proto::function_argument::ArgType;
use substrait::proto::r#type::{self as ptype, Kind, Nullability};
use substrait::proto::{Expression, FunctionArgument, FunctionOption, expression as expr};

use super::{Scope, Textify, TextifyError};
use crate::extensions::ExtensionLookup;
use crate::textify::foundation::MaybeToken;
use crate::textify::types::Anchor;

// …(…) for function call
// […] for variant
// <…> for parameters
// !{…} for missing value

// $… for field reference
// #… for anchor
// @… for URI anchor
// …::… for cast
// …:… for specifying type
// &… for enum

pub fn textify_binary<S: Scope, W: fmt::Write>(items: &[u8], ctx: &S, w: &mut W) -> fmt::Result {
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

pub fn textify_literal_from_string<S: Scope, W: fmt::Write>(
    s: &str,
    t: Kind,
    ctx: &S,
    w: &mut W,
) -> fmt::Result {
    let escaped = as_escaped_string(s);
    write!(w, "\"{}\":", escaped)?;
    t.textify(ctx, w)
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

pub fn textify_identifier<S: Scope, W: fmt::Write>(s: &str, _ctx: &S, w: &mut W) -> fmt::Result {
    if is_identifer(s) {
        write!(w, "{}", s)?;
        return Ok(());
    }

    let escaped = as_escaped_string(s);
    write!(w, "'{}'", escaped)?;
    Ok(())
}

/// Write an enum value. Enums are written as `&<identifier>`, if the string is
/// a valid identifier; otherwise, they are written as `&'<escaped_string>'`.
pub fn textify_enum<S: Scope, W: fmt::Write>(s: &str, ctx: &S, w: &mut W) -> fmt::Result {
    write!(w, "&")?;
    textify_identifier(s, ctx, w)
}

pub fn timestamp_to_string(t: i64) -> String {
    let ts = chrono::DateTime::from_timestamp_nanos(t);
    ts.to_rfc3339()
}

trait Kinded {
    fn kind<E: ExtensionLookup>(&self, ctx: &E) -> Option<Kind>;
}

impl Kinded for LiteralType {
    fn kind<E: ExtensionLookup>(&self, _ctx: &E) -> Option<Kind> {
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
    fn name() -> &'static str {
        "LiteralType"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        match self {
            LiteralType::Boolean(true) => write!(w, "true")?,
            LiteralType::Boolean(false) => write!(w, "false")?,
            LiteralType::I8(i) => write!(w, "{}:i8", i)?,
            LiteralType::I16(i) => write!(w, "{}:i16", i)?,
            LiteralType::I32(i) => write!(w, "{}:i32", i)?,
            // Int64 and Float64 are special cases, they do not need a type suffix
            LiteralType::I64(i) => write!(w, "{}", i)?,
            LiteralType::Fp32(f) => write!(w, "{}:fp32", f)?,
            LiteralType::Fp64(f) => write!(w, "{}", f)?,
            LiteralType::String(s) => write!(w, "\"{}\"", s.escape_default())?,
            LiteralType::Binary(items) => textify_binary(items, ctx, w)?,
            LiteralType::Timestamp(t) => {
                let k = match self.kind(ctx.extensions()) {
                    Some(k) => k,
                    None => {
                        let err = TextifyError::internal(
                            "LiteralType",
                            Some("Timestamp"),
                            format!("No kind found for {:?}", self),
                        );
                        write!(w, "{}", ctx.failure(err))?;
                        return Ok(());
                    }
                };
                let s = timestamp_to_string(*t);
                textify_literal_from_string(&s, k, ctx, w)?
            }
            LiteralType::Date(_) => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(TextifyError::unimplemented(
                        "LiteralType",
                        Some("Date"),
                        "Date literal textification not implemented",
                    ))
                );
            }
            LiteralType::Time(_) => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(TextifyError::unimplemented(
                        "LiteralType",
                        Some("Time"),
                        "Time literal textification not implemented",
                    ))
                );
            }
            LiteralType::IntervalYearToMonth(_i) => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(TextifyError::unimplemented(
                        "LiteralType",
                        Some("IntervalYearToMonth"),
                        "IntervalYearToMonth literal textification not implemented",
                    ))
                );
            }
            LiteralType::IntervalDayToSecond(_i) => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(TextifyError::unimplemented(
                        "LiteralType",
                        Some("IntervalDayToSecond"),
                        "IntervalDayToSecond literal textification not implemented",
                    ))
                );
            }
            LiteralType::IntervalCompound(_i) => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(TextifyError::unimplemented(
                        "LiteralType",
                        Some("IntervalCompound"),
                        "IntervalCompound literal textification not implemented",
                    ))
                );
            }
            LiteralType::FixedChar(_c) => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(TextifyError::unimplemented(
                        "LiteralType",
                        Some("FixedChar"),
                        "FixedChar literal textification not implemented",
                    ))
                );
            }
            LiteralType::VarChar(_c) => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(TextifyError::unimplemented(
                        "LiteralType",
                        Some("VarChar"),
                        "VarChar literal textification not implemented",
                    ))
                );
            }
            LiteralType::FixedBinary(_i) => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(TextifyError::unimplemented(
                        "LiteralType",
                        Some("FixedBinary"),
                        "FixedBinary literal textification not implemented",
                    ))
                );
            }
            LiteralType::Decimal(_d) => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(TextifyError::unimplemented(
                        "LiteralType",
                        Some("Decimal"),
                        "Decimal literal textification not implemented",
                    ))
                );
            }
            LiteralType::PrecisionTime(_p) => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(TextifyError::unimplemented(
                        "LiteralType",
                        Some("PrecisionTime"),
                        "PrecisionTime literal textification not implemented",
                    ))
                );
            }
            LiteralType::PrecisionTimestamp(_p) => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(TextifyError::unimplemented(
                        "LiteralType",
                        Some("PrecisionTimestamp"),
                        "PrecisionTimestamp literal textification not implemented",
                    ))
                );
            }
            LiteralType::PrecisionTimestampTz(_p) => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(TextifyError::unimplemented(
                        "LiteralType",
                        Some("PrecisionTimestampTz"),
                        "PrecisionTimestampTz literal textification not implemented",
                    ))
                );
            }
            LiteralType::Struct(_s) => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(TextifyError::unimplemented(
                        "LiteralType",
                        Some("Struct"),
                        "Struct literal textification not implemented",
                    )),
                );
            }
            LiteralType::Map(_m) => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(TextifyError::unimplemented(
                        "LiteralType",
                        Some("Map"),
                        "Map literal textification not implemented",
                    )),
                );
            }
            LiteralType::TimestampTz(_t) => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(TextifyError::unimplemented(
                        "LiteralType",
                        Some("TimestampTz"),
                        "TimestampTz literal textification not implemented",
                    ))
                );
            }
            LiteralType::Uuid(_u) => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(TextifyError::unimplemented(
                        "LiteralType",
                        Some("Uuid"),
                        "Uuid literal textification not implemented",
                    ))
                );
            }
            LiteralType::Null(_n) => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(TextifyError::unimplemented(
                        "LiteralType",
                        Some("Null"),
                        "Null literal textification not implemented",
                    ))
                );
            }
            LiteralType::List(_l) => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(TextifyError::unimplemented(
                        "LiteralType",
                        Some("List"),
                        "List literal textification not implemented",
                    ))
                );
            }
            LiteralType::EmptyList(_l) => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(TextifyError::unimplemented(
                        "LiteralType",
                        Some("EmptyList"),
                        "EmptyList literal textification not implemented",
                    ))
                );
            }
            LiteralType::EmptyMap(_l) => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(TextifyError::unimplemented(
                        "LiteralType",
                        Some("EmptyMap"),
                        "EmptyMap literal textification not implemented",
                    ))
                );
            }
            LiteralType::UserDefined(_u) => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(TextifyError::unimplemented(
                        "LiteralType",
                        Some("UserDefined"),
                        "UserDefined literal textification not implemented",
                    ))
                );
            }
        }
        Ok(())
    }
}

impl Textify for expr::Literal {
    fn name() -> &'static str {
        "Literal"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        write!(w, "{}", ctx.expect(&self.literal_type))
    }
}

impl Textify for FieldReference {
    fn name() -> &'static str {
        "FieldReference"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        let ref_type = match &self.reference_type {
            None => {
                return write!(
                    w,
                    "{}",
                    TextifyError::invalid(
                        "FieldReference",
                        Some("reference_type"),
                        "Required field missing, None found",
                    )
                );
            }
            Some(ReferenceType::DirectReference(r)) => r,
            _ => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(TextifyError::unimplemented(
                        "FieldReference",
                        Some("FieldReference"),
                        "FieldReference textification implemented only for StructField",
                    ))
                );
            }
        };

        match &ref_type.reference_type {
            None => write!(
                w,
                "{}",
                TextifyError::invalid(
                    "ReferenceSegment",
                    Some("reference_type"),
                    "Required field missing, None found",
                )
            ),
            Some(reference_segment::ReferenceType::StructField(s)) => write!(w, "${}", s.field),
            _ => write!(
                w,
                "{}",
                ctx.failure(TextifyError::unimplemented(
                    "ReferenceSegment",
                    Some("reference_type"),
                    "ReferenceSegment textification implemented only for StructField",
                ))
            ),
        }
    }
}

impl Textify for ScalarFunction {
    fn name() -> &'static str {
        "ScalarFunction"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        let ext_lookup = ctx.extensions();
        let ext = ext_lookup.find_function(self.function_reference);
        let name = MaybeToken(match ext {
            Ok(ref extf) => Ok(extf.name.as_str()),
            Err(e) => Err(ctx.failure(e)),
        });

        let args = ctx.separated(&self.arguments, ", ");
        let options = ctx.separated(&self.options, ", ");
        let between = if self.arguments.is_empty() || self.options.is_empty() {
            ""
        } else {
            ", "
        };
        let anchor = Anchor(self.function_reference);

        write!(w, "{name}{anchor}({args}{between}{options})",)?;
        Ok(())
    }
}

impl Textify for FunctionOption {
    fn name() -> &'static str {
        "FunctionOption"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, _ctx: &S, w: &mut W) -> fmt::Result {
        write!(w, "{}⇒[", self.name)?;
        let mut first = true;
        for pref in self.preference.iter() {
            if !first {
                write!(w, ", ")?;
            } else {
                first = false;
            }
            write!(w, "{}", pref)?;
        }
        write!(w, "]")?;
        Ok(())
    }
}

impl Textify for FunctionArgument {
    fn name() -> &'static str {
        "FunctionArgument"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        write!(w, "{}", ctx.expect(&self.arg_type))
    }
}

impl Textify for ArgType {
    fn name() -> &'static str {
        "ArgType"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        match self {
            ArgType::Type(t) => t.textify(ctx, w),
            ArgType::Value(v) => v.textify(ctx, w),
            ArgType::Enum(e) => textify_enum(e, ctx, w),
        }
    }
}

impl Textify for RexType {
    fn name() -> &'static str {
        "RexType"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        match self {
            RexType::Literal(literal) => literal.textify(ctx, w),
            RexType::Selection(f) => f.textify(ctx, w),
            RexType::ScalarFunction(s) => s.textify(ctx, w),
            RexType::WindowFunction(_w) => write!(
                w,
                "{}",
                ctx.failure(TextifyError::unimplemented(
                    "RexType",
                    Some("WindowFunction"),
                    "WindowFunction textification not implemented",
                ))
            ),
            RexType::IfThen(_i) => write!(
                w,
                "{}",
                ctx.failure(TextifyError::unimplemented(
                    "RexType",
                    Some("IfThen"),
                    "IfThen textification not implemented",
                ))
            ),
            RexType::SwitchExpression(_s) => write!(
                w,
                "{}",
                ctx.failure(TextifyError::unimplemented(
                    "RexType",
                    Some("SwitchExpression"),
                    "SwitchExpression textification not implemented",
                ))
            ),
            RexType::SingularOrList(_s) => write!(
                w,
                "{}",
                ctx.failure(TextifyError::unimplemented(
                    "RexType",
                    Some("SingularOrList"),
                    "SingularOrList textification not implemented",
                ))
            ),
            RexType::MultiOrList(_m) => write!(
                w,
                "{}",
                ctx.failure(TextifyError::unimplemented(
                    "RexType",
                    Some("MultiOrList"),
                    "MultiOrList textification not implemented",
                ))
            ),
            RexType::Cast(_c) => write!(
                w,
                "{}",
                ctx.failure(TextifyError::unimplemented(
                    "RexType",
                    Some("Cast"),
                    "Cast textification not implemented",
                ))
            ),
            RexType::Subquery(_s) => write!(
                w,
                "{}",
                ctx.failure(TextifyError::unimplemented(
                    "RexType",
                    Some("Subquery"),
                    "Subquery textification not implemented",
                ))
            ),
            RexType::Nested(_n) => write!(
                w,
                "{}",
                ctx.failure(TextifyError::unimplemented(
                    "RexType",
                    Some("Nested"),
                    "Nested textification not implemented",
                ))
            ),
            RexType::DynamicParameter(_d) => write!(
                w,
                "{}",
                ctx.failure(TextifyError::unimplemented(
                    "RexType",
                    Some("DynamicParameter"),
                    "DynamicParameter textification not implemented",
                ))
            ),
            RexType::Enum(_) => write!(
                w,
                "{}",
                ctx.failure(TextifyError::unimplemented(
                    "RexType",
                    Some("Enum"),
                    "Enum textification not implemented",
                ))
            ),
        }
    }
}

impl Textify for Expression {
    fn name() -> &'static str {
        "Expression"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        write!(w, "{}", ctx.expect(&self.rex_type))
    }
}

#[cfg(test)]
mod tests {
    use super::super::OutputOptions;
    use super::*;
    use crate::extensions::simple::{ExtensionKind, MissingReference};
    use crate::fixtures::TestContext;
    use crate::textify::foundation::Error;

    #[test]
    fn test_literal_textify() {
        let ctx = TestContext::new();

        let literal = LiteralType::Boolean(true);
        assert_eq!(ctx.textify_no_errors(literal), "true");
    }

    #[test]
    fn test_expression_textify() {
        let ctx = TestContext::new();

        // Test empty expression
        let expr_empty = Expression { rex_type: None }; // Renamed to avoid conflict
        let (s, errs) = ctx.textify(expr_empty);
        assert!(!errs.is_empty());
        assert_eq!(s, "!{RexType}");

        // Test literal expression
        let expr_lit = Expression {
            rex_type: Some(RexType::Literal(expr::Literal {
                nullable: false,
                type_variation_reference: 0,
                literal_type: Some(expr::literal::LiteralType::Boolean(true)),
            })),
        };
        assert_eq!(ctx.textify_no_errors(expr_lit), "true");
    }

    #[test]
    fn test_rextype_textify() {
        let ctx = TestContext::new();

        let func = RexType::ScalarFunction(ScalarFunction {
            function_reference: 1000, // Does not exist
            arguments: vec![],
            options: vec![],
            output_type: None,
            #[allow(deprecated)]
            args: vec![],
        });
        // With strict=false (default in OutputOptions), it should format as unknown
        // If strict=true, it would be an error.
        // Assuming default OutputOptions has strict = false.
        // ScopedContext.options() has strict. Default OutputOptions has strict: false.
        let (s, errq) = ctx.textify(func);
        let errs: Vec<_> = errq.0;
        match errs[0] {
            Error::Lookup(MissingReference::MissingAnchor(k, a)) => {
                assert_eq!(k, ExtensionKind::Function);
                assert_eq!(a, 1000);
            }
            _ => panic!("Expected Lookup MissingAnchor: {}", errs[0]),
        }
        assert_eq!(s, "!{function}#1000()");

        let ctx = ctx.with_uri(1, "first").with_function(1, 100, "first");
        let func = RexType::ScalarFunction(ScalarFunction {
            function_reference: 100,
            arguments: vec![],
            options: vec![],
            output_type: None,
            #[allow(deprecated)]
            args: vec![],
        });
        let s = ctx.textify_no_errors(func);
        assert_eq!(s, "first#100()");

        // Test for duplicated function name requiring anchor
        let options_show_anchor = OutputOptions {
            show_simple_extension_anchors: false, // Will be overridden by duplicate detection
            ..Default::default()
        };
        let ctx = TestContext::new()
            .with_options(options_show_anchor)
            .with_uri(1, "somewhere_on_the_internet")
            .with_uri(2, "somewhere_else")
            .with_function(1, 231, "duplicated")
            .with_function(2, 232, "duplicated");

        let rex_dup = RexType::ScalarFunction(ScalarFunction {
            function_reference: 231,
            arguments: vec![FunctionArgument {
                arg_type: Some(ArgType::Value(Expression {
                    rex_type: Some(RexType::Literal(expr::Literal {
                        nullable: false,
                        type_variation_reference: 0,
                        literal_type: Some(expr::literal::LiteralType::Boolean(true)),
                    })),
                })),
            }],
            options: vec![],
            output_type: None,
            #[allow(deprecated)]
            args: vec![],
        });
        let s = ctx.textify_no_errors(rex_dup);
        assert_eq!(s, "duplicated#231(true)");
    }
}
