use std::fmt::{self};

use chrono::{DateTime, NaiveDate};
use expr::RexType;
use substrait::proto::expression::field_reference::ReferenceType;
use substrait::proto::expression::literal::LiteralType;
use substrait::proto::expression::{
    Cast, FieldReference, IfThen, ReferenceSegment, ScalarFunction, cast, reference_segment,
};
use substrait::proto::function_argument::ArgType;
use substrait::proto::{
    AggregateFunction, Expression, FunctionArgument, FunctionOption, expression as expr,
};

use super::{PlanError, Scope, Textify, Visibility};
use crate::extensions::simple::ExtensionKind;
use crate::textify::types::{Name, NamedAnchor, OutputType, escaped};

// …(…) for function call
// […] for variant
// <…> for parameters
// !{…} for missing value

// $… for field reference
// #… for anchor
// @… for URN anchor
// …::… for cast
// …:… for specifying type
// &… for enum

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

/// Write an error token for a literal type that hasn't been implemented yet.
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

/// Write an enum value. Enums are written as `&<identifier>`, if the string is
/// a valid identifier; otherwise, they are written as `&'<escaped_string>'`.
pub fn textify_enum<S: Scope, W: fmt::Write>(s: &str, _ctx: &S, w: &mut W) -> fmt::Result {
    write!(w, "&{}", Name(s))
}

pub fn timestamp_to_string(t: i64) -> String {
    let ts = chrono::DateTime::from_timestamp_nanos(t);
    ts.to_rfc3339()
}

/// Convert days since Unix epoch to date string
fn days_to_date_string(days: i32) -> String {
    let epoch = NaiveDate::from_ymd_opt(1970, 1, 1).unwrap();
    let date = epoch + chrono::Duration::days(days as i64);
    date.format("%Y-%m-%d").to_string()
}

/// Convert microseconds since midnight to time string
fn microseconds_to_time_string(microseconds: i64) -> String {
    let total_seconds = microseconds / 1_000_000;
    let remaining_microseconds = microseconds % 1_000_000;

    let hours = total_seconds / 3600;
    let minutes = (total_seconds % 3600) / 60;
    let seconds = total_seconds % 60;

    if remaining_microseconds == 0 {
        format!("{hours:02}:{minutes:02}:{seconds:02}")
    } else {
        // Convert microseconds to fractional seconds
        let fractional = remaining_microseconds as f64 / 1_000_000.0;
        format!("{hours:02}:{minutes:02}:{seconds:02}{fractional:.6}")
            .trim_end_matches('0')
            .trim_end_matches('.')
            .to_string()
    }
}

/// Convert microseconds since Unix epoch to timestamp string
fn microseconds_to_timestamp_string(microseconds: i64) -> String {
    let epoch = DateTime::from_timestamp(0, 0).unwrap().naive_utc();
    let duration = chrono::Duration::microseconds(microseconds);
    let datetime = epoch + duration;

    // Format with fractional seconds, then clean up trailing zeros
    let formatted = datetime.format("%Y-%m-%dT%H:%M:%S%.f").to_string();

    // If there are fractional seconds, trim trailing zeros and dot if needed
    if formatted.contains('.') {
        formatted
            .trim_end_matches('0')
            .trim_end_matches('.')
            .to_string()
    } else {
        formatted
    }
}

/// Write just the value portion of a literal, with no type suffix or
/// nullability marker.
///
/// For unimplemented types, writes an error token via `ctx.failure()`.
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
        LiteralType::PrecisionTime(_) => unimplemented_literal("PrecisionTime", ctx, w),
        LiteralType::PrecisionTimestamp(_) => unimplemented_literal("PrecisionTimestamp", ctx, w),
        LiteralType::PrecisionTimestampTz(_) => {
            unimplemented_literal("PrecisionTimestampTz", ctx, w)
        }
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

/// The type suffix for a literal (e.g., `"i32"`, `"fp64"`, `"date"`).
///
/// Returns `None` for unimplemented types whose [`write_literal_value`] already
/// emitted an error token.
fn literal_type_suffix(lit: &LiteralType) -> Option<&'static str> {
    match lit {
        LiteralType::Boolean(_) => Some("boolean"),
        LiteralType::I8(_) => Some("i8"),
        LiteralType::I16(_) => Some("i16"),
        LiteralType::I32(_) => Some("i32"),
        LiteralType::I64(_) => Some("i64"),
        LiteralType::Fp32(_) => Some("fp32"),
        LiteralType::Fp64(_) => Some("fp64"),
        LiteralType::String(_) => Some("string"),
        LiteralType::Binary(_) => Some("binary"),
        LiteralType::Date(_) => Some("date"),
        LiteralType::Time(_) => Some("time"),
        #[allow(deprecated)]
        LiteralType::Timestamp(_) => Some("timestamp"),
        _ => None,
    }
}

/// Whether the type suffix is required for disambiguation.
///
/// Returns `false` for types that are the default for their value syntax:
/// - `boolean` — `true`/`false` are unambiguous
/// - `i64` — bare integers default to i64
/// - `fp64` — bare floats default to fp64
/// - `string` — quoted strings default to string
/// - `binary` — hex format is unambiguous
///
/// All other types (including unknown/future variants) require the suffix.
fn literal_suffix_required(lit: &LiteralType) -> bool {
    !matches!(
        lit,
        LiteralType::Boolean(_)
            | LiteralType::String(_)
            | LiteralType::Binary(_)
            | LiteralType::I64(_)
            | LiteralType::Fp64(_)
    )
}

impl Textify for expr::Literal {
    fn name() -> &'static str {
        "Literal"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        let Some(lit) = self.literal_type.as_ref() else {
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
            Visibility::Required => self.nullable || literal_suffix_required(lit),
        };
        if show_suffix {
            if let Some(suffix) = literal_type_suffix(lit) {
                write!(w, ":{suffix}")?;
            }
            if self.nullable {
                write!(w, "?")?;
            }
        }
        Ok(())
    }
}

pub struct Reference(pub i32);

impl fmt::Display for Reference {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "${}", self.0)
    }
}

impl From<Reference> for Expression {
    fn from(r: Reference) -> Self {
        // XXX: Why is it so many layers to make a struct field reference? This is
        // surprisingly complex
        Expression {
            rex_type: Some(RexType::Selection(Box::new(FieldReference {
                reference_type: Some(ReferenceType::DirectReference(ReferenceSegment {
                    reference_type: Some(reference_segment::ReferenceType::StructField(Box::new(
                        reference_segment::StructField {
                            field: r.0,
                            child: None,
                        },
                    ))),
                })),
                root_type: None,
            }))),
        }
    }
}

impl Textify for Reference {
    fn name() -> &'static str {
        "Reference"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, _ctx: &S, w: &mut W) -> fmt::Result {
        write!(w, "{self}")
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
                    ctx.failure(PlanError::invalid(
                        "FieldReference",
                        Some("reference_type"),
                        "Required field reference_type is missing",
                    ))
                );
            }
            Some(ReferenceType::DirectReference(r)) => r,
            _ => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(PlanError::unimplemented(
                        "FieldReference",
                        Some("FieldReference"),
                        "FieldReference textification implemented only for StructField",
                    ))
                );
            }
        };

        match &ref_type.reference_type {
            Some(reference_segment::ReferenceType::StructField(s)) => {
                write!(w, "{}", Reference(s.field))
            }
            None => write!(
                w,
                "{}",
                ctx.failure(PlanError::invalid(
                    "ReferenceSegment",
                    Some("reference_type"),
                    "Required field reference_type is missing",
                ))
            ),
            _ => write!(
                w,
                "{}",
                ctx.failure(PlanError::unimplemented(
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
        let name_and_anchor =
            NamedAnchor::lookup(ctx, ExtensionKind::Function, self.function_reference);
        let name_and_anchor = ctx.display(&name_and_anchor);

        let args = ctx.separated(&self.arguments, ", ");
        let options = ctx.separated(&self.options, ", ");
        let between = if self.arguments.is_empty() || self.options.is_empty() {
            ""
        } else {
            ", "
        };

        let output = OutputType(self.output_type.as_ref());
        let output_type = ctx.optional(&output, ctx.options().fn_types);

        write!(
            w,
            "{name_and_anchor}({args}{between}{options}){output_type}"
        )?;
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
            write!(w, "{pref}")?;
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
        write!(w, "{}", ctx.expect(self.arg_type.as_ref()))
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

impl Textify for Cast {
    fn name() -> &'static str {
        "Cast"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        let failure_err;
        let fb: &dyn fmt::Display = match cast::FailureBehavior::try_from(self.failure_behavior) {
            Ok(cast::FailureBehavior::Unspecified) => &"",
            Ok(cast::FailureBehavior::ReturnNull) => &"?",
            Ok(cast::FailureBehavior::ThrowException) => &"!",
            Err(_) => {
                failure_err = ctx.failure(PlanError::invalid(
                    "Cast",
                    Some("failure_behavior"),
                    format!("Unknown failure_behavior value: {}", self.failure_behavior),
                ));
                &failure_err
            }
        };
        let input = ctx.expect(self.input.as_deref());
        let target_type = ctx.expect(self.r#type.as_ref());
        write!(w, "({input})::{fb}{target_type}")
    }
}

impl Textify for IfThen {
    fn name() -> &'static str {
        "IfThen"
    }

    // This method writes ifThen using the following convention of a comma separated sequence of 'if_clause -> then_clause, '
    // followed by the final else clause denoted with '_'
    // ex: true -> if_then(true || false -> true, _ -> false)
    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        write!(w, "if_then(")?;
        for clause in &self.ifs {
            let if_expr = ctx.expect(clause.r#if.as_ref());
            let then_expr = ctx.expect(clause.then.as_ref());
            write!(w, "{if_expr} -> {then_expr}, ")?;
        }
        let else_expr = ctx.expect(self.r#else.as_deref());
        write!(w, "_ -> {else_expr})")
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
                ctx.failure(PlanError::unimplemented(
                    "RexType",
                    Some("WindowFunction"),
                    "WindowFunction textification not implemented",
                ))
            ),
            RexType::IfThen(i) => i.textify(ctx, w),
            RexType::SwitchExpression(_s) => write!(
                w,
                "{}",
                ctx.failure(PlanError::unimplemented(
                    "RexType",
                    Some("SwitchExpression"),
                    "SwitchExpression textification not implemented",
                ))
            ),
            RexType::SingularOrList(_s) => write!(
                w,
                "{}",
                ctx.failure(PlanError::unimplemented(
                    "RexType",
                    Some("SingularOrList"),
                    "SingularOrList textification not implemented",
                ))
            ),
            RexType::MultiOrList(_m) => write!(
                w,
                "{}",
                ctx.failure(PlanError::unimplemented(
                    "RexType",
                    Some("MultiOrList"),
                    "MultiOrList textification not implemented",
                ))
            ),
            RexType::Cast(c) => c.textify(ctx, w),
            RexType::Subquery(_s) => write!(
                w,
                "{}",
                ctx.failure(PlanError::unimplemented(
                    "RexType",
                    Some("Subquery"),
                    "Subquery textification not implemented",
                ))
            ),
            RexType::Nested(_n) => write!(
                w,
                "{}",
                ctx.failure(PlanError::unimplemented(
                    "RexType",
                    Some("Nested"),
                    "Nested textification not implemented",
                ))
            ),
            RexType::DynamicParameter(_d) => write!(
                w,
                "{}",
                ctx.failure(PlanError::unimplemented(
                    "RexType",
                    Some("DynamicParameter"),
                    "DynamicParameter textification not implemented",
                ))
            ),
            #[allow(deprecated)]
            RexType::Enum(_) => write!(
                w,
                "{}",
                ctx.failure(PlanError::unimplemented(
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
        write!(w, "{}", ctx.expect(self.rex_type.as_ref()))
    }
}

impl Textify for AggregateFunction {
    fn name() -> &'static str {
        "AggregateFunction"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        // Similar to ScalarFunction textification
        let name_and_anchor =
            NamedAnchor::lookup(ctx, ExtensionKind::Function, self.function_reference);
        let name_and_anchor = ctx.display(&name_and_anchor);

        let args = ctx.separated(&self.arguments, ", ");
        let options = ctx.separated(&self.options, ", ");
        let between = if self.arguments.is_empty() || self.options.is_empty() {
            ""
        } else {
            ", "
        };

        let output = OutputType(self.output_type.as_ref());
        let output_type = ctx.optional(&output, ctx.options().fn_types);

        write!(
            w,
            "{name_and_anchor}({args}{between}{options}){output_type}"
        )
    }
}

#[cfg(test)]
mod tests {
    use substrait::proto::Type;
    use substrait::proto::expression::{cast, if_then};
    use substrait::proto::r#type::{I16, I32, Kind, Nullability};

    use super::*;
    use crate::extensions::simple::{ExtensionKind, MissingReference};
    use crate::fixtures::TestContext;
    use crate::textify::foundation::{FormatError, FormatErrorType};

    fn literal_bool(value: bool) -> Expression {
        Expression {
            rex_type: Some(RexType::Literal(expr::Literal {
                nullable: false,
                type_variation_reference: 0,
                literal_type: Some(expr::literal::LiteralType::Boolean(value)),
            })),
        }
    }

    fn non_nullable_literal(lit: expr::literal::LiteralType) -> expr::Literal {
        expr::Literal {
            nullable: false,
            type_variation_reference: 0,
            literal_type: Some(lit),
        }
    }

    #[test]
    fn test_literal_textify() {
        let ctx = TestContext::new();

        let literal = non_nullable_literal(LiteralType::Boolean(true));
        assert_eq!(ctx.textify_no_errors(&literal), "true");
    }

    fn nullable_literal(lit: expr::literal::LiteralType) -> expr::Literal {
        expr::Literal {
            nullable: true,
            type_variation_reference: 0,
            literal_type: Some(lit),
        }
    }

    #[test]
    fn test_nullable_boolean_literal_textify() {
        let ctx = TestContext::new();
        assert_eq!(
            ctx.textify_no_errors(&nullable_literal(expr::literal::LiteralType::Boolean(true))),
            "true:boolean?"
        );
        assert_eq!(
            ctx.textify_no_errors(&nullable_literal(expr::literal::LiteralType::Boolean(
                false
            ))),
            "false:boolean?"
        );
    }

    #[test]
    fn test_nullable_integer_literal_textify() {
        let ctx = TestContext::new();
        assert_eq!(
            ctx.textify_no_errors(&nullable_literal(expr::literal::LiteralType::I32(78))),
            "78:i32?"
        );
        assert_eq!(
            ctx.textify_no_errors(&nullable_literal(expr::literal::LiteralType::I64(42))),
            "42:i64?"
        );
    }

    #[test]
    fn test_nullable_float_literal_textify() {
        let ctx = TestContext::new();
        assert_eq!(
            ctx.textify_no_errors(&nullable_literal(expr::literal::LiteralType::Fp32(2.5))),
            "2.5:fp32?"
        );
        assert_eq!(
            ctx.textify_no_errors(&nullable_literal(expr::literal::LiteralType::Fp64(3.19))),
            "3.19:fp64?"
        );
    }

    #[test]
    fn test_expression_textify() {
        let ctx = TestContext::new();

        // Test empty expression
        let expr_empty = Expression { rex_type: None }; // Renamed to avoid conflict
        let (s, errs) = ctx.textify(&expr_empty);
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
        assert_eq!(ctx.textify_no_errors(&expr_lit), "true");
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
        let (s, errq) = ctx.textify(&func);
        let errs: Vec<_> = errq.0;
        match errs[0] {
            FormatError::Lookup(MissingReference::MissingAnchor(k, a)) => {
                assert_eq!(k, ExtensionKind::Function);
                assert_eq!(a, 1000);
            }
            _ => panic!("Expected Lookup MissingAnchor: {}", errs[0]),
        }
        assert_eq!(s, "!{function}#1000()");

        let ctx = ctx.with_urn(1, "first").with_function(1, 100, "first");
        let func = RexType::ScalarFunction(ScalarFunction {
            function_reference: 100,
            arguments: vec![],
            options: vec![],
            output_type: None,
            #[allow(deprecated)]
            args: vec![],
        });
        let s = ctx.textify_no_errors(&func);
        assert_eq!(s, "first()");

        // Test for duplicated function name requiring anchor
        let options_show_anchor = Default::default();

        let ctx = TestContext::new()
            .with_options(options_show_anchor)
            .with_urn(1, "somewhere_on_the_internet")
            .with_urn(2, "somewhere_else")
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
        let s = ctx.textify_no_errors(&rex_dup);
        assert_eq!(s, "duplicated#231(true)");
    }

    #[test]
    fn test_ifthen_textify() {
        let ctx = TestContext::new();

        let if_then = IfThen {
            ifs: vec![
                if_then::IfClause {
                    r#if: Some(literal_bool(true)),
                    then: Some(literal_bool(false)),
                },
                if_then::IfClause {
                    r#if: Some(literal_bool(false)),
                    then: Some(literal_bool(true)),
                },
            ],
            r#else: Some(Box::new(literal_bool(true))),
        };

        let s = ctx.textify_no_errors(&if_then);
        assert_eq!(s, "if_then(true -> false, false -> true, _ -> true)");
    }

    #[test]
    fn test_ifthen_textify_missing_else() {
        let ctx = TestContext::new();

        let if_then = IfThen {
            ifs: vec![if_then::IfClause {
                r#if: Some(literal_bool(true)),
                then: Some(literal_bool(false)),
            }],
            r#else: None,
        };

        let (s, errs) = ctx.textify(&if_then);
        assert_eq!(s, "if_then(true -> false, _ -> !{Expression})");
        assert_eq!(errs.0.len(), 1);
    }

    fn make_i32_type() -> Type {
        Type {
            kind: Some(Kind::I32(I32 {
                nullability: Nullability::Required as i32,
                type_variation_reference: 0,
            })),
        }
    }

    fn make_i16_type() -> Type {
        Type {
            kind: Some(Kind::I16(I16 {
                nullability: Nullability::Required as i32,
                type_variation_reference: 0,
            })),
        }
    }

    fn literal_i32(value: i32) -> Expression {
        Expression {
            rex_type: Some(RexType::Literal(expr::Literal {
                nullable: false,
                type_variation_reference: 0,
                literal_type: Some(expr::literal::LiteralType::I32(value)),
            })),
        }
    }

    #[test]
    fn test_cast_textify() {
        let ctx = TestContext::new();
        let cast = Cast {
            r#type: Some(make_i16_type()),
            input: Some(Box::new(literal_i32(78))),
            failure_behavior: 0,
        };
        assert_eq!(ctx.textify_no_errors(&cast), "(78:i32)::i16");
    }

    #[test]
    fn test_cast_textify_via_rextype() {
        let ctx = TestContext::new();
        let rex = RexType::Cast(Box::new(Cast {
            r#type: Some(make_i16_type()),
            input: Some(Box::new(literal_i32(78))),
            failure_behavior: 0,
        }));
        assert_eq!(ctx.textify_no_errors(&rex), "(78:i32)::i16");
    }

    #[test]
    fn test_cast_textify_nested() {
        // ((78:i32)::i16)::i32 — cast of a cast
        let ctx = TestContext::new();
        let inner_cast = Expression {
            rex_type: Some(RexType::Cast(Box::new(Cast {
                r#type: Some(make_i16_type()),
                input: Some(Box::new(literal_i32(78))),
                failure_behavior: 0,
            }))),
        };
        let outer_cast = Cast {
            r#type: Some(make_i32_type()),
            input: Some(Box::new(inner_cast)),
            failure_behavior: 0,
        };
        assert_eq!(ctx.textify_no_errors(&outer_cast), "((78:i32)::i16)::i32");
    }

    #[test]
    fn test_cast_textify_return_null() {
        let ctx = TestContext::new();
        let cast = Cast {
            r#type: Some(make_i16_type()),
            input: Some(Box::new(literal_i32(78))),
            failure_behavior: cast::FailureBehavior::ReturnNull as i32,
        };
        assert_eq!(ctx.textify_no_errors(&cast), "(78:i32)::?i16");
    }

    #[test]
    fn test_cast_textify_throw_exception() {
        let ctx = TestContext::new();
        let cast = Cast {
            r#type: Some(make_i16_type()),
            input: Some(Box::new(literal_i32(78))),
            failure_behavior: cast::FailureBehavior::ThrowException as i32,
        };
        assert_eq!(ctx.textify_no_errors(&cast), "(78:i32)::!i16");
    }

    #[test]
    fn test_cast_textify_missing_input() {
        let ctx = TestContext::new();
        let cast = Cast {
            r#type: Some(make_i16_type()),
            input: None,
            failure_behavior: 0,
        };
        let (s, errs) = ctx.textify(&cast);
        assert_eq!(s, "(!{Expression})::i16");
        match &errs.0[0] {
            FormatError::Format(e) => {
                assert_eq!(e.message, "Expression");
                assert_eq!(e.error_type, FormatErrorType::InvalidValue);
            }
            other => panic!("Expected Format(InvalidValue) for missing input, got: {other}"),
        }
    }

    #[test]
    fn test_cast_textify_missing_type() {
        let ctx = TestContext::new();
        let cast = Cast {
            r#type: None,
            input: Some(Box::new(literal_i32(78))),
            failure_behavior: 0,
        };
        let (s, errs) = ctx.textify(&cast);
        assert_eq!(s, "(78:i32)::!{Type}");
        match &errs.0[0] {
            FormatError::Format(e) => {
                assert_eq!(e.message, "Type");
                assert_eq!(e.error_type, FormatErrorType::InvalidValue);
            }
            other => panic!("Expected Format(InvalidValue) for missing type, got: {other}"),
        }
    }

    #[test]
    fn test_cast_textify_invalid_failure_behavior() {
        let ctx = TestContext::new();
        let cast = Cast {
            r#type: Some(make_i16_type()),
            input: Some(Box::new(literal_i32(78))),
            failure_behavior: 99,
        };
        let (s, errs) = ctx.textify(&cast);
        // Error token is embedded inline — input and type are still written
        assert_eq!(s, "(78:i32)::!{Cast}i16");
        match &errs.0[0] {
            FormatError::Format(e) => {
                assert_eq!(e.message, "Cast");
                assert_eq!(e.error_type, FormatErrorType::InvalidValue);
            }
            other => {
                panic!("Expected Format(InvalidValue) for invalid failure_behavior, got: {other}")
            }
        }
    }
}
