use std::borrow::Cow;
use std::fmt::{self};

use chrono::{DateTime, NaiveDate, NaiveTime};
use expr::RexType;
use substrait::proto::expression::field_reference::{ReferenceType, RootReference, RootType};
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PrecisionFormatError {
    /// `precision` isn't one of the precisions literals support (0, 3, 6, 9, 12).
    UnsupportedPrecision,
    /// `value` can't be represented at `precision`: it overflows the
    /// representable range (timestamps) or falls outside a single day (times).
    OutOfRange,
}

impl PrecisionFormatError {
    fn into_plan_error(self, variant: &'static str, precision: i32) -> PlanError {
        let message = match self {
            PrecisionFormatError::UnsupportedPrecision => {
                format!("unsupported precision {precision} for {variant}")
            }
            PrecisionFormatError::OutOfRange => {
                format!("value is out of range for {variant} at precision {precision}")
            }
        };
        PlanError::invalid("LiteralType", Some(variant), message)
    }
}

/// Returns the diagnostic for truncating a picosecond value to nanoseconds.
fn picosecond_truncation_warning(variant: &'static str) -> PlanError {
    PlanError::invalid(
        variant,
        Some("value"),
        "precision 12 (picoseconds) truncated to nanoseconds; sub-nanosecond precision lost",
    )
}

fn write_precision_literal<S: Scope, W: fmt::Write>(
    variant: &'static str,
    precision: i32,
    formatted: Result<String, PrecisionFormatError>,
    ctx: &S,
    w: &mut W,
) -> fmt::Result {
    match formatted {
        Ok(s) => {
            if precision == 12 {
                ctx.push_error(picosecond_truncation_warning(variant).into());
            }
            write!(w, "'{}'", escaped(&s))
        }
        Err(e) => write!(w, "{}", ctx.failure(e.into_plan_error(variant, precision))),
    }
}

/// Write an enum value. Enums are written as `&<identifier>`, if the string is
/// a valid identifier; otherwise, they are written as `&'<escaped_string>'`.
pub fn textify_enum<S: Scope, W: fmt::Write>(s: &str, _ctx: &S, w: &mut W) -> fmt::Result {
    write!(w, "&{}", Name(s))
}

/// Convert days since Unix epoch to date string
fn days_to_date_string(days: i32) -> String {
    let epoch = NaiveDate::from_ymd_opt(1970, 1, 1).unwrap();
    let date = epoch + chrono::Duration::days(days as i64);
    date.format("%Y-%m-%d").to_string()
}

/// Convert a value in `precision` units, to a `chrono::Duration`.
/// Precision 12 (picoseconds) is truncated to nanoseconds: chrono can't represent sub-nanosecond resolution.
fn duration_from_precision_units(
    value: i64,
    precision: i32,
) -> Result<chrono::Duration, PrecisionFormatError> {
    match precision {
        0 => chrono::Duration::try_seconds(value).ok_or(PrecisionFormatError::OutOfRange),
        3 => chrono::Duration::try_milliseconds(value).ok_or(PrecisionFormatError::OutOfRange),
        6 => Ok(chrono::Duration::microseconds(value)),
        9 => Ok(chrono::Duration::nanoseconds(value)),
        12 => Ok(chrono::Duration::nanoseconds(value / 1000)),
        _ => Err(PrecisionFormatError::UnsupportedPrecision),
    }
}

/// Convert a value in precision units since the Unix epoch, to a timestamp string.
/// Errors if `value` is out of chrono's representable date range.
fn precision_timestamp_to_string(
    value: i64,
    precision: i32,
) -> Result<String, PrecisionFormatError> {
    let duration = duration_from_precision_units(value, precision)?;
    let epoch = DateTime::from_timestamp(0, 0).unwrap().naive_utc();
    let datetime = epoch
        .checked_add_signed(duration)
        .ok_or(PrecisionFormatError::OutOfRange)?;

    let formatted = datetime.format("%Y-%m-%dT%H:%M:%S%.f").to_string();
    Ok(if formatted.contains('.') {
        formatted
            .trim_end_matches('0')
            .trim_end_matches('.')
            .to_string()
    } else {
        formatted
    })
}

/// Convert a value in precision units since midnight, to a time-of-day string.
/// Errors if `value` falls outside a single day: `NaiveTime + Duration` wraps
/// modulo 24 hours, which would otherwise silently misrepresent the value.
///
/// The sign check is on `value` itself, not the `chrono::Duration` derived from
/// it: at precision 12, `duration_from_precision_units` truncates towards zero,
/// so a small negative `value` (e.g. `-1`) would otherwise round to a
/// zero/non-negative duration and be wrongly accepted.
fn precision_time_to_string(value: i64, precision: i32) -> Result<String, PrecisionFormatError> {
    if value < 0 {
        return Err(PrecisionFormatError::OutOfRange);
    }
    let duration = duration_from_precision_units(value, precision)?;
    if duration >= chrono::Duration::days(1) {
        return Err(PrecisionFormatError::OutOfRange);
    }
    let midnight = NaiveTime::from_hms_opt(0, 0, 0).unwrap();
    let time = midnight + duration;

    let formatted = time.format("%H:%M:%S%.f").to_string();
    Ok(if formatted.contains('.') {
        formatted
            .trim_end_matches('0')
            .trim_end_matches('.')
            .to_string()
    } else {
        formatted
    })
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
        #[allow(deprecated)]
        LiteralType::Time(microseconds) => write_precision_literal(
            "Time",
            6,
            precision_time_to_string(*microseconds, 6),
            ctx,
            w,
        ),
        #[allow(deprecated)]
        LiteralType::Timestamp(microseconds) => write_precision_literal(
            "Timestamp",
            6,
            precision_timestamp_to_string(*microseconds, 6),
            ctx,
            w,
        ),
        LiteralType::IntervalYearToMonth(_) => unimplemented_literal("IntervalYearToMonth", ctx, w),
        LiteralType::IntervalDayToSecond(_) => unimplemented_literal("IntervalDayToSecond", ctx, w),
        LiteralType::IntervalCompound(_) => unimplemented_literal("IntervalCompound", ctx, w),
        LiteralType::FixedChar(_) => unimplemented_literal("FixedChar", ctx, w),
        LiteralType::VarChar(_) => unimplemented_literal("VarChar", ctx, w),
        LiteralType::FixedBinary(_) => unimplemented_literal("FixedBinary", ctx, w),
        LiteralType::Decimal(_) => unimplemented_literal("Decimal", ctx, w),
        LiteralType::PrecisionTime(p) => write_precision_literal(
            "PrecisionTime",
            p.precision,
            precision_time_to_string(p.value, p.precision),
            ctx,
            w,
        ),
        LiteralType::PrecisionTimestamp(p) => write_precision_literal(
            "PrecisionTimestamp",
            p.precision,
            precision_timestamp_to_string(p.value, p.precision),
            ctx,
            w,
        ),
        LiteralType::PrecisionTimestampTz(p) => write_precision_literal(
            "PrecisionTimestampTz",
            p.precision,
            precision_timestamp_to_string(p.value, p.precision),
            ctx,
            w,
        ),
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

/// The type suffix for a literal (e.g., `"i32"`, `"fp64"`, `"date"`).
/// Returns `None` for unimplemented types whose [`write_literal_value`] already
/// emitted an error token.
fn literal_type_suffix(lit: &LiteralType, nullable: bool) -> Option<Cow<'static, str>> {
    let q = if nullable { "?" } else { "" };
    // truncate precision-12 (picosecond) values down to nanoseconds, since chrono can't
    // represent picoseconds. The suffix must report that same truncated precision (9).
    let displayed_precision = |precision: i32| if precision == 12 { 9 } else { precision };
    match lit {
        LiteralType::Boolean(_) => Some(format!("boolean{q}").into()),
        LiteralType::I8(_) => Some(format!("i8{q}").into()),
        LiteralType::I16(_) => Some(format!("i16{q}").into()),
        LiteralType::I32(_) => Some(format!("i32{q}").into()),
        LiteralType::I64(_) => Some(format!("i64{q}").into()),
        LiteralType::Fp32(_) => Some(format!("fp32{q}").into()),
        LiteralType::Fp64(_) => Some(format!("fp64{q}").into()),
        LiteralType::String(_) => Some(format!("string{q}").into()),
        LiteralType::Binary(_) => Some(format!("binary{q}").into()),
        LiteralType::Date(_) => Some(format!("date{q}").into()),
        #[allow(deprecated)]
        LiteralType::Time(_) => Some(format!("time{q}").into()),
        #[allow(deprecated)]
        LiteralType::Timestamp(_) => Some(format!("timestamp{q}").into()),
        LiteralType::PrecisionTimestamp(p) => Some(
            format!(
                "precisiontimestamp{q}<{}>",
                displayed_precision(p.precision)
            )
            .into(),
        ),
        LiteralType::PrecisionTimestampTz(p) => Some(
            format!(
                "precisiontimestamptz{q}<{}>",
                displayed_precision(p.precision)
            )
            .into(),
        ),
        LiteralType::PrecisionTime(p) => {
            Some(format!("precisiontime{q}<{}>", displayed_precision(p.precision)).into())
        }
        _ => None,
    }
}

/// Whether this type is the default interpretation for its value syntax.
///
/// Each literal value syntax has a default type that the parser assumes when
/// no explicit type suffix is present:
/// - `true`/`false` → `boolean`
/// - bare integers (`42`) → `i64`
/// - bare floats (`3.19`) → `fp64`
/// - single-quoted strings (`'hello'`) → `string`
/// - hex literals (`0x...`) → `binary`
///
/// Non-default types (e.g., `i32`, `fp32`, `date`) always need an explicit
/// suffix to distinguish them from the default.
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
            Visibility::Required => self.nullable || !is_default_for_syntax(lit),
        };
        if let LiteralType::Null(typ) = lit {
            write!(w, ":{}", ctx.expect(Some(typ)))?;
            return Ok(());
        }
        if show_suffix && let Some(suffix) = literal_type_suffix(lit, self.nullable) {
            write!(w, ":{suffix}")?;
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
                root_type: Some(RootType::RootReference(RootReference {})),
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
        match &self.root_type {
            Some(RootType::RootReference(_)) => {}
            None => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(PlanError::invalid(
                        "FieldReference",
                        Some("root_type"),
                        "Required field root_type is missing",
                    ))
                );
            }
            Some(RootType::Expression(_)) => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(PlanError::unimplemented(
                        "FieldReference",
                        Some("root_type"),
                        "FieldReference textification not implemented for Expression root_type",
                    ))
                );
            }
            Some(RootType::OuterReference(_)) => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(PlanError::unimplemented(
                        "FieldReference",
                        Some("root_type"),
                        "FieldReference textification not implemented for OuterReference root_type",
                    ))
                );
            }
            Some(RootType::LambdaParameterReference(_)) => {
                return write!(
                    w,
                    "{}",
                    ctx.failure(PlanError::unimplemented(
                        "FieldReference",
                        Some("root_type"),
                        "FieldReference textification not implemented for LambdaParameterReference root_type",
                    ))
                );
            }
        }

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
        let output_type = ctx.display(&output);

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
            RexType::Lambda(_) => write!(
                w,
                "{}",
                ctx.failure(PlanError::unimplemented(
                    "RexType",
                    Some("Lambda"),
                    "Lambda textification not implemented",
                ))
            ),
            RexType::LambdaInvocation(_) => write!(
                w,
                "{}",
                ctx.failure(PlanError::unimplemented(
                    "RexType",
                    Some("LambdaInvocation"),
                    "LambdaInvocation textification not implemented",
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
        let output_type = ctx.display(&output);

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
    use substrait::proto::r#type::{Boolean, I16, I32, I64, Kind, Nullability, UserDefined};

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
    fn test_precision_timestamp_to_string() {
        assert_eq!(
            precision_timestamp_to_string(10, 0),
            Ok("1970-01-01T00:00:10".to_string())
        );
        assert_eq!(
            precision_timestamp_to_string(123_456_789, 9),
            Ok("1970-01-01T00:00:00.123456789".to_string())
        );
        // Precision 12 (picoseconds) is truncated to nanoseconds (best-effort):
        // the trailing 500 picoseconds below don't survive.
        assert_eq!(
            precision_timestamp_to_string(123_456_789_500, 12),
            Ok("1970-01-01T00:00:00.123456789".to_string())
        );
        assert_eq!(
            precision_timestamp_to_string(0, 13),
            Err(PrecisionFormatError::UnsupportedPrecision)
        );
    }

    #[test]
    fn test_precision_time_to_string() {
        assert_eq!(precision_time_to_string(0, 0), Ok("00:00:00".to_string()));
        assert_eq!(
            // 01:01:01 = 3661 seconds, in microseconds
            precision_time_to_string(3_661_000_000, 6),
            Ok("01:01:01".to_string())
        );
        assert_eq!(
            // 01:01:01 = 3661 seconds, in picoseconds, plus 500 ps that get
            // truncated away.
            precision_time_to_string(3_661_000_000_000_500, 12),
            Ok("01:01:01".to_string())
        );
        assert_eq!(
            precision_time_to_string(0, 13),
            Err(PrecisionFormatError::UnsupportedPrecision)
        );
        // A negative value at precision 12 truncates towards zero when converted
        // to a `chrono::Duration` (-1 / 1000 == 0), so the sign must be checked
        // on the raw value, not the derived duration.
        assert_eq!(
            precision_time_to_string(-1, 12),
            Err(PrecisionFormatError::OutOfRange)
        );
    }

    #[test]
    fn test_nullable_precision_timestamp_literal_textify() {
        let ctx = TestContext::new();
        assert_eq!(
            ctx.textify_no_errors(&nullable_literal(
                expr::literal::LiteralType::PrecisionTimestamp(expr::literal::PrecisionTimestamp {
                    precision: 6,
                    value: 1000,
                })
            )),
            "'1970-01-01T00:00:00.001':precisiontimestamp?<6>"
        );
        assert_eq!(
            ctx.textify_no_errors(&nullable_literal(
                expr::literal::LiteralType::PrecisionTimestampTz(
                    expr::literal::PrecisionTimestamp {
                        precision: 3,
                        value: 5,
                    }
                )
            )),
            "'1970-01-01T00:00:00.005':precisiontimestamptz?<3>"
        );
        assert_eq!(
            ctx.textify_no_errors(&nullable_literal(
                expr::literal::LiteralType::PrecisionTime(expr::literal::PrecisionTime {
                    precision: 0,
                    value: 61,
                })
            )),
            "'00:01:01':precisiontime?<0>"
        );
    }

    #[test]
    fn test_precision_time_literal_precision_12_best_effort() {
        let ctx = TestContext::new();
        let (s, errs) = ctx.textify(&non_nullable_literal(
            expr::literal::LiteralType::PrecisionTime(expr::literal::PrecisionTime {
                precision: 12,
                value: 3_661_000_000_000_500,
            }),
        ));
        assert_eq!(s, "'01:01:01':precisiontime<9>");
        assert_eq!(errs.0.len(), 1);
        assert!(errs.0[0].to_string().contains("truncated"));
    }

    #[test]
    fn test_precision_timestamp_literal_supported_precision_no_warning() {
        let ctx = TestContext::new();
        // Precisions chrono can represent exactly shouldn't trigger the
        // precision-12 truncation warning.
        let (_, errs) = ctx.textify(&non_nullable_literal(
            expr::literal::LiteralType::PrecisionTimestamp(expr::literal::PrecisionTimestamp {
                precision: 9,
                value: 123_456_789,
            }),
        ));
        assert_eq!(errs.0.len(), 0);
    }

    #[test]
    fn test_precision_timestamp_literal_unrecognized_precision_invalid() {
        let ctx = TestContext::new();
        let (s, errs) = ctx.textify(&non_nullable_literal(
            expr::literal::LiteralType::PrecisionTimestamp(expr::literal::PrecisionTimestamp {
                precision: 13,
                value: 0,
            }),
        ));
        // The value fails to render (unrecognized precision), but the suffix
        // is still written with the precision as-is.
        assert_eq!(s, "!{LiteralType}:precisiontimestamp<13>");
        assert_eq!(errs.0.len(), 1);
        assert!(errs.0[0].to_string().contains("PrecisionTimestamp"));
    }

    #[test]
    fn test_precision_timestamp_literal_out_of_range_does_not_panic() {
        let ctx = TestContext::new();
        // 9e12 seconds since epoch is a valid i64 and a valid `chrono::Duration`,
        // but it's outside NaiveDateTime's representable range: adding it to the
        // epoch with `+` would panic. Textification should report a failure
        // token instead.
        let (s, errs) = ctx.textify(&non_nullable_literal(
            expr::literal::LiteralType::PrecisionTimestamp(expr::literal::PrecisionTimestamp {
                precision: 0,
                value: 9_000_000_000_000,
            }),
        ));
        assert_eq!(s, "!{LiteralType}:precisiontimestamp<0>");
        assert_eq!(errs.0.len(), 1);
    }

    #[test]
    fn test_precision_time_literal_beyond_one_day_invalid() {
        let ctx = TestContext::new();
        // 86,460 seconds since midnight is more than a day; `NaiveTime + Duration`
        // wraps modulo 24 hours rather than erroring, which would otherwise
        // silently misrepresent the value as 00:01:00.
        let (s, errs) = ctx.textify(&non_nullable_literal(
            expr::literal::LiteralType::PrecisionTime(expr::literal::PrecisionTime {
                precision: 0,
                value: 86_460,
            }),
        ));
        assert_eq!(s, "!{LiteralType}:precisiontime<0>");
        assert_eq!(errs.0.len(), 1);
    }

    #[test]
    fn test_precision_time_literal_unrecognized_precision_invalid() {
        let ctx = TestContext::new();
        let (s, errs) = ctx.textify(&non_nullable_literal(
            expr::literal::LiteralType::PrecisionTime(expr::literal::PrecisionTime {
                precision: 13,
                value: 0,
            }),
        ));
        assert_eq!(s, "!{LiteralType}:precisiontime<13>");
        assert_eq!(errs.0.len(), 1);
        assert!(errs.0[0].to_string().contains("PrecisionTime"));
    }

    #[test]
    fn test_nullable_precision_timestamp_literal_precision_12_best_effort() {
        let ctx = TestContext::new();
        // Picoseconds aren't representable, the textify best-effort approach is to
        // truncate to nanoseconds. The lost precision is reported via the error accumulator.
        let (s, errs) = ctx.textify(&nullable_literal(
            expr::literal::LiteralType::PrecisionTimestamp(expr::literal::PrecisionTimestamp {
                precision: 12,
                value: 123_456_789_500,
            }),
        ));
        assert_eq!(s, "'1970-01-01T00:00:00.123456789':precisiontimestamp?<9>");
        assert_eq!(errs.0.len(), 1);
        assert!(errs.0[0].to_string().contains("truncated"));
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
            output_type: Some(Type {
                kind: Some(Kind::I64(I64 {
                    nullability: Nullability::Required as i32,
                    type_variation_reference: 0,
                })),
            }),
            #[allow(deprecated)]
            args: vec![],
        });
        let (s, errq) = ctx.textify(&func);
        let errs: Vec<_> = errq.0;
        match errs[0] {
            FormatError::Lookup(MissingReference::MissingAnchor(k, a)) => {
                assert_eq!(k, ExtensionKind::Function);
                assert_eq!(a, 1000);
            }
            _ => panic!("Expected Lookup MissingAnchor: {}", errs[0]),
        }
        assert_eq!(s, "!{function}#1000():i64");

        let ctx = ctx.with_urn(1, "first").with_function(1, 100, "first");
        let func = RexType::ScalarFunction(ScalarFunction {
            function_reference: 100,
            arguments: vec![],
            options: vec![],
            output_type: Some(Type {
                kind: Some(Kind::I64(I64 {
                    nullability: Nullability::Required as i32,
                    type_variation_reference: 0,
                })),
            }),
            #[allow(deprecated)]
            args: vec![],
        });
        let s = ctx.textify_no_errors(&func);
        assert_eq!(s, "first():i64");

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
            output_type: Some(Type {
                kind: Some(Kind::Bool(Boolean {
                    nullability: Nullability::Required as i32,
                    type_variation_reference: 0,
                })),
            }),
            #[allow(deprecated)]
            args: vec![],
        });
        let s = ctx.textify_no_errors(&rex_dup);
        assert_eq!(s, "duplicated#231(true):boolean");
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

    fn struct_field_reference(field: i32) -> FieldReference {
        FieldReference {
            reference_type: Some(ReferenceType::DirectReference(ReferenceSegment {
                reference_type: Some(reference_segment::ReferenceType::StructField(Box::new(
                    reference_segment::StructField { field, child: None },
                ))),
            })),
            root_type: Some(RootType::RootReference(RootReference {})),
        }
    }

    #[test]
    fn test_field_reference_missing_root_type() {
        let ctx = TestContext::new();
        let mut fr = struct_field_reference(3);
        fr.root_type = None;
        let (s, errs) = ctx.textify(&fr);
        assert_eq!(s, "!{FieldReference}");
        match &errs.0[0] {
            FormatError::Format(e) => {
                assert_eq!(e.message, "FieldReference");
                assert_eq!(e.error_type, FormatErrorType::InvalidValue);
            }
            other => panic!("Expected Format(InvalidValue) for missing root_type, got: {other}"),
        }
    }

    #[test]
    fn test_field_reference_root_reference() {
        let ctx = TestContext::new();
        let fr = struct_field_reference(3);
        assert_eq!(ctx.textify_no_errors(&fr), "$3");
    }

    #[test]
    fn test_field_reference_outer_reference_unimplemented() {
        use substrait::proto::expression::field_reference;

        let ctx = TestContext::new();
        let mut fr = struct_field_reference(3);
        fr.root_type = Some(RootType::OuterReference(field_reference::OuterReference {
            steps_out: 1,
        }));
        let (s, errs) = ctx.textify(&fr);
        assert_eq!(s, "!{FieldReference}");
        match &errs.0[0] {
            FormatError::Format(e) => {
                assert_eq!(e.message, "FieldReference");
                assert_eq!(e.error_type, FormatErrorType::Unimplemented);
            }
            other => panic!("Expected Format(Unimplemented) for OuterReference, got: {other}"),
        }
    }

    #[test]
    fn test_field_reference_expression_unimplemented() {
        let ctx = TestContext::new();
        let mut fr = struct_field_reference(3);
        fr.root_type = Some(RootType::Expression(Box::new(literal_bool(true))));
        let (s, errs) = ctx.textify(&fr);
        assert_eq!(s, "!{FieldReference}");
        match &errs.0[0] {
            FormatError::Format(e) => {
                assert_eq!(e.message, "FieldReference");
                assert_eq!(e.error_type, FormatErrorType::Unimplemented);
            }
            other => panic!("Expected Format(Unimplemented) for Expression, got: {other}"),
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

    #[test]
    fn test_cast_to_user_defined_type_textifies_without_u_prefix() {
        // A type stored as "u!json" normalizes to "json"; cast emits "::json".
        let ctx = TestContext::new()
            .with_urn(1, "urn:example:types")
            .with_type(1, 5, "u!json");
        let cast = Cast {
            r#type: Some(Type {
                kind: Some(Kind::UserDefined(UserDefined {
                    type_variation_reference: 0,
                    nullability: Nullability::Required as i32,
                    type_reference: 5,
                    type_parameters: vec![],
                })),
            }),
            input: Some(Box::new(literal_i32(1))),
            failure_behavior: 0,
        };
        assert_eq!(ctx.textify_no_errors(&cast), "(1:i32)::json");
    }
}
