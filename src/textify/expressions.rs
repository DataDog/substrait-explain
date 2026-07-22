use std::borrow::Cow;
use std::fmt;

use chrono::{DateTime, NaiveDate};
use expr::RexType;
use substrait::proto::aggregate_function::AggregationInvocation;
use substrait::proto::expression::field_reference::{ReferenceType, RootReference, RootType};
use substrait::proto::expression::literal::LiteralType;
use substrait::proto::expression::window_function::{self, bound};
use substrait::proto::expression::{
    Cast, FieldReference, IfThen, ReferenceSegment, ScalarFunction, WindowFunction, cast,
    reference_segment,
};
use substrait::proto::function_argument::ArgType;
use substrait::proto::{
    AggregateFunction, AggregationPhase, Expression, FunctionArgument, FunctionOption, SortField,
    expression as expr,
};

use super::{PlanError, Scope, Textify, Visibility};
use crate::extensions::simple::ExtensionKind;
use crate::textify::types::{Name, NamedAnchor, OutputType, escaped};
use crate::textify::values::{Arguments, NamedArg, Value, decode_enum_field};

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
        LiteralType::Null(_) => write!(w, "null"),
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
        #[allow(deprecated)]
        LiteralType::Time(_) => Some("time"),
        #[allow(deprecated)]
        LiteralType::Timestamp(_) => Some("timestamp"),
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

/// The fields shared by every Substrait function call - `ScalarFunction`,
/// `AggregateFunction`, and `WindowFunction` - that render as
/// `name#anchor(args, options)`.
///
/// The remaining fields of each function message (the output type, and the
/// aggregate/window-specific parts) are textified alongside this by the
/// respective `Textify` implementations.
#[derive(Debug, Clone, Copy)]
pub struct FunctionInvocation<'a> {
    pub function_reference: u32,
    pub arguments: &'a [FunctionArgument],
    pub options: &'a [FunctionOption],
}

impl<'a> From<&'a ScalarFunction> for FunctionInvocation<'a> {
    fn from(f: &'a ScalarFunction) -> Self {
        FunctionInvocation {
            function_reference: f.function_reference,
            arguments: &f.arguments,
            options: &f.options,
        }
    }
}

impl<'a> From<&'a AggregateFunction> for FunctionInvocation<'a> {
    fn from(f: &'a AggregateFunction) -> Self {
        FunctionInvocation {
            function_reference: f.function_reference,
            arguments: &f.arguments,
            options: &f.options,
        }
    }
}

impl Textify for FunctionInvocation<'_> {
    fn name() -> &'static str {
        "FunctionInvocation"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        let name_and_anchor =
            NamedAnchor::lookup(ctx, ExtensionKind::Function, self.function_reference);
        let name_and_anchor = ctx.display(&name_and_anchor);

        let args = ctx.separated(self.arguments, ", ");
        let options = ctx.separated(self.options, ", ");
        let between = if self.arguments.is_empty() || self.options.is_empty() {
            ""
        } else {
            ", "
        };

        write!(w, "{name_and_anchor}({args}{between}{options})")
    }
}

impl Textify for ScalarFunction {
    fn name() -> &'static str {
        "ScalarFunction"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        let invocation = FunctionInvocation::from(self);
        let output_type = OutputType(self.output_type.as_ref());

        let invocation = ctx.display(&invocation);
        let output_type = ctx.display(&output_type);

        write!(w, "{invocation}{output_type}")
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

fn window_sort_order<'a>(sorts: &'a [SortField]) -> Value<'a> {
    match sorts {
        [single] => Value::from(single),
        many => Value::Tuple(many.iter().map(Value::from).collect()),
    }
}

/// A single partition expression is written bare (`partition=$0`) rather than as a 1-tuple.
fn window_partition_value(partitions: &[Expression]) -> Value<'_> {
    match partitions {
        [single] => Value::Expression(single),
        many => Value::Tuple(many.iter().map(Value::Expression).collect()),
    }
}

/// The frame keyword (`rows`/`range`) and bounds for a window function, if a
/// frame is present. `range=` additionally requires exactly one `order=`
/// field; a proto violating that invariant is still textified best-effort,
/// with a diagnostic surfaced via `ctx.push_error`.
fn window_frame_named_arg<'a, S: Scope>(f: &'a WindowFunction, ctx: &S) -> Option<NamedArg<'a>> {
    let bounds_type = window_function::BoundsType::try_from(f.bounds_type);
    let has_bounds = f.lower_bound.is_some() || f.upper_bound.is_some();
    if matches!(bounds_type, Ok(window_function::BoundsType::Unspecified)) && !has_bounds {
        return None;
    }

    let keyword = match bounds_type {
        Ok(window_function::BoundsType::Rows) => "rows",
        Ok(window_function::BoundsType::Range) => {
            // The parser rejects range= frames unless there is exactly one order= field.
            if f.sorts.len() != 1 {
                ctx.push_error(
                    PlanError::invalid(
                        "WindowFunction",
                        Some("bounds_type"),
                        format!(
                            "range frame requires exactly one order= field, found {}",
                            f.sorts.len()
                        ),
                    )
                    .into(),
                );
            }
            "range"
        }
        Ok(window_function::BoundsType::Unspecified) => {
            // bounds_type is required whenever a frame is present.
            ctx.push_error(
                PlanError::invalid(
                    "WindowFunction",
                    Some("bounds_type"),
                    "bounds_type is Unspecified but lower_bound/upper_bound are set",
                )
                .into(),
            );
            "rows"
        }
        Err(_) => {
            ctx.push_error(
                PlanError::invalid(
                    "WindowFunction",
                    Some("bounds_type"),
                    format!("Unknown BoundsType: {}", f.bounds_type),
                )
                .into(),
            );
            "rows"
        }
    };
    let lower = window_bound_value(f.lower_bound.as_ref(), "lower_bound");
    let upper = window_bound_value(f.upper_bound.as_ref(), "upper_bound");
    Some(NamedArg {
        name: Cow::Borrowed(keyword),
        value: Value::Tuple(vec![lower, upper]),
    })
}

/// Assembles the `over(...)` named arguments: `phase=`, `order=`,
/// `invocation=`, `partition=`, and `rows=`/`range=`.
fn window_over_named_args<'a, S: Scope>(f: &'a WindowFunction, ctx: &S) -> Vec<NamedArg<'a>> {
    let mut named_args = Vec::new();

    // the parser requires a phase= argument to be present in over(...) and is always written.
    named_args.push(NamedArg {
        name: Cow::Borrowed("phase"),
        value: decode_enum_field::<AggregationPhase>(f.phase, "AggregationPhase", "phase"),
    });

    // order= is omitted when there are no sort fields.
    if !f.sorts.is_empty() {
        named_args.push(NamedArg {
            name: Cow::Borrowed("order"),
            value: window_sort_order(&f.sorts),
        });
    }

    if f.invocation != AggregationInvocation::Unspecified as i32 {
        named_args.push(NamedArg {
            name: Cow::Borrowed("invocation"),
            value: decode_enum_field::<AggregationInvocation>(
                f.invocation,
                "AggregationInvocation",
                "invocation",
            ),
        });
    }

    if !f.partitions.is_empty() {
        named_args.push(NamedArg {
            name: Cow::Borrowed("partition"),
            value: window_partition_value(&f.partitions),
        });
    }

    if let Some(frame) = window_frame_named_arg(f, ctx) {
        named_args.push(frame);
    }

    named_args
}

fn window_bound_value<'a>(
    bound: Option<&window_function::Bound>,
    field_name: &'static str,
) -> Value<'a> {
    let Some(bound) = bound else {
        return Value::EmptyGroup;
    };
    match &bound.kind {
        None => Value::Missing(PlanError::invalid(
            "WindowFunction",
            Some(field_name),
            "Bound is present but has no kind set",
        )),
        Some(bound::Kind::Unbounded(_)) => Value::EmptyGroup,
        Some(bound::Kind::CurrentRow(_)) => Value::Integer(0),
        Some(bound::Kind::Preceding(p)) if p.offset < 0 => Value::Missing(PlanError::invalid(
            "WindowFunction",
            Some(field_name),
            format!("Preceding bound offset {} must not be negative", p.offset),
        )),
        Some(bound::Kind::Preceding(p)) if p.offset == 0 => Value::Missing(PlanError::invalid(
            "WindowFunction",
            Some(field_name),
            "Preceding bound offset must not be 0; use CurrentRow instead",
        )),
        Some(bound::Kind::Preceding(p)) => Value::Integer(-p.offset),
        Some(bound::Kind::Following(f)) if f.offset < 0 => Value::Missing(PlanError::invalid(
            "WindowFunction",
            Some(field_name),
            format!("Following bound offset {} must not be negative", f.offset),
        )),
        Some(bound::Kind::Following(f)) if f.offset == 0 => Value::Missing(PlanError::invalid(
            "WindowFunction",
            Some(field_name),
            "Following bound offset must not be 0; use CurrentRow instead",
        )),
        Some(bound::Kind::Following(f)) => Value::Integer(f.offset),
    }
}

impl Textify for WindowFunction {
    fn name() -> &'static str {
        "WindowFunction"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        // name/reference + arguments: `sum#10@1($0)`
        textify_function_call_prefix(
            self.function_reference,
            &self.arguments,
            &self.options,
            ctx,
            w,
        )?;

        // over-clause: ` over(phase=..., order=..., ...)`
        let named_args = window_over_named_args(self, ctx);
        write!(w, " over(")?;
        Arguments::inline(vec![], named_args).textify(ctx, w)?;
        write!(w, ")")?;

        // output type: `:i64`
        let output = OutputType(self.output_type.as_ref());
        write!(w, "{}", ctx.display(&output))
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
            RexType::WindowFunction(f) => f.textify(ctx, w),
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
        let invocation = FunctionInvocation::from(self);
        let output_type = OutputType(self.output_type.as_ref());

        let invocation = ctx.display(&invocation);
        let output_type = ctx.display(&output_type);

        write!(w, "{invocation}{output_type}")
    }
}

#[cfg(test)]
mod tests {
    use substrait::proto::Type;
    use substrait::proto::expression::{cast, if_then};
    use substrait::proto::sort_field::{SortDirection, SortKind};
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

    fn base_window_function() -> WindowFunction {
        WindowFunction {
            function_reference: 10,
            arguments: vec![],
            options: vec![],
            output_type: Some(make_i16_type()),
            phase: AggregationPhase::InitialToResult as i32,
            sorts: vec![],
            invocation: AggregationInvocation::Unspecified as i32,
            partitions: vec![],
            bounds_type: window_function::BoundsType::Unspecified as i32,
            lower_bound: None,
            upper_bound: None,
            #[allow(deprecated)]
            args: vec![],
        }
    }

    fn field_expr(field: i32) -> Expression {
        Expression {
            rex_type: Some(RexType::Selection(Box::new(struct_field_reference(field)))),
        }
    }

    fn sort_field(field: i32, direction: SortDirection) -> SortField {
        SortField {
            expr: Some(field_expr(field)),
            sort_kind: Some(SortKind::Direction(direction as i32)),
        }
    }

    /// How many accumulated diagnostics a textify case expects.
    enum Diag {
        /// Clean render, no errors.
        None,
        /// At least one diagnostic (best-effort output that won't re-parse).
        Some,
        /// Exactly this many diagnostics.
        Exactly(usize),
    }

    fn bound(kind: bound::Kind) -> window_function::Bound {
        window_function::Bound { kind: Some(kind) }
    }

    /// Window-function textify cases. Each builds a `WindowFunction` proto,
    /// renders it, and checks the output string plus how many diagnostics were
    /// accumulated. Grouped as a table since they share one shape;
    #[test]
    fn test_window_function_textify() {
        let rows = window_function::BoundsType::Rows as i32;
        let current_row = || bound::Kind::CurrentRow(bound::CurrentRow {});
        let unbounded = || bound::Kind::Unbounded(bound::Unbounded {});

        let cases: Vec<(&str, &str, WindowFunction, &str, Diag)> = vec![
            (
                "no frame: only the always-present phase= is emitted",
                "row_number",
                base_window_function(),
                "row_number() over(phase=&InitialToResult):i16",
                Diag::None,
            ),
            (
                "full: bare partition, order, invocation, rows frame",
                "sum",
                {
                    let mut f = base_window_function();
                    f.partitions = vec![field_expr(0)];
                    f.sorts = vec![sort_field(1, SortDirection::AscNullsLast)];
                    f.invocation = AggregationInvocation::Distinct as i32;
                    f.bounds_type = rows;
                    f.lower_bound = Some(bound(bound::Kind::Preceding(bound::Preceding {
                        offset: 3,
                    })));
                    f.upper_bound = Some(bound(current_row()));
                    f
                },
                "sum() over(phase=&InitialToResult, order=($1, &AscNullsLast), invocation=&Distinct, partition=$0, rows=(-3, 0)):i16",
                Diag::None,
            ),
            (
                "multiple order= fields render as a tuple of tuples",
                "sum",
                {
                    let mut f = base_window_function();
                    f.sorts = vec![
                        sort_field(0, SortDirection::AscNullsLast),
                        sort_field(1, SortDirection::DescNullsFirst),
                    ];
                    f
                },
                "sum() over(phase=&InitialToResult, order=(($0, &AscNullsLast), ($1, &DescNullsFirst))):i16",
                Diag::None,
            ),
            (
                "unbounded upper (None lower renders `_` too)",
                "sum",
                {
                    let mut f = base_window_function();
                    f.bounds_type = rows;
                    f.lower_bound = None;
                    f.upper_bound = Some(bound(unbounded()));
                    f
                },
                "sum() over(phase=&InitialToResult, rows=(_, _)):i16",
                Diag::None,
            ),
            (
                // bounds_type = Unspecified (0) is a *valid* enum variant, but a
                // frame is set anyway. Distinct from the out-of-range case below:
                // this hits the `Ok(Unspecified)` arm (a semantically-missing
                // frame type), and also checks the early-return guard does NOT
                // drop the frame when bounds are present. Best-effort rows=
                // output with the loss surfaced.
                "bounds_type = Unspecified (valid enum) with bounds set is lossy",
                "sum",
                {
                    let mut f = base_window_function();
                    f.lower_bound = Some(bound(current_row()));
                    f
                },
                "sum() over(phase=&InitialToResult, rows=(0, _)):i16",
                Diag::Some,
            ),
            (
                // A negative offset on either bound would re-parse as the
                // opposite direction (preceding<->following); surface an error
                // for each rather than silently flipping the sign. One proto
                // exercises both guard arms (Preceding.offset<0, Following.offset<0);
                // magnitude-agnostic, so this also covers i64::MIN.
                "negative offset on either bound surfaces an error",
                "sum",
                {
                    let mut f = base_window_function();
                    f.bounds_type = rows;
                    f.lower_bound = Some(bound(bound::Kind::Preceding(bound::Preceding {
                        offset: -5,
                    })));
                    f.upper_bound = Some(bound(bound::Kind::Following(bound::Following {
                        offset: -5,
                    })));
                    f
                },
                "sum() over(phase=&InitialToResult, rows=(!{WindowFunction}, !{WindowFunction})):i16",
                Diag::Exactly(2),
            ),
            (
                // A present Bound with no kind is malformed (the parser never
                // produces one); must not collapse into the `_` unbounded render.
                "present bound with no kind surfaces error",
                "sum",
                {
                    let mut f = base_window_function();
                    f.bounds_type = rows;
                    f.lower_bound = Some(window_function::Bound { kind: None });
                    f.upper_bound = Some(bound(current_row()));
                    f
                },
                "sum() over(phase=&InitialToResult, rows=(!{WindowFunction}, 0)):i16",
                Diag::Some,
            ),
            (
                // A 0 offset on PRECEDING/FOLLOWING would re-parse as CurrentRow;
                // surface a diagnostic for each rather than rendering bare `0`.
                "zero offset on each bound surfaces two errors",
                "sum",
                {
                    let mut f = base_window_function();
                    f.bounds_type = rows;
                    f.lower_bound = Some(bound(bound::Kind::Preceding(bound::Preceding {
                        offset: 0,
                    })));
                    f.upper_bound = Some(bound(bound::Kind::Following(bound::Following {
                        offset: 0,
                    })));
                    f
                },
                "sum() over(phase=&InitialToResult, rows=(!{WindowFunction}, !{WindowFunction})):i16",
                Diag::Exactly(2),
            ),
            (
                // range= requires exactly one order= field; wrong count still
                // renders best-effort but surfaces a diagnostic.
                "range= with wrong order-field count surfaces error",
                "sum",
                {
                    let mut f = base_window_function();
                    f.bounds_type = window_function::BoundsType::Range as i32;
                    f.lower_bound = Some(bound(unbounded()));
                    f.upper_bound = Some(bound(current_row()));
                    f
                },
                "sum() over(phase=&InitialToResult, range=(_, 0)):i16",
                Diag::Some,
            ),
            (
                // bounds_type = 99 is an out-of-range int: `try_from` returns
                // Err, so this hits the `Err(_)` arm (distinct from the valid
                // `Ok(Unspecified)` case).
                "bounds_type = out-of-range int falls back to rows= with a diagnostic",
                "sum",
                {
                    let mut f = base_window_function();
                    f.bounds_type = 99;
                    f.lower_bound = Some(bound(current_row()));
                    f.upper_bound = Some(bound(current_row()));
                    f
                },
                "sum() over(phase=&InitialToResult, rows=(0, 0)):i16",
                Diag::Some,
            ),
            (
                // Unknown enum ints render as an error token tagged with the
                // field's own enum type.
                "unknown phase= / invocation= enum ints surface errors",
                "sum",
                {
                    let mut f = base_window_function();
                    f.phase = 99;
                    f.invocation = 99;
                    f
                },
                "sum() over(phase=!{AggregationPhase}, invocation=!{AggregationInvocation}):i16",
                Diag::Some,
            ),
        ];

        for (what, func, f, expected, diag) in cases {
            let ctx = TestContext::new()
                .with_urn(1, "urn:example")
                .with_function(1, 10, func);
            let (s, errs) = ctx.textify(&f);
            assert_eq!(s, expected, "case: {what}");
            match diag {
                Diag::None => {
                    assert!(errs.0.is_empty(), "case: {what}: expected no diagnostics")
                }
                Diag::Some => assert!(!errs.0.is_empty(), "case: {what}: expected a diagnostic"),
                Diag::Exactly(n) => {
                    assert_eq!(errs.0.len(), n, "case: {what}: diagnostic count")
                }
            }
        }
    }
}
