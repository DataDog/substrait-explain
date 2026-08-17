//! Shared value-rendering primitives ([`Value`], [`NamedArg`], [`Arguments`])
//! used by both relation and expression textification.

use std::borrow::Cow;
use std::convert::TryFrom;
use std::fmt;

use prost::UnknownEnumValue;
use substrait::proto::aggregate_function::AggregationInvocation;
use substrait::proto::sort_field::{SortDirection, SortKind};
use substrait::proto::{
    AggregateFunction, AggregationPhase, Expression, SortField, Type, join_rel, set_rel,
};

use super::types::Name;
use super::{PlanError, Scope, Textify};
use crate::extensions::{ExtensionColumn, ExtensionValue};

/// A trait for enum types that can be rendered as `&VariantName` in the text
/// format.
pub trait ValueEnum {
    fn as_enum_str(&self) -> Result<Cow<'static, str>, PlanError>;
}

#[derive(Debug, Clone)]
pub struct NamedArg<'a> {
    pub name: Cow<'a, str>,
    pub value: Value<'a>,
}

#[derive(Debug, Clone)]
pub enum Value<'a> {
    TableName(Vec<Name<'a>>),
    Field(Option<Name<'a>>, Option<&'a Type>),
    Tuple(Vec<Value<'a>>),
    Reference(i32),
    Expression(&'a Expression),
    AggregateFunction(&'a AggregateFunction),
    /// Represents a missing, invalid, or unspecified value.
    Missing(PlanError),
    /// Represents a valid enum value as a string for textification.
    Enum(Cow<'a, str>),
    EmptyGroup,
    Integer(i64),
    /// A decoded extension argument value.
    ExtensionArgument(ExtensionValue),
    /// A decoded extension output column.
    ExtColumn(ExtensionColumn),
}

impl<'a> Value<'a> {
    pub fn expect(maybe_value: Option<Self>, f: impl FnOnce() -> PlanError) -> Self {
        match maybe_value {
            Some(s) => s,
            None => Value::Missing(f()),
        }
    }
}

impl<'a> From<Result<Vec<Name<'a>>, PlanError>> for Value<'a> {
    fn from(token: Result<Vec<Name<'a>>, PlanError>) -> Self {
        match token {
            Ok(value) => Value::TableName(value),
            Err(err) => Value::Missing(err),
        }
    }
}

impl<'a> Textify for Value<'a> {
    fn name() -> &'static str {
        "Value"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        match self {
            Value::TableName(names) => write!(w, "{}", ctx.separated(names, ".")),
            Value::Field(name, typ) => {
                write!(w, "{}:{}", ctx.expect(name.as_ref()), ctx.expect(*typ))
            }
            Value::Tuple(values) => write!(w, "({})", ctx.separated(values, ", ")),
            // Field-reference syntax (`$N`); inlined rather than importing `expressions::Reference`.
            Value::Reference(i) => write!(w, "${i}"),
            Value::Expression(e) => write!(w, "{}", ctx.display(*e)),
            Value::AggregateFunction(agg_fn) => agg_fn.textify(ctx, w),
            Value::Missing(err) => write!(w, "{}", ctx.failure(err.clone())),
            Value::Enum(res) => write!(w, "&{res}"),
            Value::Integer(i) => write!(w, "{i}"),
            Value::EmptyGroup => write!(w, "_"),
            Value::ExtensionArgument(ev) => ev.textify(ctx, w),
            Value::ExtColumn(ec) => ec.textify(ctx, w),
        }
    }
}

/// A comma-separated argument list: positional arguments first, then named
/// arguments. Renders as `arg, arg, name=arg`, or `_` when empty.
#[derive(Debug, Clone, Default)]
pub struct Arguments<'a> {
    /// Positional arguments (e.g., a filter condition, group-bys, etc.)
    pub positional: Vec<Value<'a>>,
    /// Named arguments (e.g., limit=10, offset=5)
    pub named: Vec<NamedArg<'a>>,
}

impl<'a> Arguments<'a> {
    pub fn new(positional: Vec<Value<'a>>, named: Vec<NamedArg<'a>>) -> Self {
        Arguments { positional, named }
    }
}

impl<'a> Textify for Arguments<'a> {
    fn name() -> &'static str {
        "Arguments"
    }
    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        if self.positional.is_empty() && self.named.is_empty() {
            return write!(w, "_");
        }

        write!(w, "{}", ctx.separated(self.positional.iter(), ", "))?;
        if !self.positional.is_empty() && !self.named.is_empty() {
            write!(w, ", ")?;
        }
        write!(w, "{}", ctx.separated(self.named.iter(), ", "))
    }
}

impl<'a> From<&'a SortField> for Value<'a> {
    fn from(sf: &'a SortField) -> Self {
        let field = match &sf.expr {
            Some(expr) => Value::Expression(expr),
            None => Value::Missing(PlanError::unimplemented(
                "SortField",
                Some("expr"),
                "Missing expr",
            )),
        };
        let direction = match &sf.sort_kind {
            Some(kind) => Value::from(kind),
            None => Value::Missing(PlanError::invalid(
                "SortKind",
                Some(Cow::Borrowed("sort_kind")),
                "Missing sort_kind",
            )),
        };
        Value::Tuple(vec![field, direction])
    }
}

/// Converts an [`ValueEnum::as_enum_str`] result into a [`Value`]. Shared by
/// the blanket `From<&T>` impl below and by callers that only have an owned
/// enum value (and so can't borrow it for the lifetime `Value<'a>` requires).
pub(crate) fn enum_str_value<'a>(result: Result<Cow<'static, str>, PlanError>) -> Value<'a> {
    match result {
        Ok(s) => Value::Enum(s),
        Err(e) => Value::Missing(e),
    }
}

/// Decode a raw protobuf enum field (`i32`) into its shared [`Value`]
/// rendering: convert to the enum type and then to its `&Variant` string, or
/// produce a field-specific diagnostic when the raw value matches no variant.
///
/// Keeps the decode-or-diagnose shape in one place for callers that render an
/// enum field straight from its `i32` (currently `SetRel`'s `op`). `message` is
/// the proto message tag used for the failure token, `field` the offending
/// field name; both are named in the diagnostic so it identifies which field
/// carried the unknown value.
pub(crate) fn decode_enum_field<'a, T>(
    raw: i32,
    message: &'static str,
    field: &'static str,
) -> Value<'a>
where
    T: TryFrom<i32> + ValueEnum,
{
    match T::try_from(raw) {
        Ok(v) => enum_str_value(v.as_enum_str()),
        Err(_) => Value::Missing(PlanError::invalid(
            message,
            Some(field),
            format!("Unknown {message}.{field}: {raw}"),
        )),
    }
}

impl<'a, T: ValueEnum + ?Sized> From<&'a T> for Value<'a> {
    fn from(enum_val: &'a T) -> Self {
        enum_str_value(enum_val.as_enum_str())
    }
}

impl ValueEnum for SortKind {
    fn as_enum_str(&self) -> Result<Cow<'static, str>, PlanError> {
        let d = match self {
            &SortKind::Direction(d) => SortDirection::try_from(d),
            SortKind::ComparisonFunctionReference(f) => {
                return Err(PlanError::invalid(
                    "SortKind",
                    Some(Cow::Owned(format!("function reference{f}"))),
                    "SortKind::ComparisonFunctionReference unimplemented",
                ));
            }
        };
        let s = match d {
            Err(UnknownEnumValue(d)) => {
                return Err(PlanError::invalid(
                    "SortKind",
                    Some(Cow::Owned(format!("unknown variant: {d:?}"))),
                    "Unknown SortDirection",
                ));
            }
            Ok(SortDirection::AscNullsFirst) => "AscNullsFirst",
            Ok(SortDirection::AscNullsLast) => "AscNullsLast",
            Ok(SortDirection::DescNullsFirst) => "DescNullsFirst",
            Ok(SortDirection::DescNullsLast) => "DescNullsLast",
            Ok(SortDirection::Clustered) => "Clustered",
            Ok(SortDirection::Unspecified) => {
                return Err(PlanError::invalid(
                    "SortKind",
                    Option::<Cow<str>>::None,
                    "Unspecified SortDirection",
                ));
            }
        };
        Ok(Cow::Borrowed(s))
    }
}

impl ValueEnum for join_rel::JoinType {
    fn as_enum_str(&self) -> Result<Cow<'static, str>, PlanError> {
        let s = match self {
            join_rel::JoinType::Unspecified => {
                return Err(PlanError::invalid(
                    "JoinType",
                    Option::<Cow<str>>::None,
                    "Unspecified JoinType",
                ));
            }
            join_rel::JoinType::Inner => "Inner",
            join_rel::JoinType::Outer => "Outer",
            join_rel::JoinType::Left => "Left",
            join_rel::JoinType::Right => "Right",
            join_rel::JoinType::LeftSemi => "LeftSemi",
            join_rel::JoinType::RightSemi => "RightSemi",
            join_rel::JoinType::LeftAnti => "LeftAnti",
            join_rel::JoinType::RightAnti => "RightAnti",
            join_rel::JoinType::LeftSingle => "LeftSingle",
            join_rel::JoinType::RightSingle => "RightSingle",
            join_rel::JoinType::LeftMark => "LeftMark",
            join_rel::JoinType::RightMark => "RightMark",
        };
        Ok(Cow::Borrowed(s))
    }
}

impl ValueEnum for set_rel::SetOp {
    fn as_enum_str(&self) -> Result<Cow<'static, str>, PlanError> {
        let s = match self {
            set_rel::SetOp::Unspecified => {
                return Err(PlanError::invalid(
                    "SetOp",
                    Option::<Cow<str>>::None,
                    "Unspecified SetOp",
                ));
            }
            set_rel::SetOp::MinusPrimary => "MinusPrimary",
            set_rel::SetOp::MinusPrimaryAll => "MinusPrimaryAll",
            set_rel::SetOp::MinusMultiset => "MinusMultiset",
            set_rel::SetOp::IntersectionPrimary => "IntersectionPrimary",
            set_rel::SetOp::IntersectionMultiset => "IntersectionMultiset",
            set_rel::SetOp::IntersectionMultisetAll => "IntersectionMultisetAll",
            set_rel::SetOp::UnionDistinct => "UnionDistinct",
            set_rel::SetOp::UnionAll => "UnionAll",
        };
        Ok(Cow::Borrowed(s))
    }
}

impl ValueEnum for AggregationPhase {
    fn as_enum_str(&self) -> Result<Cow<'static, str>, PlanError> {
        let s = match self {
            AggregationPhase::Unspecified => "Unspecified",
            AggregationPhase::InitialToIntermediate => "InitialToIntermediate",
            AggregationPhase::IntermediateToIntermediate => "IntermediateToIntermediate",
            AggregationPhase::InitialToResult => "InitialToResult",
            AggregationPhase::IntermediateToResult => "IntermediateToResult",
        };
        Ok(Cow::Borrowed(s))
    }
}

impl ValueEnum for AggregationInvocation {
    fn as_enum_str(&self) -> Result<Cow<'static, str>, PlanError> {
        let s = match self {
            AggregationInvocation::Unspecified => "Unspecified",
            AggregationInvocation::All => "All",
            AggregationInvocation::Distinct => "Distinct",
        };
        Ok(Cow::Borrowed(s))
    }
}

impl<'a> Textify for NamedArg<'a> {
    fn name() -> &'static str {
        "NamedArg"
    }
    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        write!(w, "{}=", self.name)?;
        self.value.textify(ctx, w)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixtures::TestContext;

    #[test]
    fn test_arguments_textify_positional_only() {
        let ctx = TestContext::new();
        let args = Arguments::new(vec![Value::Integer(42), Value::Integer(7)], vec![]);
        let (result, errors) = ctx.textify(&args);
        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
        assert_eq!(result, "42, 7");
    }

    #[test]
    fn test_arguments_textify_named_only() {
        let ctx = TestContext::new();
        let args = Arguments::new(
            vec![],
            vec![
                NamedArg {
                    name: Cow::Borrowed("limit"),
                    value: Value::Integer(10),
                },
                NamedArg {
                    name: Cow::Borrowed("offset"),
                    value: Value::Integer(5),
                },
            ],
        );
        let (result, errors) = ctx.textify(&args);
        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
        assert_eq!(result, "limit=10, offset=5");
    }

    #[test]
    fn test_arguments_textify_both() {
        let ctx = TestContext::new();
        let args = Arguments::new(
            vec![Value::Integer(1)],
            vec![NamedArg {
                name: "foo".into(),
                value: Value::Integer(2),
            }],
        );
        let (result, errors) = ctx.textify(&args);
        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
        assert_eq!(result, "1, foo=2");
    }

    #[test]
    fn test_arguments_textify_empty() {
        let ctx = TestContext::new();
        let args = Arguments::new(vec![], vec![]);
        let (result, errors) = ctx.textify(&args);
        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
        assert_eq!(result, "_");
    }

    #[test]
    fn test_named_arg_textify_error_token() {
        let ctx = TestContext::new();
        let named_arg = NamedArg {
            name: "foo".into(),
            value: Value::Missing(PlanError::invalid(
                "my_enum",
                Some(Cow::Borrowed("my_enum")),
                Cow::Borrowed("my_enum"),
            )),
        };
        let (result, errors) = ctx.textify(&named_arg);
        // Should show !{my_enum} in the output
        assert!(result.contains("foo=!{my_enum}"), "Output: {result}");
        // Should also accumulate an error
        assert!(!errors.is_empty(), "Expected error for error token");
    }

    #[test]
    fn test_decode_enum_field_known_variant() {
        let value =
            decode_enum_field::<set_rel::SetOp>(set_rel::SetOp::UnionAll as i32, "SetRel", "op");
        match value {
            Value::Enum(s) => assert_eq!(s, "UnionAll"),
            other => panic!("Expected Value::Enum, got {other:?}"),
        }
    }

    #[test]
    fn test_decode_enum_field_unknown_variant_names_field() {
        let value = decode_enum_field::<set_rel::SetOp>(99, "SetRel", "op");
        match value {
            Value::Missing(err) => {
                assert_eq!(err.message, "SetRel");
                assert_eq!(err.lookup.as_deref(), Some("op"));
                // The description names the offending field, not just the
                // message, so the diagnostic is actionable on its own.
                assert_eq!(err.description, "Unknown SetRel.op: 99");
            }
            other => panic!("Expected Value::Missing, got {other:?}"),
        }
    }
}
