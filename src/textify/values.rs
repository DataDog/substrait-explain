//! Shared value-rendering primitives ([`Value`], [`NamedArg`], [`Arguments`])
//! used by both relation and expression textification.

use std::borrow::Cow;
use std::convert::TryFrom;
use std::fmt;

use prost::UnknownEnumValue;
use substrait::proto::aggregate_function::AggregationInvocation;
use substrait::proto::sort_field::{SortDirection, SortKind};
use substrait::proto::{
    AggregateFunction, AggregationPhase, Expression, SortField, join_rel, set_rel,
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
    Field(Option<Name<'a>>, Option<&'a substrait::proto::Type>),
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

/// How an argument list renders inside a relation's `[...]` or an
/// expression's `(...)`.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum ArgsLayout {
    /// `arg, arg, arg` on a single line.
    #[default]
    Inline,
    /// One `- arg` per line, used for `Read:Virtual` rows. See
    /// [`super::rels::Relation::write_header`] for the exact layout.
    Rows,
}

#[derive(Debug, Clone)]
pub struct Arguments<'a> {
    /// Positional arguments (e.g., a filter condition, group-bys, etc.)
    pub positional: Vec<Value<'a>>,
    /// Named arguments (e.g., limit=10, offset=5)
    pub named: Vec<NamedArg<'a>>,
    /// How this argument list is laid out. Defaults to [`ArgsLayout::Inline`];
    /// only `Read:Virtual` opts into [`ArgsLayout::Rows`].
    layout: ArgsLayout,
}

impl<'a> Arguments<'a> {
    /// An inline argument list (`arg, arg, arg`), the default for every
    /// relation.
    pub fn inline(positional: Vec<Value<'a>>, named: Vec<NamedArg<'a>>) -> Self {
        Arguments {
            positional,
            named,
            layout: ArgsLayout::Inline,
        }
    }

    /// A row-per-line argument list (`- arg` per line) used for `Read:Virtual`
    /// with many rows. Currently not enabled for named arguments.
    /// TODO: enable for named arguments as well.
    pub fn rows(positional: Vec<Value<'a>>) -> Self {
        Arguments {
            positional,
            named: vec![],
            layout: ArgsLayout::Rows,
        }
    }

    pub fn layout(&self) -> ArgsLayout {
        self.layout
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
            Some(expr) => match &expr.rex_type {
                Some(substrait::proto::expression::RexType::Selection(fref)) => {
                    if let Some(substrait::proto::expression::field_reference::ReferenceType::DirectReference(seg)) = &fref.reference_type {
                        if let Some(substrait::proto::expression::reference_segment::ReferenceType::StructField(sf)) = &seg.reference_type {
                            Value::Reference(sf.field)
                        } else { Value::Missing(PlanError::unimplemented("SortField", Some("expr"), "Not a struct field")) }
                    } else { Value::Missing(PlanError::unimplemented("SortField", Some("expr"), "Not a direct reference")) }
                }
                _ => Value::Missing(PlanError::unimplemented(
                    "SortField",
                    Some("expr"),
                    "Not a selection",
                )),
            },
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
/// Shared by the callers that render an enum field straight from its `i32`
/// (window `phase=`/`invocation=`, `SetRel`'s `op`), so the
/// decode-or-diagnose shape lives in one place. `message` is the proto message
/// tag used for the failure token, `field` the offending field name.
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
            format!("Unknown {message}: {raw}"),
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
