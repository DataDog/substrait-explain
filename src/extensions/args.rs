//! Text-format data structures used by registered advanced extension handlers.
//!
//! These types describe the arguments accepted by custom relation types,
//! enhancements, and optimization hints. Relation extensions can additionally
//! describe output columns.
//!
//! The interface presented to extension handlers is structured rather than
//! textual: handlers read and write values such as [`ExtensionArgs`], [`Expr`],
//! and [`proto::Type`]. `substrait-explain` handles the surrounding
//! parsing/textification. Some values need plan context before they reach a
//! handler; for example, an expression argument like `add($0, $1)` is parsed
//! using [`SimpleExtensions`](crate::extensions::SimpleExtensions) to resolve
//! the text function name to the protobuf function anchor, and formatted by
//! resolving that anchor back to a text name.
//!
//! The extension-facing interface for Substrait objects (e.g. [`proto::Type`])
//! should map directly to Substrait protobuf concepts. Sometimes that means
//! storing the protobuf type directly, as named output columns do with
//! [`proto::Type`]; sometimes it means using a small wrapper, as
//! expression-compatible arguments do with [`Expr`] around
//! [`proto::Expression`].
//!
//! Untyped scalar literals (e.g. `2`, `2.435`, `'string'`) are kept as
//! extension scalar values so text rendering can preserve scalar syntax even in
//! verbose output, while handlers that accept expressions can still widen them
//! into default Substrait literal expressions.

use std::collections::HashSet;
use std::fmt;

use indexmap::IndexMap;
use prost_types::Timestamp as ProtoTimestamp;
use substrait::proto;
use substrait::proto::expression::field_reference::ReferenceType;
use substrait::proto::expression::literal::{LiteralType, PrecisionTime, PrecisionTimestamp};
use substrait::proto::expression::{Literal, RexType, reference_segment};

use super::ExtensionError;
use crate::textify::expressions::Reference;

/// Kind of relation addendum in the text format.
///
/// Addenda are `+`-prefixed lines attached to relations. They are syntax-level
/// constructs, distinct from [`crate::extensions::registry::ExtensionType`],
/// which describes registry namespaces.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AddendumKind {
    Enhancement,
    Optimization,
    ExtensionTable,
}

impl AddendumKind {
    pub(crate) fn prefix(self) -> &'static str {
        match self {
            AddendumKind::Enhancement => "Enh",
            AddendumKind::Optimization => "Opt",
            AddendumKind::ExtensionTable => "Ext",
        }
    }
}

/// A Substrait expression carried as an extension argument or output column.
///
/// Boxed because `proto::Expression` is large (multiple `Vec` fields in
/// variants like `ScalarFunction`).
#[derive(Debug, Clone)]
pub struct Expr(Box<proto::Expression>);

impl Expr {
    /// Borrow the underlying Substrait expression protobuf.
    pub fn as_proto(&self) -> &proto::Expression {
        self.0.as_ref()
    }

    /// Clone the underlying Substrait expression protobuf.
    pub fn to_proto(&self) -> proto::Expression {
        self.as_proto().clone()
    }

    /// If this expression is a direct field reference (`$N`), return it.
    pub fn as_direct_reference(&self) -> Option<Reference> {
        let Some(RexType::Selection(field_ref)) = self.as_proto().rex_type.as_ref() else {
            return None;
        };
        let Some(ReferenceType::DirectReference(segment)) = field_ref.reference_type.as_ref()
        else {
            return None;
        };
        let Some(reference_segment::ReferenceType::StructField(field)) =
            segment.reference_type.as_ref()
        else {
            return None;
        };
        if field.child.is_some() {
            return None;
        }
        Some(Reference(field.field))
    }
}

impl From<proto::Expression> for Expr {
    fn from(expr: proto::Expression) -> Self {
        Expr(Box::new(expr))
    }
}

impl From<Literal> for Expr {
    fn from(literal: Literal) -> Self {
        proto::Expression {
            rex_type: Some(RexType::Literal(literal)),
        }
        .into()
    }
}

impl From<Reference> for Expr {
    fn from(reference: Reference) -> Self {
        proto::Expression::from(reference).into()
    }
}

impl From<Expr> for proto::Expression {
    fn from(expr: Expr) -> Self {
        *expr.0
    }
}

impl From<i64> for Expr {
    fn from(value: i64) -> Self {
        Literal {
            literal_type: Some(LiteralType::I64(value)),
            nullable: false,
            type_variation_reference: 0,
        }
        .into()
    }
}

impl From<f64> for Expr {
    fn from(value: f64) -> Self {
        Literal {
            literal_type: Some(LiteralType::Fp64(value)),
            nullable: false,
            type_variation_reference: 0,
        }
        .into()
    }
}

impl From<bool> for Expr {
    fn from(value: bool) -> Self {
        Literal {
            literal_type: Some(LiteralType::Boolean(value)),
            nullable: false,
            type_variation_reference: 0,
        }
        .into()
    }
}

impl From<String> for Expr {
    fn from(value: String) -> Self {
        Literal {
            literal_type: Some(LiteralType::String(value)),
            nullable: false,
            type_variation_reference: 0,
        }
        .into()
    }
}

impl From<&str> for Expr {
    fn from(value: &str) -> Self {
        value.to_string().into()
    }
}

/// A Substrait literal carried as an extension argument.
///
/// Unlike [`Expr`], this represents literal argument syntax directly rather than
/// a full Substrait expression.
#[derive(Debug, Clone, PartialEq)]
pub struct ExtensionLiteral(Literal);

impl ExtensionLiteral {
    /// Borrow the underlying Substrait literal protobuf.
    pub fn as_proto(&self) -> &Literal {
        &self.0
    }

    /// Clone the underlying Substrait literal protobuf.
    pub fn to_proto(&self) -> Literal {
        self.0.clone()
    }

    pub fn date_days(days: i32) -> Self {
        Self(Literal {
            literal_type: Some(LiteralType::Date(days)),
            nullable: false,
            type_variation_reference: 0,
        })
    }

    #[allow(deprecated)]
    pub fn time_micros(micros: i64) -> Self {
        // TODO: Decide whether this raw-unit constructor should reject values
        // outside one time-of-day. The text parser uses HH:MM:SS and cannot
        // round-trip values that format as 24:00:00 or beyond.
        Self(Literal {
            literal_type: Some(LiteralType::Time(micros)),
            nullable: false,
            type_variation_reference: 0,
        })
    }

    #[allow(deprecated)]
    pub fn timestamp_micros(micros: i64) -> Self {
        Self(Literal {
            literal_type: Some(LiteralType::Timestamp(micros)),
            nullable: false,
            type_variation_reference: 0,
        })
    }

    pub fn precision_time_units(precision: i32, value: i64) -> Result<Self, ExtensionError> {
        validate_precision(precision)?;
        // TODO: Decide whether this raw-unit constructor should reject values
        // outside one time-of-day. The text parser uses HH:MM:SS and cannot
        // round-trip values that format as 24:00:00 or beyond.
        Ok(Self(Literal {
            literal_type: Some(LiteralType::PrecisionTime(PrecisionTime {
                precision,
                value,
            })),
            nullable: false,
            type_variation_reference: 0,
        }))
    }

    pub fn precision_timestamp_units(precision: i32, value: i64) -> Result<Self, ExtensionError> {
        validate_precision(precision)?;
        Ok(Self::precision_timestamp_units_unchecked(precision, value))
    }

    pub fn precision_timestamp_seconds(value: i64) -> Self {
        Self::precision_timestamp_units_unchecked(0, value)
    }

    pub fn precision_timestamp_millis(value: i64) -> Self {
        Self::precision_timestamp_units_unchecked(3, value)
    }

    pub fn precision_timestamp_micros(value: i64) -> Self {
        Self::precision_timestamp_units_unchecked(6, value)
    }

    pub fn precision_timestamp_nanos(value: i64) -> Self {
        Self::precision_timestamp_units_unchecked(9, value)
    }

    /// Build a timezone-less Substrait precision timestamp from a protobuf UTC
    /// timestamp. The epoch value is preserved, but timezone semantics are not.
    pub fn precision_timestamp(
        precision: i32,
        timestamp: ProtoTimestamp,
    ) -> Result<Self, ExtensionError> {
        let value = timestamp_to_units(precision, timestamp)?;
        Ok(Self::precision_timestamp_units_unchecked(precision, value))
    }

    /// Build a UTC Substrait precision timestamp from a protobuf UTC timestamp.
    pub fn precision_timestamp_tz_utc(
        precision: i32,
        timestamp: ProtoTimestamp,
    ) -> Result<Self, ExtensionError> {
        let value = timestamp_to_units(precision, timestamp)?;
        Ok(Self(Literal {
            literal_type: Some(LiteralType::PrecisionTimestampTz(PrecisionTimestamp {
                precision,
                value,
            })),
            nullable: false,
            type_variation_reference: 0,
        }))
    }

    fn precision_timestamp_units_unchecked(precision: i32, value: i64) -> Self {
        Self(Literal {
            literal_type: Some(LiteralType::PrecisionTimestamp(PrecisionTimestamp {
                precision,
                value,
            })),
            nullable: false,
            type_variation_reference: 0,
        })
    }
}

fn validate_precision(precision: i32) -> Result<(), ExtensionError> {
    if (0..=12).contains(&precision) {
        Ok(())
    } else {
        Err(ExtensionError::InvalidArgument(format!(
            "temporal precision must be between 0 and 12, got {precision}"
        )))
    }
}

fn timestamp_to_units(precision: i32, timestamp: ProtoTimestamp) -> Result<i64, ExtensionError> {
    validate_precision(precision)?;
    if precision > 9 {
        return Err(ExtensionError::InvalidArgument(format!(
            "protobuf Timestamp can only represent precision 0 through 9, got {precision}"
        )));
    }
    let timestamp = timestamp.try_normalize().map_err(|original| {
        ExtensionError::InvalidArgument(format!(
            "timestamp out of range or overflow (seconds={}, nanos={})",
            original.seconds, original.nanos
        ))
    })?;
    let scale = 10_i64.pow(precision as u32);
    let nanos_per_unit = 10_i32.pow((9 - precision) as u32);
    if timestamp.nanos % nanos_per_unit != 0 {
        return Err(ExtensionError::InvalidArgument(format!(
            "timestamp nanos {} are not exactly representable at precision {precision}",
            timestamp.nanos
        )));
    }
    let seconds_units = timestamp.seconds.checked_mul(scale).ok_or_else(|| {
        ExtensionError::InvalidArgument("timestamp value overflows i64".to_string())
    })?;
    seconds_units
        .checked_add(i64::from(timestamp.nanos / nanos_per_unit))
        .ok_or_else(|| ExtensionError::InvalidArgument("timestamp value overflows i64".to_string()))
}

impl TryFrom<Literal> for ExtensionLiteral {
    type Error = ExtensionError;

    #[allow(deprecated)]
    fn try_from(literal: Literal) -> Result<Self, Self::Error> {
        match literal.literal_type.as_ref() {
            Some(
                LiteralType::Date(_)
                | LiteralType::Time(_)
                | LiteralType::Timestamp(_)
                | LiteralType::PrecisionTime(_)
                | LiteralType::PrecisionTimestamp(_)
                | LiteralType::PrecisionTimestampTz(_),
            ) => Ok(Self(literal)),
            Some(other) => Err(ExtensionError::InvalidArgument(format!(
                "literal type {other:?} is not supported as an extension literal"
            ))),
            None => Err(ExtensionError::InvalidArgument(
                "literal is missing literal_type".to_string(),
            )),
        }
    }
}

impl From<ExtensionLiteral> for Literal {
    fn from(literal: ExtensionLiteral) -> Self {
        literal.0
    }
}

/// Represents extension arguments plus optional output columns.
///
/// Named arguments are stored in an [`IndexMap`] whose iteration order
/// determines display order. Extension [`super::Explainable::to_args()`]
/// implementations should insert named arguments in the order they should
/// appear in the text format.
#[derive(Debug, Clone, Default)]
pub struct ExtensionArgs {
    /// Positional arguments.
    pub positional: Vec<ExtensionValue>,
    /// Named arguments, displayed in the order they were inserted
    pub named: IndexMap<String, ExtensionValue>,
    /// Output columns for custom relation types.
    pub output_columns: Vec<ExtensionColumn>,
}

/// Helper struct for extracting named arguments with validation.
///
/// Tracks which arguments have been consumed. Callers **must** call
/// [`check_exhausted`](ArgsExtractor::check_exhausted) before dropping to
/// verify no unexpected arguments remain. In debug builds, dropping without
/// calling `check_exhausted` will panic (matching the [`RuleIter`](crate::parser::RuleIter) pattern).
pub struct ArgsExtractor<'a> {
    args: &'a ExtensionArgs,
    consumed: HashSet<&'a str>,
    checked: bool,
}

impl<'a> ArgsExtractor<'a> {
    /// Create a new extractor for the given arguments
    pub fn new(args: &'a ExtensionArgs) -> Self {
        Self {
            args,
            consumed: HashSet::new(),
            checked: false,
        }
    }

    /// Get a named argument value, marking it as consumed if found.
    pub fn get_named_arg(&mut self, name: &str) -> Option<&'a ExtensionValue> {
        match self.args.named.get_key_value(name) {
            Some((k, value)) => {
                self.consumed.insert(k);
                Some(value)
            }
            None => None,
        }
    }

    /// Get a named argument value or return an error
    /// Marks the argument as consumed if found
    pub fn expect_named_arg<T>(&mut self, name: &str) -> Result<T, ExtensionError>
    where
        T: TryFrom<&'a ExtensionValue>,
        T::Error: Into<ExtensionError>,
    {
        match self.get_named_arg(name) {
            Some(value) => T::try_from(value).map_err(Into::into),
            None => Err(ExtensionError::MissingArgument {
                name: name.to_string(),
            }),
        }
    }

    /// Get a named argument value or default
    /// Marks the argument as consumed if it exists in the source args
    pub fn get_named_or<T>(&mut self, name: &str, default: T) -> Result<T, ExtensionError>
    where
        T: TryFrom<&'a ExtensionValue>,
        T::Error: Into<ExtensionError>,
    {
        match self.get_named_arg(name) {
            Some(value) => T::try_from(value).map_err(Into::into),
            None => Ok(default),
        }
    }

    /// Check that all named arguments in the source have been consumed,
    /// returning an error if not.
    ///
    /// Must be called before the extractor is dropped, to validate that all
    /// args are correctly handled. In debug builds, dropping without calling
    /// this method will panic.
    pub fn check_exhausted(&mut self) -> Result<(), ExtensionError> {
        self.checked = true;

        let mut unknown_args = Vec::new();
        for name in self.args.named.keys() {
            if !self.consumed.contains(name.as_str()) {
                unknown_args.push(name.as_str());
            }
        }

        if unknown_args.is_empty() {
            Ok(())
        } else {
            // Sort for stable error messages
            unknown_args.sort();
            Err(ExtensionError::InvalidArgument(format!(
                "Unknown named arguments: {}",
                unknown_args.join(", ")
            )))
        }
    }
}

impl Drop for ArgsExtractor<'_> {
    fn drop(&mut self) {
        if self.checked || std::thread::panicking() {
            return;
        }
        // If we get here, the caller forgot to call check_exhausted().
        debug_assert!(
            false,
            "ArgsExtractor dropped without calling check_exhausted()"
        );
    }
}

/// A tuple-valued extension argument.
///
/// Tuple values preserve positional order and can be iterated by value or by
/// reference.
#[derive(Debug, Clone)]
pub struct TupleValue(Vec<ExtensionValue>);

impl TupleValue {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn iter(&self) -> std::slice::Iter<'_, ExtensionValue> {
        self.0.iter()
    }
}

impl<'a> IntoIterator for &'a TupleValue {
    type Item = &'a ExtensionValue;
    type IntoIter = std::slice::Iter<'a, ExtensionValue>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl IntoIterator for TupleValue {
    type Item = ExtensionValue;
    type IntoIter = std::vec::IntoIter<ExtensionValue>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl FromIterator<ExtensionValue> for TupleValue {
    fn from_iter<I: IntoIterator<Item = ExtensionValue>>(iter: I) -> Self {
        TupleValue(iter.into_iter().collect())
    }
}

impl From<Vec<ExtensionValue>> for TupleValue {
    fn from(items: Vec<ExtensionValue>) -> Self {
        TupleValue(items)
    }
}

/// Represents a value in extension arguments.
///
/// These values are the structured form of text-format extension arguments,
/// fully resolved - i.e. any additional context (such as function anchors etc)
/// are part of this struct itself.
#[derive(Debug, Clone)]
pub enum ExtensionValue {
    /// Untyped literals. These are not input or output with types (e.g. `2`,
    /// not `2:i64`), and suitable for protobuf extension fields that are not
    /// substrait types.
    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),

    /// Substrait expression value, including typed literals and field references.
    ///
    /// Use `TryFrom<&ExtensionValue> for Expr` when a handler accepts either an
    /// expression or a scalar value widened into an expression.
    Expr(Expr),
    /// Substrait literal value using typed literal syntax.
    Literal(ExtensionLiteral),
    /// Enum value (e.g. &CORE, &Inner) — the string holds the identifier
    /// without the `&` prefix
    Enum(String),
    /// Tuple of values, e.g. (&HASH, &RANGE) or (42, 'hello')
    Tuple(TupleValue),
    // TODO: Consider adding support for types as arguments. May need dedicated
    // syntax (`:typename`, perhaps?), as type names may not be distinguishable
    // from identifiers
}

/// The variant kind of an [`ExtensionValue`], used in diagnostics.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExtensionValueKind {
    String,
    Integer,
    Float,
    Boolean,
    Reference,
    Enum,
    Tuple,
    Expression,
    Literal,
}

impl fmt::Display for ExtensionValueKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExtensionValueKind::String => write!(f, "string"),
            ExtensionValueKind::Integer => write!(f, "integer"),
            ExtensionValueKind::Float => write!(f, "float"),
            ExtensionValueKind::Boolean => write!(f, "boolean"),
            ExtensionValueKind::Reference => write!(f, "reference"),
            ExtensionValueKind::Enum => write!(f, "enum"),
            ExtensionValueKind::Tuple => write!(f, "tuple"),
            ExtensionValueKind::Expression => write!(f, "expression"),
            ExtensionValueKind::Literal => write!(f, "literal"),
        }
    }
}

impl ExtensionValue {
    /// Return the variant kind of this value for structured diagnostics.
    pub fn kind(&self) -> ExtensionValueKind {
        match self {
            ExtensionValue::String(_) => ExtensionValueKind::String,
            ExtensionValue::Integer(_) => ExtensionValueKind::Integer,
            ExtensionValue::Float(_) => ExtensionValueKind::Float,
            ExtensionValue::Boolean(_) => ExtensionValueKind::Boolean,
            ExtensionValue::Expr(_) => ExtensionValueKind::Expression,
            ExtensionValue::Literal(_) => ExtensionValueKind::Literal,
            ExtensionValue::Enum(_) => ExtensionValueKind::Enum,
            ExtensionValue::Tuple(_) => ExtensionValueKind::Tuple,
        }
    }
}

impl From<Expr> for ExtensionValue {
    fn from(expr: Expr) -> Self {
        ExtensionValue::Expr(expr)
    }
}

impl From<ExtensionLiteral> for ExtensionValue {
    fn from(literal: ExtensionLiteral) -> Self {
        ExtensionValue::Literal(literal)
    }
}

impl From<proto::Expression> for ExtensionValue {
    fn from(expr: proto::Expression) -> Self {
        Expr::from(expr).into()
    }
}

impl From<Literal> for ExtensionValue {
    fn from(literal: Literal) -> Self {
        Expr::from(literal).into()
    }
}

impl From<Reference> for ExtensionValue {
    fn from(reference: Reference) -> Self {
        Expr::from(reference).into()
    }
}

impl From<i64> for ExtensionValue {
    fn from(value: i64) -> Self {
        ExtensionValue::Integer(value)
    }
}

impl From<f64> for ExtensionValue {
    fn from(value: f64) -> Self {
        ExtensionValue::Float(value)
    }
}

impl From<bool> for ExtensionValue {
    fn from(value: bool) -> Self {
        ExtensionValue::Boolean(value)
    }
}

impl From<String> for ExtensionValue {
    fn from(value: String) -> Self {
        ExtensionValue::String(value)
    }
}

impl From<&str> for ExtensionValue {
    fn from(value: &str) -> Self {
        ExtensionValue::String(value.to_string())
    }
}

fn invalid_type(expected: ExtensionValueKind, actual: &ExtensionValue) -> ExtensionError {
    ExtensionError::InvalidArgumentType {
        expected,
        actual: actual.kind(),
    }
}

impl<'a> TryFrom<&'a ExtensionValue> for &'a str {
    type Error = ExtensionError;

    fn try_from(value: &'a ExtensionValue) -> Result<&'a str, Self::Error> {
        match value {
            ExtensionValue::String(s) => Ok(s),
            v => Err(invalid_type(ExtensionValueKind::String, v)),
        }
    }
}

impl TryFrom<ExtensionValue> for String {
    type Error = ExtensionError;

    fn try_from(value: ExtensionValue) -> Result<String, Self::Error> {
        <&str>::try_from(&value).map(ToOwned::to_owned)
    }
}

/// Helper for extracting the identifier from an [`ExtensionValue::Enum`].
pub struct EnumValue(pub String);

impl<'a> TryFrom<&'a ExtensionValue> for EnumValue {
    type Error = ExtensionError;

    fn try_from(value: &'a ExtensionValue) -> Result<EnumValue, Self::Error> {
        match value {
            ExtensionValue::Enum(s) => Ok(EnumValue(s.clone())),
            v => Err(invalid_type(ExtensionValueKind::Enum, v)),
        }
    }
}

impl<'a> TryFrom<&'a ExtensionValue> for &'a TupleValue {
    type Error = ExtensionError;

    fn try_from(value: &'a ExtensionValue) -> Result<&'a TupleValue, Self::Error> {
        match value {
            ExtensionValue::Tuple(tv) => Ok(tv),
            v => Err(invalid_type(ExtensionValueKind::Tuple, v)),
        }
    }
}

impl TryFrom<&ExtensionValue> for ExtensionLiteral {
    type Error = ExtensionError;

    fn try_from(value: &ExtensionValue) -> Result<ExtensionLiteral, Self::Error> {
        match value {
            ExtensionValue::Literal(literal) => Ok(literal.clone()),
            v => Err(invalid_type(ExtensionValueKind::Literal, v)),
        }
    }
}

impl TryFrom<&ExtensionValue> for i64 {
    type Error = ExtensionError;

    fn try_from(value: &ExtensionValue) -> Result<i64, Self::Error> {
        match value {
            ExtensionValue::Integer(i) => Ok(*i),
            v => Err(invalid_type(ExtensionValueKind::Integer, v)),
        }
    }
}

impl TryFrom<&ExtensionValue> for f64 {
    type Error = ExtensionError;

    fn try_from(value: &ExtensionValue) -> Result<f64, Self::Error> {
        match value {
            ExtensionValue::Float(f) => Ok(*f),
            v => Err(invalid_type(ExtensionValueKind::Float, v)),
        }
    }
}

impl TryFrom<&ExtensionValue> for bool {
    type Error = ExtensionError;

    fn try_from(value: &ExtensionValue) -> Result<bool, Self::Error> {
        match value {
            ExtensionValue::Boolean(b) => Ok(*b),
            v => Err(invalid_type(ExtensionValueKind::Boolean, v)),
        }
    }
}

impl TryFrom<&ExtensionValue> for Reference {
    type Error = ExtensionError;

    fn try_from(value: &ExtensionValue) -> Result<Reference, Self::Error> {
        match value {
            ExtensionValue::Expr(expr) => expr
                .as_direct_reference()
                .ok_or_else(|| invalid_type(ExtensionValueKind::Reference, value)),
            v => Err(invalid_type(ExtensionValueKind::Reference, v)),
        }
    }
}

impl TryFrom<&ExtensionValue> for Expr {
    type Error = ExtensionError;

    fn try_from(value: &ExtensionValue) -> Result<Expr, Self::Error> {
        match value {
            ExtensionValue::Expr(e) => Ok(e.clone()),
            ExtensionValue::Literal(literal) => Ok(Expr::from(literal.to_proto())),
            // Untyped extension scalars are intentionally expression-compatible:
            // `arg=2` carries no syntax that distinguishes "configuration
            // integer" from "i64 literal expression". Scalar-specific
            // extraction (`i64`, `&str`, `bool`, etc.) still requires the scalar
            // variants, while expression extraction widens them to default
            // non-nullable Substrait literal expressions.
            ExtensionValue::Integer(i) => Ok(Expr::from(*i)),
            ExtensionValue::Float(f) => Ok(Expr::from(*f)),
            ExtensionValue::String(s) => Ok(Expr::from(s.as_str())),
            ExtensionValue::Boolean(b) => Ok(Expr::from(*b)),
            v => Err(invalid_type(ExtensionValueKind::Expression, v)),
        }
    }
}

/// Represents an output column specification.
///
/// These values mirror the text-format output column forms. Named columns keep
/// the parsed Substrait type protobuf so handlers can convert directly to
/// relation schemas.
#[derive(Debug, Clone)]
pub enum ExtensionColumn {
    /// Named column with a parsed Substrait type (e.g. `name:i64?`).
    Named {
        /// Column name as it appears in the extension relation output.
        name: String,
        /// Parsed Substrait type for the column.
        ///
        /// This uses the protobuf field name, hence the raw identifier.
        r#type: proto::Type,
    },
    /// Expression-compatible output column, including field references.
    Expr(Expr),
}

impl ExtensionArgs {
    /// Push a positional extension argument.
    pub fn push<T>(&mut self, value: T)
    where
        T: Into<ExtensionValue>,
    {
        self.positional.push(value.into());
    }

    /// Insert a named extension argument, returning any previous value.
    pub fn insert<K, V>(&mut self, name: K, value: V) -> Option<ExtensionValue>
    where
        K: Into<String>,
        V: Into<ExtensionValue>,
    {
        self.named.insert(name.into(), value.into())
    }

    /// Create an extractor for validating named arguments
    pub fn extractor(&self) -> ArgsExtractor<'_> {
        ArgsExtractor::new(self)
    }
}

#[cfg(test)]
mod tests {
    use prost_types::Timestamp as ProtoTimestamp;
    use substrait::proto::expression::RexType;
    use substrait::proto::expression::literal::LiteralType;

    use super::{Expr, ExtensionLiteral, ExtensionValue};

    fn literal_type(literal: &ExtensionLiteral) -> &LiteralType {
        literal
            .as_proto()
            .literal_type
            .as_ref()
            .expect("literal_type")
    }

    #[test]
    fn precision_timestamp_tz_utc_converts_protobuf_timestamp_units() {
        let literal = ExtensionLiteral::precision_timestamp_tz_utc(
            9,
            ProtoTimestamp {
                seconds: 1,
                nanos: 123_456_789,
            },
        )
        .expect("timestamp literal");

        match literal_type(&literal) {
            LiteralType::PrecisionTimestampTz(value) => {
                assert_eq!(value.precision, 9);
                assert_eq!(value.value, 1_123_456_789);
            }
            other => panic!("Expected PrecisionTimestampTz, got {other:?}"),
        }
    }

    #[test]
    fn precision_timestamp_converts_protobuf_timestamp_to_naive_literal() {
        let literal = ExtensionLiteral::precision_timestamp(
            6,
            ProtoTimestamp {
                seconds: 1,
                nanos: 123_456_000,
            },
        )
        .expect("timestamp literal");

        match literal_type(&literal) {
            LiteralType::PrecisionTimestamp(value) => {
                assert_eq!(value.precision, 6);
                assert_eq!(value.value, 1_123_456);
            }
            other => panic!("Expected PrecisionTimestamp, got {other:?}"),
        }
    }

    #[test]
    fn precision_timestamp_from_protobuf_rejects_lossy_precision() {
        let err = ExtensionLiteral::precision_timestamp_tz_utc(
            6,
            ProtoTimestamp {
                seconds: 1,
                nanos: 123_456_789,
            },
        )
        .expect_err("lossy timestamp should be rejected");

        assert!(
            err.to_string()
                .contains("not exactly representable at precision 6"),
            "{err}"
        );
    }

    #[test]
    fn precision_timestamp_from_protobuf_rejects_precision_above_nanos() {
        let err = ExtensionLiteral::precision_timestamp_tz_utc(
            10,
            ProtoTimestamp {
                seconds: 1,
                nanos: 123_456_789,
            },
        )
        .expect_err("protobuf timestamp cannot represent precision 10");

        assert!(
            err.to_string()
                .contains("can only represent precision 0 through 9"),
            "{err}"
        );
    }

    #[test]
    fn precision_timestamp_convenience_constructors_use_expected_precision() {
        let cases = [
            (ExtensionLiteral::precision_timestamp_seconds(7), 0, 7),
            (ExtensionLiteral::precision_timestamp_millis(7), 3, 7),
            (ExtensionLiteral::precision_timestamp_micros(7), 6, 7),
            (ExtensionLiteral::precision_timestamp_nanos(7), 9, 7),
        ];

        for (literal, expected_precision, expected_value) in cases {
            match literal_type(&literal) {
                LiteralType::PrecisionTimestamp(value) => {
                    assert_eq!(value.precision, expected_precision);
                    assert_eq!(value.value, expected_value);
                }
                other => panic!("Expected PrecisionTimestamp, got {other:?}"),
            }
        }
    }

    #[test]
    fn extension_literal_extracts_directly_and_widens_to_expr() {
        let value = ExtensionValue::from(ExtensionLiteral::precision_timestamp_micros(42));

        let literal = ExtensionLiteral::try_from(&value).expect("extension literal");
        match literal_type(&literal) {
            LiteralType::PrecisionTimestamp(timestamp) => {
                assert_eq!(timestamp.precision, 6);
                assert_eq!(timestamp.value, 42);
            }
            other => panic!("Expected PrecisionTimestamp, got {other:?}"),
        }

        let expr = Expr::try_from(&value).expect("expression widening");
        assert!(matches!(
            expr.as_proto().rex_type.as_ref(),
            Some(RexType::Literal(_))
        ));
    }
}
