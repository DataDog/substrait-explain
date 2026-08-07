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
use std::slice::Iter as SliceIter;
use std::vec::IntoIter as VecIntoIter;
use std::{fmt, thread};

use indexmap::IndexMap;
use substrait::proto;
use substrait::proto::expression::field_reference::ReferenceType;
use substrait::proto::expression::literal::LiteralType;
use substrait::proto::expression::{RexType, reference_segment};

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
    /// Create a direct field-reference expression (`$N`).
    pub fn field(index: i32) -> Self {
        Reference(index).into()
    }

    /// Borrow the underlying Substrait expression protobuf.
    pub fn as_proto(&self) -> &proto::Expression {
        self.0.as_ref()
    }

    /// Clone the underlying Substrait expression protobuf.
    pub fn to_proto(&self) -> proto::Expression {
        self.as_proto().clone()
    }

    /// If this expression is a direct field reference (`$N`), return it.
    pub fn as_direct_reference(&self) -> Option<i32> {
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
        Some(field.field)
    }
}

impl From<proto::Expression> for Expr {
    fn from(expr: proto::Expression) -> Self {
        Expr(Box::new(expr))
    }
}

impl From<proto::expression::Literal> for Expr {
    fn from(literal: proto::expression::Literal) -> Self {
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
        proto::expression::Literal {
            literal_type: Some(LiteralType::I64(value)),
            nullable: false,
            type_variation_reference: 0,
        }
        .into()
    }
}

impl From<f64> for Expr {
    fn from(value: f64) -> Self {
        proto::expression::Literal {
            literal_type: Some(LiteralType::Fp64(value)),
            nullable: false,
            type_variation_reference: 0,
        }
        .into()
    }
}

impl From<bool> for Expr {
    fn from(value: bool) -> Self {
        proto::expression::Literal {
            literal_type: Some(LiteralType::Boolean(value)),
            nullable: false,
            type_variation_reference: 0,
        }
        .into()
    }
}

impl From<String> for Expr {
    fn from(value: String) -> Self {
        proto::expression::Literal {
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
/// calling `check_exhausted` will panic. This catches [`Explainable`](super::Explainable)
/// implementations that forget to reject unexpected named arguments.
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

    /// Get a named argument converted to `T`, or `None` if it is absent.
    ///
    /// Marks the argument as consumed if it exists in the source args.
    pub fn get_named<T>(&mut self, name: &str) -> Result<Option<T>, ExtensionError>
    where
        T: TryFrom<&'a ExtensionValue>,
        T::Error: Into<ExtensionError>,
    {
        self.get_named_arg(name)
            .map(|value| {
                T::try_from(value).map_err(|error| ExtensionError::NamedArgumentConversion {
                    name: name.to_string(),
                    source: Box::new(error.into()),
                })
            })
            .transpose()
    }

    /// Get a required named argument converted to `T`.
    ///
    /// Marks the argument as consumed if found.
    pub fn expect_named<T>(&mut self, name: &str) -> Result<T, ExtensionError>
    where
        T: TryFrom<&'a ExtensionValue>,
        T::Error: Into<ExtensionError>,
    {
        self.get_named(name)?
            .ok_or_else(|| ExtensionError::MissingArgument {
                name: name.to_string(),
            })
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
        if self.checked || thread::panicking() {
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

    pub fn iter(&self) -> SliceIter<'_, ExtensionValue> {
        self.0.iter()
    }
}

impl<'a> IntoIterator for &'a TupleValue {
    type Item = &'a ExtensionValue;
    type IntoIter = SliceIter<'a, ExtensionValue>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl IntoIterator for TupleValue {
    type Item = ExtensionValue;
    type IntoIter = VecIntoIter<ExtensionValue>;

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
    /// An untyped null literal.
    Null,
    /// A conversion or formatting error preserved for best-effort textification.
    Error(ExtensionError),

    /// Substrait expression value, including typed literals and field references.
    ///
    /// Use `TryFrom<&ExtensionValue> for Expr` when a handler accepts either an
    /// expression or a scalar value widened into an expression.
    Expr(Expr),
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
    Null,
    Error,
    Reference,
    Enum,
    Tuple,
    Expression,
}

impl fmt::Display for ExtensionValueKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExtensionValueKind::String => write!(f, "string"),
            ExtensionValueKind::Integer => write!(f, "integer"),
            ExtensionValueKind::Float => write!(f, "float"),
            ExtensionValueKind::Boolean => write!(f, "boolean"),
            ExtensionValueKind::Null => write!(f, "null"),
            ExtensionValueKind::Error => write!(f, "error"),
            ExtensionValueKind::Reference => write!(f, "reference"),
            ExtensionValueKind::Enum => write!(f, "enum"),
            ExtensionValueKind::Tuple => write!(f, "tuple"),
            ExtensionValueKind::Expression => write!(f, "expression"),
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
            ExtensionValue::Null => ExtensionValueKind::Null,
            ExtensionValue::Error(_) => ExtensionValueKind::Error,
            ExtensionValue::Expr(_) => ExtensionValueKind::Expression,
            ExtensionValue::Enum(_) => ExtensionValueKind::Enum,
            ExtensionValue::Tuple(_) => ExtensionValueKind::Tuple,
        }
    }
}

impl From<ExtensionError> for ExtensionValue {
    fn from(error: ExtensionError) -> Self {
        ExtensionValue::Error(error)
    }
}

impl From<Expr> for ExtensionValue {
    fn from(expr: Expr) -> Self {
        ExtensionValue::Expr(expr)
    }
}

impl From<proto::Expression> for ExtensionValue {
    fn from(expr: proto::Expression) -> Self {
        Expr::from(expr).into()
    }
}

impl From<proto::expression::Literal> for ExtensionValue {
    fn from(literal: proto::expression::Literal) -> Self {
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

impl ExtensionError {
    fn invalid_type(expected: ExtensionValueKind, actual: &ExtensionValue) -> Self {
        match actual {
            ExtensionValue::Error(source) => Self::ArgumentConversion {
                expected,
                source: Box::new(source.clone()),
            },
            _ => Self::InvalidArgumentType {
                expected,
                actual: actual.kind(),
            },
        }
    }
}

impl<'a> TryFrom<&'a ExtensionValue> for &'a str {
    type Error = ExtensionError;

    fn try_from(value: &'a ExtensionValue) -> Result<&'a str, Self::Error> {
        match value {
            ExtensionValue::String(s) => Ok(s),
            v => Err(ExtensionError::invalid_type(ExtensionValueKind::String, v)),
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
            v => Err(ExtensionError::invalid_type(ExtensionValueKind::Enum, v)),
        }
    }
}

impl<'a> TryFrom<&'a ExtensionValue> for &'a TupleValue {
    type Error = ExtensionError;

    fn try_from(value: &'a ExtensionValue) -> Result<&'a TupleValue, Self::Error> {
        match value {
            ExtensionValue::Tuple(tv) => Ok(tv),
            v => Err(ExtensionError::invalid_type(ExtensionValueKind::Tuple, v)),
        }
    }
}

impl TryFrom<&ExtensionValue> for i64 {
    type Error = ExtensionError;

    fn try_from(value: &ExtensionValue) -> Result<i64, Self::Error> {
        match value {
            ExtensionValue::Integer(i) => Ok(*i),
            v => Err(ExtensionError::invalid_type(ExtensionValueKind::Integer, v)),
        }
    }
}

impl TryFrom<&ExtensionValue> for f64 {
    type Error = ExtensionError;

    fn try_from(value: &ExtensionValue) -> Result<f64, Self::Error> {
        match value {
            ExtensionValue::Float(f) => Ok(*f),
            v => Err(ExtensionError::invalid_type(ExtensionValueKind::Float, v)),
        }
    }
}

impl TryFrom<&ExtensionValue> for bool {
    type Error = ExtensionError;

    fn try_from(value: &ExtensionValue) -> Result<bool, Self::Error> {
        match value {
            ExtensionValue::Boolean(b) => Ok(*b),
            v => Err(ExtensionError::invalid_type(ExtensionValueKind::Boolean, v)),
        }
    }
}

impl TryFrom<&ExtensionValue> for Reference {
    type Error = ExtensionError;

    fn try_from(value: &ExtensionValue) -> Result<Reference, Self::Error> {
        match value {
            ExtensionValue::Expr(expr) => expr
                .as_direct_reference()
                .map(Reference)
                .ok_or_else(|| ExtensionError::invalid_type(ExtensionValueKind::Reference, value)),
            v => Err(ExtensionError::invalid_type(
                ExtensionValueKind::Reference,
                v,
            )),
        }
    }
}

impl TryFrom<&ExtensionValue> for Expr {
    type Error = ExtensionError;

    fn try_from(value: &ExtensionValue) -> Result<Expr, Self::Error> {
        match value {
            ExtensionValue::Expr(e) => Ok(e.clone()),
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
            v => Err(ExtensionError::invalid_type(
                ExtensionValueKind::Expression,
                v,
            )),
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

impl ExtensionColumn {
    /// Create an expression output column that references an existing input field (`$N`).
    pub fn field(index: i32) -> Self {
        Self::Expr(Expr::field(index))
    }
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
    use super::*;

    #[test]
    fn get_named_converts_present_values_and_returns_none_for_missing_values() {
        let mut args = ExtensionArgs::default();
        args.insert("count", 8_i64);
        let mut extractor = args.extractor();

        assert_eq!(extractor.get_named::<i64>("count").unwrap(), Some(8));
        assert_eq!(extractor.get_named::<i64>("missing").unwrap(), None);
        assert!(extractor.check_exhausted().is_ok());
    }

    #[test]
    fn get_named_contextualizes_conversion_errors() {
        let mut args = ExtensionArgs::default();
        args.insert("count", ExtensionValue::Null);
        let mut extractor = args.extractor();

        let error = extractor
            .get_named::<i64>("count")
            .expect_err("null should not convert to i64");

        assert_eq!(
            error.to_string(),
            "Invalid named argument 'count': Invalid argument: expected integer, got null"
        );
        assert!(extractor.check_exhausted().is_ok());
    }

    #[test]
    fn error_value_reports_expected_type_and_source_when_extracted() {
        let value = ExtensionValue::Error(ExtensionError::Custom("bad value".to_string()));

        let error = i64::try_from(&value).expect_err("error value should not convert");

        assert_eq!(
            error.to_string(),
            "Cannot convert argument to integer: bad value"
        );
        assert!(matches!(
            &error,
            ExtensionError::ArgumentConversion {
                expected: ExtensionValueKind::Integer,
                source,
            } if matches!(source.as_ref(), ExtensionError::Custom(message) if message == "bad value")
        ));
    }

    #[test]
    fn expect_named_reports_missing_argument_name() {
        let args = ExtensionArgs::default();
        let mut extractor = args.extractor();

        let error = extractor
            .expect_named::<i64>("count")
            .expect_err("missing argument should fail");

        assert_eq!(error.to_string(), "Missing required argument: count");
        assert!(extractor.check_exhausted().is_ok());
    }
}
