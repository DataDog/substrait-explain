//! Core extension data structures for extension arguments, values, and columns.

use std::collections::HashSet;
use std::fmt;

use indexmap::IndexMap;
use substrait::proto;

use super::ExtensionError;
use crate::textify::expressions::Reference;
use crate::textify::types::escaped;

/// A Substrait expression carried as an extension argument or output column.
///
/// Boxed because `proto::Expression` is large (multiple `Vec` fields in
/// variants like `ScalarFunction`).
#[derive(Debug, Clone)]
pub struct Expr(pub Box<proto::Expression>);

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<expression>")
    }
}

/// Represents the arguments and output columns for an extension relation.
///
/// Named arguments are stored in an [`IndexMap`] whose iteration order
/// determines display order. Extension [`super::Explainable::to_args()`]
/// implementations should insert named arguments in the order they should
/// appear in the text format.
#[derive(Debug, Clone)]
pub struct ExtensionArgs {
    /// Positional arguments (expressions, literals, references)
    pub positional: Vec<ExtensionValue>,
    /// Named arguments, displayed in the order they were inserted
    pub named: IndexMap<String, ExtensionValue>,
    /// Output columns (named columns, references, or expressions)
    pub output_columns: Vec<ExtensionColumn>,
    /// The type of extension relation (Leaf/Single/Multi)
    pub relation_type: ExtensionRelationType,
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

/// Represents a value in extension arguments.
#[derive(Debug, Clone)]
pub enum ExtensionValue {
    /// String literal value
    String(String),
    /// Integer literal value
    Integer(i64),
    /// Float literal value
    Float(f64),
    /// Boolean literal value
    Boolean(bool),
    /// Field reference ($0, $1, etc.)
    Reference(i32),
    /// Enum value (e.g. &CORE, &Inner) — the string holds the identifier
    /// without the `&` prefix
    Enum(String),
    /// A Substrait expression (e.g. `add($0, $1)`)
    Expression(Expr),
    // TODO: Consider adding support for types as arguments. May need dedicated
    // syntax (`:typename`, perhaps?), as type names may not be distinguishable
    // from identifiers
}

impl fmt::Display for ExtensionValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExtensionValue::String(s) => write!(f, "String({})", escaped(s)),
            ExtensionValue::Integer(i) => write!(f, "Integer({})", i),
            ExtensionValue::Float(n) => write!(f, "Float({})", n),
            ExtensionValue::Boolean(b) => write!(f, "Boolean({})", b),
            ExtensionValue::Reference(r) => write!(f, "Reference({})", r),
            ExtensionValue::Enum(e) => write!(f, "Enum(&{})", e),
            ExtensionValue::Expression(e) => write!(f, "Expression({})", e),
        }
    }
}

impl<'a> TryFrom<&'a ExtensionValue> for &'a str {
    type Error = ExtensionError;

    fn try_from(value: &'a ExtensionValue) -> Result<&'a str, Self::Error> {
        match value {
            ExtensionValue::String(s) => Ok(s),
            v => Err(ExtensionError::InvalidArgument(format!(
                "Expected string, got {v}",
            ))),
        }
    }
}

impl TryFrom<ExtensionValue> for String {
    type Error = ExtensionError;

    fn try_from(value: ExtensionValue) -> Result<String, Self::Error> {
        match value {
            ExtensionValue::String(s) => Ok(s),
            v => Err(ExtensionError::InvalidArgument(format!(
                "Expected string, got {v}",
            ))),
        }
    }
}

/// Helper for extracting the identifier from an [`ExtensionValue::Enum`].
pub struct EnumValue(pub String);

impl<'a> TryFrom<&'a ExtensionValue> for EnumValue {
    type Error = ExtensionError;

    fn try_from(value: &'a ExtensionValue) -> Result<EnumValue, Self::Error> {
        match value {
            ExtensionValue::Enum(s) => Ok(EnumValue(s.clone())),
            v => Err(ExtensionError::InvalidArgument(format!(
                "Expected enum, got {v}",
            ))),
        }
    }
}

impl TryFrom<&ExtensionValue> for i64 {
    type Error = ExtensionError;

    fn try_from(value: &ExtensionValue) -> Result<i64, Self::Error> {
        match value {
            &ExtensionValue::Integer(i) => Ok(i),
            v => Err(ExtensionError::InvalidArgument(format!(
                "Expected integer, got {v}",
            ))),
        }
    }
}

impl TryFrom<&ExtensionValue> for f64 {
    type Error = ExtensionError;

    fn try_from(value: &ExtensionValue) -> Result<f64, Self::Error> {
        match value {
            &ExtensionValue::Float(f) => Ok(f),
            v => Err(ExtensionError::InvalidArgument(format!(
                "Expected float, got {v}",
            ))),
        }
    }
}

impl TryFrom<&ExtensionValue> for bool {
    type Error = ExtensionError;

    fn try_from(value: &ExtensionValue) -> Result<bool, Self::Error> {
        match value {
            &ExtensionValue::Boolean(b) => Ok(b),
            v => Err(ExtensionError::InvalidArgument(format!(
                "Expected boolean, got {v}",
            ))),
        }
    }
}

impl TryFrom<&ExtensionValue> for Reference {
    type Error = ExtensionError;

    fn try_from(value: &ExtensionValue) -> Result<Reference, Self::Error> {
        match value {
            &ExtensionValue::Reference(r) => Ok(Reference(r)),
            v => Err(ExtensionError::InvalidArgument(format!(
                "Expected reference, got {v}",
            ))),
        }
    }
}

impl TryFrom<&ExtensionValue> for Expr {
    type Error = ExtensionError;

    fn try_from(value: &ExtensionValue) -> Result<Expr, Self::Error> {
        match value {
            ExtensionValue::Expression(e) => Ok(e.clone()),
            v => Err(ExtensionError::InvalidArgument(format!(
                "Expected expression, got {v}",
            ))),
        }
    }
}

/// Represents an output column specification.
#[derive(Debug, Clone)]
pub enum ExtensionColumn {
    /// Named column with a parsed Substrait type (e.g. `name:i64?`)
    Named { name: String, r#type: proto::Type },
    /// Field reference ($0, $1, etc.)
    Reference(i32),
    /// Expression column
    Expression(Expr),
}

/// Extension relation types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExtensionRelationType {
    /// Extension leaf relation - no input children
    Leaf,
    /// Extension single relation - exactly one input child
    Single,
    /// Extension multi relation - zero or more input children
    Multi,
}

impl std::str::FromStr for ExtensionRelationType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "ExtensionLeaf" => Ok(ExtensionRelationType::Leaf),
            "ExtensionSingle" => Ok(ExtensionRelationType::Single),
            "ExtensionMulti" => Ok(ExtensionRelationType::Multi),
            _ => Err(format!("Unknown extension relation type: {}", s)),
        }
    }
}

impl ExtensionRelationType {
    /// Get the string representation used in the text format
    pub fn as_str(&self) -> &'static str {
        match self {
            ExtensionRelationType::Leaf => "ExtensionLeaf",
            ExtensionRelationType::Single => "ExtensionSingle",
            ExtensionRelationType::Multi => "ExtensionMulti",
        }
    }

    /// Validate that the child count matches this relation type
    pub fn validate_child_count(&self, child_count: usize) -> Result<(), String> {
        match self {
            ExtensionRelationType::Leaf => {
                if child_count == 0 {
                    Ok(())
                } else {
                    Err(format!(
                        "ExtensionLeaf should have no input children, got {child_count}"
                    ))
                }
            }
            ExtensionRelationType::Single => {
                if child_count == 1 {
                    Ok(())
                } else {
                    Err(format!(
                        "ExtensionSingle should have exactly 1 input child, got {child_count}"
                    ))
                }
            }
            ExtensionRelationType::Multi => {
                // ExtensionMulti relations accept zero or more children.
                Ok(())
            }
        }
    }
}

// Note: create_rel is implemented in parser/extensions.rs to avoid
// pulling in protobuf dependencies in the core args module

impl ExtensionArgs {
    /// Create a new empty ExtensionArgs
    pub fn new(relation_type: ExtensionRelationType) -> Self {
        Self {
            positional: Vec::new(),
            named: IndexMap::new(),
            output_columns: Vec::new(),
            relation_type,
        }
    }

    /// Create an extractor for validating named arguments
    pub fn extractor(&self) -> ArgsExtractor<'_> {
        ArgsExtractor::new(self)
    }
}

#[cfg(test)]
mod tests {
    use super::ExtensionRelationType;

    #[test]
    fn extension_multi_allows_zero_children() {
        assert!(ExtensionRelationType::Multi.validate_child_count(0).is_ok());
    }

    #[test]
    fn extension_multi_allows_single_child() {
        assert!(ExtensionRelationType::Multi.validate_child_count(1).is_ok());
    }

    #[test]
    fn extension_multi_allows_multiple_children() {
        assert!(ExtensionRelationType::Multi.validate_child_count(3).is_ok());
    }

    #[test]
    fn extension_single_rejects_wrong_child_counts() {
        assert!(
            ExtensionRelationType::Single
                .validate_child_count(0)
                .is_err()
        );
        assert!(
            ExtensionRelationType::Single
                .validate_child_count(2)
                .is_err()
        );
    }
}
