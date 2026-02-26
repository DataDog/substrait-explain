//! Core extension data structures without parser dependencies
//!
//! This module contains the core data structures for extension arguments,
//! values, and columns without any parser or textify dependencies.

use std::collections::{HashMap, HashSet};
use std::fmt;

use super::ExtensionError;
use crate::textify::expressions::Reference;
use crate::textify::types::escaped;

/// Placeholder for a future expression implementation.
/// Holds the raw text of the parsed expression. The inner field is private —
/// this type will be replaced with a proper expression AST in the future.
#[derive(Debug, Clone)]
pub(crate) struct RawExpression {
    text: String,
}

impl RawExpression {
    pub fn new(text: String) -> Self {
        Self { text }
    }
}

impl fmt::Display for RawExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.text)
    }
}

/// Represents the arguments and output columns for an extension relation
#[derive(Debug, Clone)]
pub struct ExtensionArgs {
    /// Positional arguments (expressions, literals, references)
    pub positional: Vec<ExtensionValue>,
    /// Named arguments (key=value pairs)
    pub named: HashMap<String, ExtensionValue>,
    /// Output columns (can be named columns, references, or expressions)
    pub output_columns: Vec<ExtensionColumn>,
    /// The type of extension relation (Leaf/Single/Multi)
    pub relation_type: ExtensionRelationType,
    /// Preferred argument order for textification. Unmentioned arguments will
    /// be sorted alphabetically (so a partial or empy vector is valid). NOTE:
    /// This is a _preferred_ order for display; order should not be relied on
    /// for semantic meaning.
    pub argument_order: Vec<String>,
}

/// Helper struct for extracting named arguments with validation
pub struct ArgsExtractor<'a> {
    args: &'a ExtensionArgs,
    // Track keys that have been successfully requested/accessed
    consumed: HashSet<&'a str>,
}

impl<'a> ArgsExtractor<'a> {
    /// Create a new extractor for the given arguments
    pub fn new(args: &'a ExtensionArgs) -> Self {
        Self {
            args,
            consumed: HashSet::new(),
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
            None => Err(ExtensionError::MissingArgument(name.to_string())),
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

    /// Check that all named arguments in the source have been consumed
    pub fn check_exhausted(&self) -> Result<(), ExtensionError> {
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

/// Represents a value in extension arguments
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
    /// Expression (function call, etc.) — not yet fully supported, hence the
    /// private interface.
    #[allow(private_interfaces)]
    Expression(RawExpression),
}

impl fmt::Display for ExtensionValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExtensionValue::String(s) => write!(f, "String({})", escaped(s)),
            ExtensionValue::Integer(i) => write!(f, "Integer({})", i),
            ExtensionValue::Float(n) => write!(f, "Float({})", n),
            ExtensionValue::Boolean(b) => write!(f, "Boolean({})", b),
            ExtensionValue::Reference(r) => write!(f, "Reference({})", r),
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

/// Represents an output column specification
#[derive(Debug, Clone)]
pub enum ExtensionColumn {
    /// Named column with type (name:type)
    Named { name: String, type_spec: String },
    /// Field reference ($0, $1, etc.)
    Reference(i32),
    /// Expression column — not yet fully supported, hence the private
    /// interface.
    #[allow(private_interfaces)]
    Expression(RawExpression),
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

// Note: relation construction lives in parser/lower/extensions.rs so this
// core args module stays parser- and protobuf-agnostic.

impl ExtensionArgs {
    /// Create a new empty ExtensionArgs
    pub fn new(relation_type: ExtensionRelationType) -> Self {
        Self {
            positional: Vec::new(),
            named: HashMap::new(),
            output_columns: Vec::new(),
            relation_type,
            argument_order: Vec::new(),
        }
    }

    /// Create an extractor for these arguments
    pub fn extractor(&self) -> ArgsExtractor<'_> {
        ArgsExtractor::new(self)
    }

    /// Add a named argument
    pub fn add_named_arg(&mut self, name: String, value: ExtensionValue) {
        self.named.insert(name, value);
    }

    /// Add a positional argument
    pub fn add_positional_arg(&mut self, value: ExtensionValue) {
        self.positional.push(value);
    }

    /// Add an output column
    pub fn add_output_column(&mut self, column: ExtensionColumn) {
        self.output_columns.push(column);
    }

    /// Get a vector of named arguments in the specified order.
    /// Returns arguments in the specified order first, then remaining arguments alphabetically.
    /// Uses the argument_order field from the extension type (empty vec means alphabetical order).
    pub fn ordered_named_args(&self) -> Vec<(&str, &ExtensionValue)> {
        let mut result = Vec::new();

        if self.argument_order.is_empty() {
            // Default: alphabetical order
            let mut args: Vec<_> = self.named.iter().collect();
            args.sort_by_key(|(name, _)| name.as_str());
            for (name, value) in args {
                result.push((name.as_str(), value));
            }
            return result;
        }
        // First, add arguments in the specified order
        for arg_name in &self.argument_order {
            if let Some(value) = self.named.get(arg_name) {
                result.push((arg_name.as_str(), value));
            }
        }

        // Then add any remaining arguments alphabetically
        let mut remaining_args: Vec<_> = self
            .named
            .iter()
            // Filter out arguments that are in the argument_order - already included above
            .filter(|(name, _)| !self.argument_order.contains(name))
            .collect();
        remaining_args.sort_by_key(|(name, _)| name.as_str());

        for (name, value) in remaining_args {
            result.push((name.as_str(), value));
        }

        result
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
