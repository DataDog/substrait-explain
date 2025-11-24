//! Core extension data structures without parser dependencies
//!
//! This module contains the core data structures for extension arguments,
//! values, and columns without any parser or textify dependencies.

use std::collections::HashMap;

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
    // TODO: Function call expression
    // Expression(…)
}

/// Represents an output column specification
#[derive(Debug, Clone)]
pub enum ExtensionColumn {
    /// Named column with type (name:type)
    Named { name: String, type_spec: String },
    /// Field reference ($0, $1, etc.)
    Reference(i32),
    // TODO: Expression (function call, literal, etc.)
    // Expression(…),
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
            named: HashMap::new(),
            output_columns: Vec::new(),
            relation_type,
            argument_order: Vec::new(),
        }
    }

    /// Get a named argument value
    pub fn get_named_arg(&self, name: &str) -> Option<&ExtensionValue> {
        self.named.get(name)
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
