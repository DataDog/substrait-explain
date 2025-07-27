//! Warning system for parser diagnostics.
//!
//! This module provides types for collecting and handling non-fatal issues
//! encountered during parsing, such as unregistered extensions.

use std::fmt;

use substrait::proto::Plan;

/// Configuration for how the parser should handle unregistered extensions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum UnregisteredExtensionMode {
    /// Fail parsing immediately when encountering an unregistered extension.
    /// This is the safest default for development and ensures all extensions
    /// are properly configured.
    #[default]
    Error,
    /// Continue parsing but collect warnings for unregistered extensions.
    /// This is useful in production when you want to handle plans with
    /// unknown extensions gracefully.
    Warn,
    /// Silently ignore unregistered extensions and use placeholder values.
    /// Only recommended for special cases where you need maximum compatibility.
    Ignore,
}

/// The type of extension relation where an issue occurred.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExtensionRelationType {
    /// Extension leaf relation (no input relations)
    Leaf,
    /// Extension single relation (one input relation)
    Single,
    /// Extension multi relation (multiple input relations)
    Multi,
}

impl fmt::Display for ExtensionRelationType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExtensionRelationType::Leaf => write!(f, "ExtensionLeaf"),
            ExtensionRelationType::Single => write!(f, "ExtensionSingle"),
            ExtensionRelationType::Multi => write!(f, "ExtensionMulti"),
        }
    }
}

/// Location information for where a warning occurred in the source.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceLocation {
    /// The line number where the issue occurred (1-based)
    pub line: i64,
    /// Contextual information about where in the parsing the issue occurred
    pub context: String,
}

/// Specific types of parsing warnings.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseWarningKind {
    /// An extension was referenced but not registered in the extension registry.
    UnregisteredExtension {
        /// The name of the unregistered extension
        extension_name: String,
        /// The type of extension relation where this occurred
        relation_type: ExtensionRelationType,
    },
    /// A field reference is out of bounds for the available input fields.
    OutOfBoundsFieldReference {
        /// The field reference (e.g., "$5")
        field_reference: String,
        /// The maximum valid field index (0-based)
        max_valid_index: usize,
        /// Context where this occurred (e.g., "Filter condition")
        context: String,
    },
    // Future warning types can be added here:
    // DeprecatedSyntax { old_syntax: String, suggested: String },
    // UnusedExtension { extension_name: String },
}

/// A non-fatal issue encountered during parsing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseWarning {
    /// The specific type of warning
    pub kind: ParseWarningKind,
    /// Where in the source the warning occurred
    pub location: SourceLocation,
}

impl ParseWarning {
    /// Create a new unregistered extension warning.
    pub fn unregistered_extension(
        extension_name: String,
        relation_type: ExtensionRelationType,
        line: i64,
        context: String,
    ) -> Self {
        Self {
            kind: ParseWarningKind::UnregisteredExtension {
                extension_name,
                relation_type,
            },
            location: SourceLocation { line, context },
        }
    }

    /// Create a new out-of-bounds field reference warning.
    pub fn out_of_bounds_field_reference(
        field_reference: String,
        max_valid_index: usize,
        context: String,
        line: i64,
        location_context: String,
    ) -> Self {
        Self {
            kind: ParseWarningKind::OutOfBoundsFieldReference {
                field_reference,
                max_valid_index,
                context,
            },
            location: SourceLocation {
                line,
                context: location_context,
            },
        }
    }
}

impl fmt::Display for ParseWarning {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ParseWarningKind::UnregisteredExtension {
                extension_name,
                relation_type,
            } => {
                write!(
                    f,
                    "Unregistered extension '{}' in {} relation at line {} ({})",
                    extension_name, relation_type, self.location.line, self.location.context
                )
            }
            ParseWarningKind::OutOfBoundsFieldReference {
                field_reference,
                max_valid_index,
                context,
            } => {
                write!(
                    f,
                    "Field reference '{}' is out of bounds (max valid: ${}) in {} at line {} ({})",
                    field_reference,
                    max_valid_index,
                    context,
                    self.location.line,
                    self.location.context
                )
            }
        }
    }
}

/// Configuration options for the parser.
#[derive(Debug, Clone)]
pub struct ParserConfig {
    /// How to handle unregistered extensions
    pub unregistered_extension_mode: UnregisteredExtensionMode,
    // Future configuration options can be added here:
    // pub strict_type_checking: bool,
    // pub allow_deprecated_syntax: bool,
}

impl Default for ParserConfig {
    fn default() -> Self {
        Self {
            unregistered_extension_mode: UnregisteredExtensionMode::Error,
        }
    }
}

/// The result of parsing a Substrait plan.
#[derive(Debug)]
pub struct ParseResult {
    /// The successfully parsed plan
    pub plan: Plan,
    /// Any warnings encountered during parsing
    pub warnings: Vec<ParseWarning>,
}

impl ParseResult {
    /// Create a new parse result with the given plan and warnings.
    pub fn new(plan: Plan, warnings: Vec<ParseWarning>) -> Self {
        Self { plan, warnings }
    }

    /// Get the plan, ignoring any warnings.
    pub fn plan(self) -> Plan {
        self.plan
    }

    /// Get the plan only if there are no warnings.
    /// Returns the warnings as an error if any were encountered.
    pub fn plan_without_warnings(self) -> Result<Plan, Vec<ParseWarning>> {
        if self.warnings.is_empty() {
            Ok(self.plan)
        } else {
            Err(self.warnings)
        }
    }

    /// Apply a function to each warning (useful for logging) and return the plan.
    pub fn with_warnings<F>(self, mut f: F) -> Plan
    where
        F: FnMut(&ParseWarning),
    {
        for warning in &self.warnings {
            f(warning);
        }
        self.plan
    }

    /// Check if there are any warnings.
    pub fn has_warnings(&self) -> bool {
        !self.warnings.is_empty()
    }

    /// Get the number of warnings.
    pub fn warning_count(&self) -> usize {
        self.warnings.len()
    }
}
