//! Extension Registry for Custom Substrait Extension Relations
//!
//! This module provides a registry system for custom Substrait extension relations,
//! allowing users to register their own extension types with custom parsing and
//! textification logic.
//!
//! # Overview
//!
//! The extension registry allows users to:
//! - Register custom extension handlers for specific extension names
//! - Parse extension arguments/named arguments into `google.protobuf.Any` detail fields
//! - Textify extension detail fields back into readable text format
//! - Support both compile-time and runtime extension registration
//!
//! # Architecture
//!
//! The system is built around several key traits:
//! - `ExtensionHandler`: Core trait for handling extension parsing and textification
//! - `ExtensionRegistry`: Registry for looking up extension handlers
//! - `ExtensionDetail`: Representation of extension-specific data
//!
//! # Example Usage
//!
//! ```rust
//! use substrait_explain::extensions::registry::{ExtensionRegistry, ExtensionHandler};
//! use substrait_explain::extensions::registry::{ExtensionArgs, ExtensionDetail};
//!
//! // Define a custom extension handler
//! struct ParquetScanHandler;
//!
//! impl ExtensionHandler for ParquetScanHandler {
//!     fn extension_name(&self) -> &str {
//!         "ParquetScan"
//!     }
//!
//!     fn parse_detail(&self, args: ExtensionArgs) -> Result<ExtensionDetail, ExtensionError> {
//!         // Parse arguments into custom detail structure
//!         let path = args.get_named_arg("path")?;
//!         let schema = args.get_named_arg("schema")?;
//!
//!         // Create detail with custom protobuf message
//!         ExtensionDetail::from_message("my.extension.ParquetScan", &custom_message)
//!     }
//!
//!     fn textify_detail(&self, detail: &ExtensionDetail) -> Result<ExtensionArgs, ExtensionError> {
//!         // Convert detail back to arguments for text output
//!         let custom_message: CustomMessage = detail.as_message()?;
//!
//!         ExtensionArgs::builder()
//!             .named_arg("path", custom_message.path)
//!             .named_arg("schema", custom_message.schema)
//!             .build()
//!     }
//! }
//!
//! // Register the handler
//! let mut registry = ExtensionRegistry::new();
//! registry.register("ParquetScan", Box::new(ParquetScanHandler));
//! ```

use std::collections::HashMap;
use std::fmt;

use thiserror::Error;

/// Error types for extension registry operations
#[derive(Debug, Error)]
pub enum ExtensionError {
    #[error("Extension '{0}' not found in registry")]
    ExtensionNotFound(String),

    #[error("Failed to parse extension detail: {0}")]
    ParseError(String),

    #[error("Failed to serialize extension detail: {0}")]
    SerializationError(String),

    #[error("Failed to encode extension detail: {0}")]
    EncodeError(String),

    #[error("Invalid argument: {0}")]
    InvalidArgument(String),

    #[error("Missing required argument: {0}")]
    MissingArgument(String),

    #[error("Type mismatch: expected {expected}, got {actual}")]
    TypeMismatch { expected: String, actual: String },
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
    /// Function call expression
    Expression(String), // For now, store as string - could be more structured later
}

/// Represents an output column specification
#[derive(Debug, Clone)]
pub enum ExtensionColumn {
    /// Named column with type (name:type)
    Named { name: String, type_spec: String },
    /// Field reference ($0, $1, etc.)
    Reference(i32),
    /// Expression (function call, literal, etc.)
    Expression(String),
}

impl ExtensionArgs {
    /// Create a new empty ExtensionArgs
    pub fn new() -> Self {
        Self {
            positional: Vec::new(),
            named: HashMap::new(),
            output_columns: Vec::new(),
        }
    }

    /// Get a named argument value
    pub fn get_named_arg(&self, name: &str) -> Result<&ExtensionValue, ExtensionError> {
        self.named
            .get(name)
            .ok_or_else(|| ExtensionError::MissingArgument(name.to_string()))
    }

    /// Get a named argument as a string
    pub fn get_string_arg(&self, name: &str) -> Result<&str, ExtensionError> {
        match self.get_named_arg(name)? {
            ExtensionValue::String(s) => Ok(s),
            other => Err(ExtensionError::TypeMismatch {
                expected: "string".to_string(),
                actual: format!("{other:?}"),
            }),
        }
    }

    /// Get a named argument as an integer
    pub fn get_integer_arg(&self, name: &str) -> Result<i64, ExtensionError> {
        match self.get_named_arg(name)? {
            ExtensionValue::Integer(i) => Ok(*i),
            other => Err(ExtensionError::TypeMismatch {
                expected: "integer".to_string(),
                actual: format!("{other:?}"),
            }),
        }
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
}

impl Default for ExtensionArgs {
    fn default() -> Self {
        Self::new()
    }
}

/// Core trait for handling extension parsing and textification
pub trait ExtensionHandler: Send + Sync {
    /// The name of the extension (e.g., "ParquetScan", "VectorNormalize")
    fn extension_name(&self) -> &str;

    /// Parse extension arguments into a protobuf Any message
    ///
    /// This method converts the parsed arguments, named arguments, and output columns
    /// from the text format into a protobuf `Any` message for the detail field.
    fn parse_detail(
        &self,
        args: ExtensionArgs,
    ) -> Result<crate::extensions::any::Any, ExtensionError>;

    /// Convert protobuf Any message back to arguments for textification
    ///
    /// This method converts the protobuf detail field back into arguments that can
    /// be formatted as text output.
    fn textify_detail(
        &self,
        detail: &crate::extensions::any::Any,
    ) -> Result<ExtensionArgs, ExtensionError>;
}

/// Enhanced extension handler trait for working with typed protobuf messages
pub trait TypedExtensionHandler<T>: ExtensionHandler
where
    T: prost::Message + prost::Name + Default,
{
    /// Parse extension arguments into a typed protobuf message
    fn parse_typed_message(&self, args: ExtensionArgs) -> Result<T, ExtensionError>;

    /// Convert a typed protobuf message back to arguments for textification
    fn textify_typed_message(&self, message: &T) -> Result<ExtensionArgs, ExtensionError>;

    /// Default implementation of parse_detail using typed message parsing
    fn parse_detail_typed(
        &self,
        args: ExtensionArgs,
    ) -> Result<crate::extensions::any::Any, ExtensionError> {
        let message = self.parse_typed_message(args)?;
        crate::extensions::any::Any::encode(&message)
    }

    /// Default implementation of textify_detail using typed message parsing
    fn textify_detail_typed(
        &self,
        detail: &crate::extensions::any::Any,
    ) -> Result<ExtensionArgs, ExtensionError> {
        let message: T = detail.decode()?;
        self.textify_typed_message(&message)
    }
}

/// Registry for extension handlers
pub struct ExtensionRegistry {
    handlers: HashMap<String, Box<dyn ExtensionHandler>>,
}

impl Clone for ExtensionRegistry {
    fn clone(&self) -> Self {
        // For now, create a new empty registry when cloning
        // In the future, we may want to implement proper cloning of handlers
        Self::new()
    }
}

impl ExtensionRegistry {
    /// Create a new empty extension registry
    pub fn new() -> Self {
        Self {
            handlers: HashMap::new(),
        }
    }

    /// Register an extension handler
    pub fn register(&mut self, name: impl Into<String>, handler: Box<dyn ExtensionHandler>) {
        let name = name.into();
        self.handlers.insert(name, handler);
    }

    /// Get an extension handler by name
    pub fn get_handler(&self, name: &str) -> Result<&dyn ExtensionHandler, ExtensionError> {
        self.handlers
            .get(name)
            .map(|h| h.as_ref())
            .ok_or_else(|| ExtensionError::ExtensionNotFound(name.to_string()))
    }

    /// Parse extension arguments into a protobuf Any message
    pub fn parse_extension(
        &self,
        extension_name: &str,
        args: ExtensionArgs,
    ) -> Result<crate::extensions::any::Any, ExtensionError> {
        let handler = self.get_handler(extension_name)?;
        handler.parse_detail(args)
    }

    /// Convert protobuf Any message back to arguments for textification
    pub fn textify_extension(
        &self,
        extension_name: &str,
        detail: &crate::extensions::any::Any,
    ) -> Result<ExtensionArgs, ExtensionError> {
        let handler = self.get_handler(extension_name)?;
        handler.textify_detail(detail)
    }

    /// Get all registered extension names
    pub fn extension_names(&self) -> Vec<&str> {
        self.handlers.keys().map(|s| s.as_str()).collect()
    }

    /// Check if an extension is registered
    pub fn has_extension(&self, name: &str) -> bool {
        self.handlers.contains_key(name)
    }
}

impl Default for ExtensionRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Debug for ExtensionRegistry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ExtensionRegistry")
            .field("handlers", &self.handlers.keys().collect::<Vec<_>>())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Mock extension handler for testing
    struct TestExtensionHandler;

    impl ExtensionHandler for TestExtensionHandler {
        fn extension_name(&self) -> &str {
            "TestExtension"
        }

        fn parse_detail(
            &self,
            args: ExtensionArgs,
        ) -> Result<crate::extensions::any::Any, ExtensionError> {
            // Simple test implementation - create Any with raw bytes
            let json_str = format!("{args:?}");
            Ok(crate::extensions::any::Any::new(
                "test.TestExtension".to_string(),
                json_str.into_bytes(),
            ))
        }

        fn textify_detail(
            &self,
            _detail: &crate::extensions::any::Any,
        ) -> Result<ExtensionArgs, ExtensionError> {
            // Simple test implementation - return empty args
            Ok(ExtensionArgs::new())
        }
    }

    #[test]
    fn test_extension_registry_basic() {
        let mut registry = ExtensionRegistry::new();

        // Initially empty
        assert_eq!(registry.extension_names().len(), 0);
        assert!(!registry.has_extension("TestExtension"));

        // Register handler
        registry.register("TestExtension", Box::new(TestExtensionHandler));

        // Now has extension
        assert_eq!(registry.extension_names().len(), 1);
        assert!(registry.has_extension("TestExtension"));

        // Can get handler
        let handler = registry.get_handler("TestExtension").unwrap();
        assert_eq!(handler.extension_name(), "TestExtension");
    }

    #[test]
    fn test_extension_args() {
        let mut args = ExtensionArgs::new();

        // Add named args
        args.add_named_arg(
            "path".to_string(),
            ExtensionValue::String("data/*.parquet".to_string()),
        );
        args.add_named_arg("batch_size".to_string(), ExtensionValue::Integer(1024));

        // Add positional args
        args.add_positional_arg(ExtensionValue::Reference(0));

        // Add output columns
        args.add_output_column(ExtensionColumn::Named {
            name: "col1".to_string(),
            type_spec: "i32".to_string(),
        });

        // Test retrieval
        assert_eq!(args.get_string_arg("path").unwrap(), "data/*.parquet");
        assert_eq!(args.get_integer_arg("batch_size").unwrap(), 1024);
        assert_eq!(args.positional.len(), 1);
        assert_eq!(args.output_columns.len(), 1);
    }

    #[test]
    fn test_extension_error_cases() {
        let registry = ExtensionRegistry::new();

        // Extension not found
        let result = registry.get_handler("NonExistent");
        assert!(matches!(result, Err(ExtensionError::ExtensionNotFound(_))));

        // Missing argument
        let args = ExtensionArgs::new();
        let result = args.get_named_arg("missing");
        assert!(matches!(result, Err(ExtensionError::MissingArgument(_))));

        // Type mismatch
        let mut args = ExtensionArgs::new();
        args.add_named_arg("test".to_string(), ExtensionValue::Integer(42));
        let result = args.get_string_arg("test");
        assert!(matches!(result, Err(ExtensionError::TypeMismatch { .. })));
    }
}
