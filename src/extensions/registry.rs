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
//! - `AnyConvertible`: For converting types to/from protobuf Any messages
//! - `Explainable`: For converting types to/from ExtensionArgs
//! - `ExtensionRegistry`: Registry for managing extension types
//!
//! # Example Usage
//!
//! ```rust
//! use substrait_explain::extensions::{ExtensionRegistry, AnyConvertible, Explainable};
//! use substrait_explain::extensions::{ExtensionArgs, ExtensionError, ExtensionValue, Any, AnyRef};
//!
//! // Define a custom extension type
//! struct ParquetScanConfig {
//!     path: String,
//! }
//!
//! // Implement AnyConvertible for protobuf serialization
//! impl AnyConvertible for ParquetScanConfig {
//!     fn to_any(&self) -> Result<Any, ExtensionError> {
//!         // For this example, we'll create a simple Any with the path
//!         Ok(Any::new(
//!             Self::type_url(),
//!             self.path.as_bytes().to_vec()
//!         ))
//!     }
//!
//!     fn from_any<'a>(any: AnyRef<'a>) -> Result<Self, ExtensionError> {
//!         // Deserialize from Any
//!         let path = String::from_utf8(any.value.to_vec())
//!             .map_err(|e| ExtensionError::ParseError(format!("Invalid UTF-8: {}", e)))?;
//!         Ok(ParquetScanConfig { path })
//!     }
//!
//!     fn type_url() -> String {
//!         "type.googleapis.com/example.ParquetScanConfig".to_string()
//!     }
//! }
//!
//! // Implement Explainable for text format conversion
//! impl Explainable for ParquetScanConfig {
//!     fn name() -> &'static str {
//!         "ParquetScan"
//!     }
//!
//!     fn from_args(args: &ExtensionArgs) -> Result<Self, ExtensionError> {
//!         let path = match args.get_named_arg("path") {
//!             Some(ExtensionValue::String(s)) => s.clone(),
//!             Some(_) => return Err(ExtensionError::InvalidArgument("path must be a string".to_string())),
//!             None => return Err(ExtensionError::MissingArgument("path".to_string())),
//!         };
//!         Ok(ParquetScanConfig { path })
//!     }
//!
//!     fn to_args(&self) -> Result<ExtensionArgs, ExtensionError> {
//!         use substrait_explain::extensions::ExtensionRelationType;
//!         let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf, "ParquetScanConfig".to_string());
//!         args.add_named_arg(
//!             "path".to_string(),
//!             substrait_explain::extensions::ExtensionValue::String(self.path.clone())
//!         );
//!         Ok(args)
//!     }
//! }
//!
//! // Register the extension type
//! let mut registry = ExtensionRegistry::new();
//! registry.register::<ParquetScanConfig>();
//! ```

use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;

use thiserror::Error;

use crate::extensions::any::{Any, AnyRef};
use crate::extensions::args::ExtensionArgs;

/// Error types for extension registry operations
#[derive(Debug, Error, Clone)]
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

/// Trait for types that can be converted to/from protobuf Any messages. Note
/// that this is already implemented for all prost::Message types. For custom
/// types, implement this trait.
pub trait AnyConvertible: Sized {
    /// Convert this type to a protobuf Any message
    fn to_any(&self) -> Result<Any, ExtensionError>;

    /// Convert from a protobuf Any message to this type
    fn from_any<'a>(any: AnyRef<'a>) -> Result<Self, ExtensionError>;

    /// Get the protobuf type URL for this type.
    /// For prost::Message types, this is provided automatically.
    /// For custom types, override this method.
    fn type_url() -> String {
        panic!("type_url() must be implemented for non-prost types")
    }
}

// Blanket implementation for all prost::Message types
impl<T> AnyConvertible for T
where
    T: prost::Message + prost::Name + Default,
{
    fn to_any(&self) -> Result<Any, ExtensionError> {
        Any::encode(self)
    }

    fn from_any<'a>(any: AnyRef<'a>) -> Result<Self, ExtensionError> {
        any.decode()
    }

    fn type_url() -> String {
        T::type_url()
    }
}

/// Trait for types that participate in text explanations.
pub trait Explainable: Sized {
    /// Canonical textual name for this extension. This is what appears in
    /// Substrait text plans and how the registry identifies the type.
    fn name() -> &'static str;

    /// Optional aliases that should also resolve to this extension when parsing
    /// text. The canonical `name()` is always used when textifying.
    fn aliases() -> &'static [&'static str] {
        &[]
    }

    /// Parse extension arguments into this type
    fn from_args(args: &ExtensionArgs) -> Result<Self, ExtensionError>;

    /// Convert this type to extension arguments
    fn to_args(&self) -> Result<ExtensionArgs, ExtensionError>;

    /// Optionally specify the preferred order for named arguments in textification.
    ///Arguments not in the list appear last, alphabetically.
    /// Arguments that exist in the list but are not present in the args
    /// will not be included in the textified output. Return an empty list to always output alphabetically.
    fn argument_order() -> Vec<String> {
        Vec::new() // Default: alphabetical order
    }
}

/// Internal trait that converts between ExtensionArgs and protobuf Any messages.
///
/// This trait exists because we need to store handlers for different extension types
/// in a single HashMap. Since Rust doesn't allow trait objects with multiple traits
/// (like `Box<dyn AnyConvertible + Explainable>`), we need a single trait that
/// combines both operations.
///
/// The ExtensionConverter acts as a bridge between:
/// - The text format representation (ExtensionArgs) used by the parser/formatter
/// - The protobuf Any messages stored in Substrait extension relations
///
/// This design allows the registry to work with any type while maintaining type safety
/// through the AnyConvertible and Explainable traits that users implement.
trait ExtensionConverter: Send + Sync {
    fn parse_detail(&self, args: &ExtensionArgs) -> Result<Any, ExtensionError>;

    fn textify_detail(&self, detail: AnyRef<'_>) -> Result<ExtensionArgs, ExtensionError>;

    /// Get the preferred argument order for this extension type
    fn argument_order(&self) -> Vec<String>;
}

/// Type adapter that implements ExtensionConverter for any type T that implements
/// both AnyConvertible and Explainable.
///
/// This struct exists to solve Rust's "trait object problem": we can't store
/// `Box<dyn AnyConvertible + Explainable>` because that's two traits, not one.
/// Instead, we store `Box<dyn ExtensionConverter>` and use this adapter to bridge
/// from the two user-facing traits to our single internal trait.
///
/// The adapter pattern allows us to:
/// 1. Keep a clean API where users only implement AnyConvertible and Explainable
/// 2. Store different types in the same HashMap through type erasure
/// 3. Maintain type safety - the concrete type T is known at registration time
/// 4. Avoid any runtime type checking or unsafe code
///
/// The PhantomData is necessary because we don't actually store a T, but we need
/// the type information to call T's static methods (from_args, from_any).
struct ExtensionAdapter<T>(std::marker::PhantomData<T>);

impl<T: AnyConvertible + Explainable + Send + Sync> ExtensionConverter for ExtensionAdapter<T> {
    fn parse_detail(&self, args: &ExtensionArgs) -> Result<Any, ExtensionError> {
        // Convert: ExtensionArgs -> T -> Any
        T::from_args(args)?.to_any()
    }

    fn textify_detail(&self, detail: AnyRef<'_>) -> Result<ExtensionArgs, ExtensionError> {
        // Convert: AnyRef -> Any -> T -> ExtensionArgs
        // Create an owned Any from the AnyRef to work with existing T::from_any
        let owned_any = Any::new(detail.type_url.to_string(), detail.value.to_vec());
        T::from_any(owned_any.as_ref())?.to_args()
    }

    fn argument_order(&self) -> Vec<String> {
        T::argument_order()
    }
}

/// Registry for extension handlers
#[derive(Default, Clone)]
pub struct ExtensionRegistry {
    handlers: HashMap<String, Arc<dyn ExtensionConverter>>,
    type_urls: HashMap<String, String>, // type_url -> extension_name
}

impl ExtensionRegistry {
    /// Create a new empty extension registry
    pub fn new() -> Self {
        Self {
            handlers: HashMap::new(),
            type_urls: HashMap::new(),
        }
    }

    /// Register an extension type that implements both AnyConvertible and Explainable
    ///
    /// The canonical textual name comes from `T::name()`. Additional aliases can
    /// be provided via `T::aliases()`.
    pub fn register<T>(&mut self)
    where
        T: AnyConvertible + Explainable + Send + Sync + 'static,
    {
        let canonical_name = T::name();
        let type_url = T::type_url();
        let handler: Arc<dyn ExtensionConverter> =
            Arc::new(ExtensionAdapter::<T>(std::marker::PhantomData));

        if self.handlers.contains_key(canonical_name) {
            panic!("Extension '{}' already registered", canonical_name);
        }

        self.handlers
            .insert(canonical_name.to_string(), Arc::clone(&handler));

        for &alias in T::aliases() {
            if alias == canonical_name {
                continue;
            }
            if self.handlers.contains_key(alias) {
                panic!(
                    "Extension alias '{}' collides with an existing handler",
                    alias
                );
            }
            self.handlers
                .insert(alias.to_string(), Arc::clone(&handler));
        }

        match self
            .type_urls
            .insert(type_url.clone(), canonical_name.to_string())
        {
            Some(existing) if existing != canonical_name => {
                panic!(
                    "Type URL '{}' already registered to '{}'",
                    type_url, existing
                );
            }
            _ => {}
        }
    }

    /// Parse extension arguments into a protobuf Any message
    pub fn parse_extension(
        &self,
        extension_name: &str,
        args: &ExtensionArgs,
    ) -> Result<Any, ExtensionError> {
        let handler = self
            .handlers
            .get(extension_name)
            .ok_or_else(|| ExtensionError::ExtensionNotFound(extension_name.to_string()))?;
        handler.parse_detail(args)
    }

    /// Decode extension detail to extension name and ExtensionArgs
    /// This is the primary method for textification - given an AnyRef with extension detail,
    /// decode it to the extension name and appropriate ExtensionArgs for display
    pub fn decode(&self, detail: AnyRef<'_>) -> Result<(String, ExtensionArgs), ExtensionError> {
        // Find extension name by type URL
        let extension_name = self
            .type_urls
            .get(detail.type_url)
            .ok_or_else(|| ExtensionError::ExtensionNotFound(detail.type_url.to_string()))?;

        // Get handler and textify the detail
        let handler = self
            .handlers
            .get(extension_name)
            .ok_or_else(|| ExtensionError::ExtensionNotFound(extension_name.clone()))?;

        let mut args = handler.textify_detail(detail)?;

        // Populate argument order from the extension handler
        args.argument_order = handler.argument_order();

        Ok((extension_name.clone(), args))
    }

    /// Get all registered extension names
    pub fn extension_names(&self) -> Vec<&str> {
        let mut names: Vec<&str> = self.type_urls.values().map(|s| s.as_str()).collect();
        names.sort_unstable();
        names.dedup();
        names
    }

    /// Check if an extension is registered
    pub fn has_extension(&self, name: &str) -> bool {
        self.handlers.contains_key(name)
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
    use crate::extensions::{ExtensionColumn, ExtensionRelationType, ExtensionValue};

    // Mock type for testing
    struct TestExtension {
        path: String,
        batch_size: i64,
    }

    // Manual implementation of AnyConvertible for testing (without prost)
    impl AnyConvertible for TestExtension {
        fn to_any(&self) -> Result<Any, ExtensionError> {
            // Simple test implementation - create Any with JSON-like bytes
            let json_str = format!(
                r#"{{"path":"{}","batch_size":{}}}"#,
                self.path, self.batch_size
            );
            Ok(Any::new(Self::type_url(), json_str.into_bytes()))
        }

        fn type_url() -> String {
            "test.TestExtension".to_string()
        }

        fn from_any<'a>(any: AnyRef<'a>) -> Result<Self, ExtensionError> {
            // Simple test implementation - parse from JSON-like bytes
            let json_str = String::from_utf8(any.value.to_vec())
                .map_err(|e| ExtensionError::ParseError(format!("Invalid UTF-8: {e}")))?;

            // Simple manual parsing for test
            if json_str.contains("path") && json_str.contains("batch_size") {
                Ok(TestExtension {
                    path: "test.parquet".to_string(),
                    batch_size: 1024,
                })
            } else {
                Err(ExtensionError::ParseError("Missing fields".to_string()))
            }
        }
    }

    impl Explainable for TestExtension {
        fn name() -> &'static str {
            "TestExtension"
        }

        fn from_args(args: &ExtensionArgs) -> Result<Self, ExtensionError> {
            let path = match args.get_named_arg("path") {
                Some(ExtensionValue::String(s)) => s.clone(),
                Some(_) => {
                    return Err(ExtensionError::InvalidArgument(
                        "path must be a string".to_string(),
                    ));
                }
                None => return Err(ExtensionError::MissingArgument("path".to_string())),
            };

            let batch_size = match args.get_named_arg("batch_size") {
                Some(ExtensionValue::Integer(i)) => *i,
                Some(_) => {
                    return Err(ExtensionError::InvalidArgument(
                        "batch_size must be an integer".to_string(),
                    ));
                }
                None => return Err(ExtensionError::MissingArgument("batch_size".to_string())),
            };

            Ok(TestExtension { path, batch_size })
        }

        fn to_args(&self) -> Result<ExtensionArgs, ExtensionError> {
            let mut args =
                ExtensionArgs::new(ExtensionRelationType::Leaf, "TestExtension".to_string());
            args.add_named_arg(
                "path".to_string(),
                ExtensionValue::String(self.path.clone()),
            );
            args.add_named_arg(
                "batch_size".to_string(),
                ExtensionValue::Integer(self.batch_size),
            );
            Ok(args)
        }
    }

    #[test]
    fn test_extension_registry_basic() {
        let mut registry = ExtensionRegistry::new();

        // Initially empty
        assert_eq!(registry.extension_names().len(), 0);
        assert!(!registry.has_extension("TestExtension"));

        // Register extension type
        registry.register::<TestExtension>();

        // Now has extension
        assert_eq!(registry.extension_names().len(), 1);
        assert!(registry.has_extension("TestExtension"));

        // Test parse and textify
        let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf, "TestExtension".to_string());
        args.add_named_arg(
            "path".to_string(),
            ExtensionValue::String("data.parquet".to_string()),
        );
        args.add_named_arg("batch_size".to_string(), ExtensionValue::Integer(2048));

        let any = registry.parse_extension("TestExtension", &args).unwrap();
        assert_eq!(any.type_url, "test.TestExtension");

        let any_ref = any.as_ref();
        let result = registry.decode(any_ref).unwrap();
        assert_eq!(result.0, "TestExtension");
        match result.1.get_named_arg("path") {
            Some(ExtensionValue::String(s)) => assert_eq!(s, "test.parquet"), // Due to our simple test impl
            _ => panic!("Expected String for path"),
        }
    }

    #[test]
    fn test_extension_args() {
        let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf, "TestExtension".to_string());

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
        match args.get_named_arg("path") {
            Some(ExtensionValue::String(s)) => assert_eq!(s, "data/*.parquet"),
            _ => panic!("Expected String for path"),
        }

        match args.get_named_arg("batch_size") {
            Some(ExtensionValue::Integer(i)) => assert_eq!(*i, 1024),
            _ => panic!("Expected Integer for batch_size"),
        }
        assert_eq!(args.positional.len(), 1);
        assert_eq!(args.output_columns.len(), 1);
    }

    #[test]
    fn test_extension_error_cases() {
        let registry = ExtensionRegistry::new();

        // Extension not found
        let args = ExtensionArgs::new(ExtensionRelationType::Leaf, "NonExistent".to_string());
        let result = registry.parse_extension("NonExistent", &args);
        assert!(matches!(result, Err(ExtensionError::ExtensionNotFound(_))));

        // Missing argument
        let args = ExtensionArgs::new(ExtensionRelationType::Leaf, "TestExtension".to_string());
        let result = args.get_named_arg("missing");
        assert!(result.is_none());

        // Type check example
        let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf, "TestExtension".to_string());
        args.add_named_arg("test".to_string(), ExtensionValue::Integer(42));
        let result = args.get_named_arg("test");
        match result {
            Some(ExtensionValue::Integer(42)) => {} // Expected
            _ => panic!("Expected Integer(42), got {result:?}"),
        }
    }
}
