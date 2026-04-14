//! Extension Registry for Custom Substrait Extension Relations
//!
//! This module provides a registry system for custom Substrait extension
//! relations, allowing users to register their own extension types with custom
//! parsing and textification logic.
//!
//! # Overview
//!
//! The extension registry allows users to:
//! - Register custom extension handlers for specific extension names
//! - Parse extension arguments/named arguments into `google.protobuf.Any`
//!   detail fields
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
//! use substrait_explain::extensions::{
//!     Any, AnyConvertible, AnyRef, ExplainContext, Explainable, ExtensionArgs, ExtensionError,
//!     ExtensionRegistry, ExtensionRelationType, ExtensionValue,
//! };
//!
//! // Define a custom extension type
//! struct CustomScanConfig {
//!     path: String,
//! }
//!
//! // Implement AnyConvertible for protobuf serialization
//! impl AnyConvertible for CustomScanConfig {
//!     fn to_any(&self) -> Result<Any, ExtensionError> {
//!         // For this example, we'll create a simple Any (protobuf details field) with the path
//!         Ok(Any::new(Self::type_url(), self.path.as_bytes().to_vec()))
//!     }
//!
//!     fn from_any<'a>(any: AnyRef<'a>) -> Result<Self, ExtensionError> {
//!         // Deserialize from Any
//!         let path = String::from_utf8(any.value.to_vec())
//!             .map_err(|e| ExtensionError::Custom(format!("Invalid UTF-8: {}", e)))?;
//!         Ok(CustomScanConfig { path })
//!     }
//!
//!     fn type_url() -> String {
//!         "type.googleapis.com/example.CustomScanConfig".to_string()
//!     }
//! }
//!
//! // Implement Explainable for text format conversion
//! impl Explainable for CustomScanConfig {
//!     fn name() -> &'static str {
//!         "ParquetScan"
//!     }
//!
//!     fn from_args(args: &ExtensionArgs, _ctx: &ExplainContext) -> Result<Self, ExtensionError> {
//!         let mut extractor = args.extractor();
//!         let path: &str = extractor.expect_named_arg("path")?;
//!         extractor.check_exhausted()?;
//!         Ok(CustomScanConfig {
//!             path: path.to_string(),
//!         })
//!     }
//!
//!     fn to_args(&self, _ctx: &ExplainContext) -> Result<ExtensionArgs, ExtensionError> {
//!         let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf);
//!         args.named.insert(
//!             "path".to_string(),
//!             ExtensionValue::String(self.path.clone()),
//!         );
//!         Ok(args)
//!     }
//! }
//!
//! // Register the extension type
//! let mut registry = ExtensionRegistry::new();
//! registry.register_relation::<CustomScanConfig>().unwrap();
//! ```

use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;

use substrait::proto;
use substrait::proto::NamedStruct;
use thiserror::Error;

use crate::extensions::any::{Any, AnyRef};
use crate::extensions::args::{ExtensionArgs, ExtensionColumn};
use crate::extensions::simple::SimpleExtensions;

/// Type of extension in the registry, used for namespace separation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExtensionType {
    /// Relation extension (e.g., ExtensionLeaf, ExtensionSingle, ExtensionMulti)
    Relation,
    /// Enhancement attached to a relation (uses `+ Enh:` prefix in text format)
    Enhancement,
    /// Optimization attached to a relation (uses `+ Opt:` prefix in text format)
    Optimization,
}

/// Errors during extension registration (setup phase)
#[derive(Debug, Error, Clone)]
pub enum RegistrationError {
    #[error("{ext_type:?} extension '{name}' already registered")]
    DuplicateName {
        ext_type: ExtensionType,
        name: String,
    },

    #[error("Type URL '{type_url}' already registered to {ext_type:?} extension '{existing_name}'")]
    ConflictingTypeUrl {
        type_url: String,
        ext_type: ExtensionType,
        existing_name: String,
    },
}

/// Errors during extension parsing, formatting, and argument extraction (runtime)
#[derive(Debug, Error, Clone)]
pub enum ExtensionError {
    /// Extension not found in registry during lookup
    #[error("Extension '{name}' not found in registry")]
    NotFound { name: String },

    /// Required argument not present (from ArgsExtractor)
    #[error("Missing required argument: {name}")]
    MissingArgument { name: String },

    /// Invalid argument value (from Explainable impls and ArgsExtractor)
    #[error("Invalid argument: {0}")]
    InvalidArgument(String),

    /// Type URL mismatch during protobuf Any decode
    #[error("Type URL mismatch: expected {expected}, got {actual}")]
    TypeUrlMismatch { expected: String, actual: String },

    /// Protobuf message decode failure
    #[error("Failed to decode protobuf message")]
    DecodeFailed(#[source] prost::DecodeError),

    /// Protobuf message encode failure
    #[error("Failed to encode protobuf message")]
    EncodeFailed(#[source] prost::EncodeError),

    /// Extension detail field is missing from the relation
    #[error("Extension detail is missing")]
    MissingDetail,

    /// Error from a custom AnyConvertible implementation
    #[error("{0}")]
    Custom(String),
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
    /// For prost::Message types, this is provided automatically via blanket impl.
    /// Custom types must implement this method.
    fn type_url() -> String;
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

/// Context provided to [`Explainable::from_args`] and [`Explainable::to_args`],
/// giving extension implementations access to plan-level metadata and
/// convenience helpers for schema conversion.
pub struct ExplainContext<'a> {
    extensions: &'a SimpleExtensions,
}

impl<'a> ExplainContext<'a> {
    /// Create a new context from the plan's simple extensions.
    pub fn new(extensions: &'a SimpleExtensions) -> Self {
        Self { extensions }
    }

    /// Access the plan's simple extensions (URN anchors, function/type mappings).
    pub fn extensions(&self) -> &SimpleExtensions {
        self.extensions
    }

    /// Convert output columns to a [`NamedStruct`].
    ///
    /// Each [`ExtensionColumn::Named`] contributes a name and type;
    /// other column variants produce an error.
    pub fn schema(&self, columns: &[ExtensionColumn]) -> Result<NamedStruct, ExtensionError> {
        let mut names = Vec::with_capacity(columns.len());
        let mut types = Vec::with_capacity(columns.len());
        for col in columns {
            match col {
                ExtensionColumn::Named { name, r#type: ty } => {
                    names.push(name.clone());
                    types.push(ty.clone());
                }
                other => {
                    return Err(ExtensionError::InvalidArgument(format!(
                        "Expected named column, got {other:?}"
                    )));
                }
            }
        }
        Ok(NamedStruct {
            names,
            r#struct: Some(proto::r#type::Struct {
                types,
                type_variation_reference: 0,
                nullability: proto::r#type::Nullability::Required as i32,
            }),
        })
    }

    /// Convert a [`NamedStruct`] to output columns.
    ///
    /// Returns an error if the struct's names and types have different lengths.
    pub fn columns(&self, schema: &NamedStruct) -> Result<Vec<ExtensionColumn>, ExtensionError> {
        let types = schema
            .r#struct
            .as_ref()
            .map(|s| s.types.as_slice())
            .unwrap_or_default();
        if schema.names.len() != types.len() {
            return Err(ExtensionError::InvalidArgument(format!(
                "NamedStruct has {} names but {} types",
                schema.names.len(),
                types.len()
            )));
        }
        Ok(schema
            .names
            .iter()
            .zip(types.iter())
            .map(|(name, ty)| ExtensionColumn::Named {
                name: name.clone(),
                r#type: ty.clone(),
            })
            .collect())
    }
}

/// Trait for types that participate in text explanations.
pub trait Explainable: Sized {
    /// Canonical textual name for this extension. This is what appears in
    /// Substrait text plans and how the registry identifies the type.
    fn name() -> &'static str;

    /// Parse extension arguments into this type.
    fn from_args(args: &ExtensionArgs, ctx: &ExplainContext) -> Result<Self, ExtensionError>;

    /// Convert this type to extension arguments.
    fn to_args(&self, ctx: &ExplainContext) -> Result<ExtensionArgs, ExtensionError>;
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
    fn parse_detail(
        &self,
        args: &ExtensionArgs,
        ctx: &ExplainContext,
    ) -> Result<Any, ExtensionError>;

    fn textify_detail(
        &self,
        detail: AnyRef<'_>,
        ctx: &ExplainContext,
    ) -> Result<ExtensionArgs, ExtensionError>;
}

/// Type adapter that implements ExtensionConverter for any type T that implements
/// both AnyConvertible and Explainable.
struct ExtensionAdapter<T>(std::marker::PhantomData<T>);

impl<T: AnyConvertible + Explainable + Send + Sync> ExtensionConverter for ExtensionAdapter<T> {
    fn parse_detail(
        &self,
        args: &ExtensionArgs,
        ctx: &ExplainContext,
    ) -> Result<Any, ExtensionError> {
        T::from_args(args, ctx)?.to_any()
    }

    fn textify_detail(
        &self,
        detail: AnyRef<'_>,
        ctx: &ExplainContext,
    ) -> Result<ExtensionArgs, ExtensionError> {
        let owned_any = Any::new(detail.type_url.to_string(), detail.value.to_vec());
        T::from_any(owned_any.as_ref())?.to_args(ctx)
    }
}

pub trait Extension: AnyConvertible + Explainable + Send + Sync + 'static {}

impl<T> Extension for T where T: AnyConvertible + Explainable + Send + Sync + 'static {}

/// Registry for extension handlers
#[derive(Default, Clone)]
pub struct ExtensionRegistry {
    // Composite key: (ExtensionType, name) -> handler
    handlers: HashMap<(ExtensionType, String), Arc<dyn ExtensionConverter>>,
    // Composite key: (ExtensionType, type_url) -> name
    type_urls: HashMap<(ExtensionType, String), String>,
    // Compiled proto FileDescriptorSet blobs for extension types.
    // Used by the JSON parser to resolve google.protobuf.Any type URLs in Go
    // protojson input. Register these alongside the Rust handler so that a
    // single registry carries all extension knowledge for both formatting and
    // JSON parsing.
    descriptors: Vec<Vec<u8>>,
}

impl ExtensionRegistry {
    /// Create a new empty extension registry
    pub fn new() -> Self {
        Self {
            handlers: HashMap::new(),
            type_urls: HashMap::new(),
            descriptors: Vec::new(),
        }
    }

    /// Register a compiled proto `FileDescriptorSet` blob for extension types.
    ///
    /// Required when parsing extensions for plans that contain
    /// `google.protobuf.Any` fields that use standard JSON encoding (with
    /// `@type` for the type_url) whose types are not part of the Substrait core
    /// schema. Pass the bytes of a compiled `.bin` descriptor, e.g.
    /// `include_bytes!("my_extensions.bin")`.
    pub fn add_descriptor(&mut self, bytes: Vec<u8>) {
        self.descriptors.push(bytes);
    }

    /// Returns slices of all registered descriptor blobs.
    pub fn descriptors(&self) -> Vec<&[u8]> {
        self.descriptors.iter().map(|b| b.as_slice()).collect()
    }

    /// Register an extension type with a specific ExtensionType
    fn register<T>(&mut self, ext_type: ExtensionType) -> Result<(), RegistrationError>
    where
        T: Extension,
    {
        let canonical_name = T::name();
        let type_url = T::type_url();
        let handler: Arc<dyn ExtensionConverter> =
            Arc::new(ExtensionAdapter::<T>(std::marker::PhantomData));

        let key = (ext_type, canonical_name.to_string());
        if self.handlers.contains_key(&key) {
            return Err(RegistrationError::DuplicateName {
                ext_type,
                name: canonical_name.to_string(),
            });
        }

        // Check for type URL conflicts before mutating any state
        let type_url_key = (ext_type, type_url.clone());
        if let Some(existing) = self.type_urls.get(&type_url_key)
            && existing != canonical_name
        {
            return Err(RegistrationError::ConflictingTypeUrl {
                type_url,
                ext_type,
                existing_name: existing.clone(),
            });
        }

        // All checks passed — safe to mutate
        self.handlers.insert(key, Arc::clone(&handler));
        self.type_urls
            .insert(type_url_key, canonical_name.to_string());
        Ok(())
    }

    /// Register a relation extension type that implements both AnyConvertible and Explainable
    ///
    /// The canonical textual name comes from `T::name()`.
    pub fn register_relation<T>(&mut self) -> Result<(), RegistrationError>
    where
        T: Extension,
    {
        self.register::<T>(ExtensionType::Relation)
    }

    /// Register an enhancement type that implements both AnyConvertible and Explainable
    ///
    /// Enhancements are registered in a separate namespace from relation extensions,
    /// allowing the same type URL to exist in both namespaces without conflict.
    ///
    /// The canonical textual name comes from `T::name()`.
    pub fn register_enhancement<T>(&mut self) -> Result<(), RegistrationError>
    where
        T: Extension,
    {
        self.register::<T>(ExtensionType::Enhancement)
    }

    /// Register an optimization type that implements both AnyConvertible and Explainable
    ///
    /// Optimizations are registered in a separate namespace from relation extensions,
    /// allowing the same type URL to exist in both namespaces without conflict.
    ///
    /// The canonical textual name comes from `T::name()`.
    pub fn register_optimization<T>(&mut self) -> Result<(), RegistrationError>
    where
        T: Extension,
    {
        self.register::<T>(ExtensionType::Optimization)
    }

    /// Parse extension arguments into a protobuf Any message
    pub fn parse_extension(
        &self,
        extension_name: &str,
        args: &ExtensionArgs,
        extensions: &SimpleExtensions,
    ) -> Result<Any, ExtensionError> {
        self.parse_with_type(ExtensionType::Relation, extension_name, args, extensions)
    }

    /// Parse enhancement arguments into a protobuf Any message
    ///
    /// Looks up the enhancement handler in the enhancement namespace and parses
    /// the arguments into a protobuf Any message.
    pub fn parse_enhancement(
        &self,
        enhancement_name: &str,
        args: &ExtensionArgs,
        extensions: &SimpleExtensions,
    ) -> Result<Any, ExtensionError> {
        self.parse_with_type(
            ExtensionType::Enhancement,
            enhancement_name,
            args,
            extensions,
        )
    }

    /// Parse optimization arguments into a protobuf Any message
    ///
    /// Looks up the optimization handler in the optimization namespace and parses
    /// the arguments into a protobuf Any message.
    pub fn parse_optimization(
        &self,
        optimization_name: &str,
        args: &ExtensionArgs,
        extensions: &SimpleExtensions,
    ) -> Result<Any, ExtensionError> {
        self.parse_with_type(
            ExtensionType::Optimization,
            optimization_name,
            args,
            extensions,
        )
    }

    /// Internal method to parse extension arguments with a specific ExtensionType
    fn parse_with_type(
        &self,
        ext_type: ExtensionType,
        name: &str,
        args: &ExtensionArgs,
        extensions: &SimpleExtensions,
    ) -> Result<Any, ExtensionError> {
        let ctx = ExplainContext::new(extensions);
        let key = (ext_type, name.to_string());
        let handler = self
            .handlers
            .get(&key)
            .ok_or_else(|| ExtensionError::NotFound {
                name: name.to_string(),
            })?;
        handler.parse_detail(args, &ctx)
    }

    /// Decode extension detail to extension name and ExtensionArgs
    pub fn decode(
        &self,
        detail: AnyRef<'_>,
        extensions: &SimpleExtensions,
    ) -> Result<(String, ExtensionArgs), ExtensionError> {
        self.decode_with_type(ExtensionType::Relation, detail, extensions)
    }

    /// Decode enhancement detail to enhancement name and ExtensionArgs
    pub fn decode_enhancement(
        &self,
        detail: AnyRef<'_>,
        extensions: &SimpleExtensions,
    ) -> Result<(String, ExtensionArgs), ExtensionError> {
        self.decode_with_type(ExtensionType::Enhancement, detail, extensions)
    }

    /// Decode optimization detail to optimization name and ExtensionArgs
    pub fn decode_optimization(
        &self,
        detail: AnyRef<'_>,
        extensions: &SimpleExtensions,
    ) -> Result<(String, ExtensionArgs), ExtensionError> {
        self.decode_with_type(ExtensionType::Optimization, detail, extensions)
    }

    /// Internal method to decode extension detail with a specific ExtensionType
    fn decode_with_type(
        &self,
        ext_type: ExtensionType,
        detail: AnyRef<'_>,
        extensions: &SimpleExtensions,
    ) -> Result<(String, ExtensionArgs), ExtensionError> {
        let ctx = ExplainContext::new(extensions);
        let type_url_key = (ext_type, detail.type_url.to_string());
        let extension_name =
            self.type_urls
                .get(&type_url_key)
                .ok_or_else(|| ExtensionError::NotFound {
                    name: detail.type_url.to_string(),
                })?;

        let name_key = (ext_type, extension_name.clone());
        let handler = self
            .handlers
            .get(&name_key)
            .ok_or_else(|| ExtensionError::NotFound {
                name: extension_name.clone(),
            })?;

        let args = handler.textify_detail(detail, &ctx)?;

        Ok((extension_name.clone(), args))
    }

    /// Get all registered extension names for a specific ExtensionType
    pub fn extension_names(&self, ext_type: ExtensionType) -> Vec<&str> {
        let mut names: Vec<&str> = self
            .type_urls
            .iter()
            .filter_map(|((t, _), name)| {
                if *t == ext_type {
                    Some(name.as_str())
                } else {
                    None
                }
            })
            .collect();
        names.sort_unstable();
        names.dedup();
        names
    }

    /// Check if an extension is registered for a specific ExtensionType
    pub fn has_extension(&self, ext_type: ExtensionType, name: &str) -> bool {
        self.handlers.contains_key(&(ext_type, name.to_string()))
    }
}

impl fmt::Debug for ExtensionRegistry {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut keys: Vec<_> = self
            .handlers
            .keys()
            .map(|(t, n)| (format!("{t:?}"), n.as_str()))
            .collect();
        keys.sort();
        f.debug_struct("ExtensionRegistry")
            .field("handlers", &keys)
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
                .map_err(|e| ExtensionError::Custom(format!("Invalid UTF-8: {e}")))?;

            // Simple manual parsing for test
            if json_str.contains("path") && json_str.contains("batch_size") {
                Ok(TestExtension {
                    path: "test.parquet".to_string(),
                    batch_size: 1024,
                })
            } else {
                Err(ExtensionError::Custom("Missing fields".to_string()))
            }
        }
    }

    impl Explainable for TestExtension {
        fn name() -> &'static str {
            "TestExtension"
        }

        fn from_args(args: &ExtensionArgs, _ctx: &ExplainContext) -> Result<Self, ExtensionError> {
            let mut extractor = args.extractor();
            let path: String = extractor.expect_named_arg::<&str>("path")?.to_string();
            let batch_size: i64 = extractor.expect_named_arg("batch_size")?;
            extractor.check_exhausted()?;

            Ok(TestExtension {
                path: path.to_string(),
                batch_size,
            })
        }

        fn to_args(&self, _ctx: &ExplainContext) -> Result<ExtensionArgs, ExtensionError> {
            let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf);
            args.named.insert(
                "path".to_string(),
                ExtensionValue::String(self.path.clone()),
            );
            args.named.insert(
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
        assert_eq!(registry.extension_names(ExtensionType::Relation).len(), 0);
        assert!(!registry.has_extension(ExtensionType::Relation, "TestExtension"));

        // Register extension type
        registry.register_relation::<TestExtension>().unwrap();

        // Now has extension
        assert_eq!(registry.extension_names(ExtensionType::Relation).len(), 1);
        assert!(registry.has_extension(ExtensionType::Relation, "TestExtension"));

        // Test parse and textify
        let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf);
        args.named.insert(
            "path".to_string(),
            ExtensionValue::String("data.parquet".to_string()),
        );
        args.named
            .insert("batch_size".to_string(), ExtensionValue::Integer(2048));

        let ext = SimpleExtensions::default();
        let any = registry
            .parse_extension("TestExtension", &args, &ext)
            .unwrap();
        assert_eq!(any.type_url, "test.TestExtension");

        let any_ref = any.as_ref();
        let result = registry.decode(any_ref, &ext).unwrap();
        assert_eq!(result.0, "TestExtension");
        match result.1.named.get("path") {
            Some(ExtensionValue::String(s)) => assert_eq!(s, "test.parquet"), // Due to our simple test impl
            _ => panic!("Expected String for path"),
        }
    }

    #[test]
    fn test_extension_args() {
        let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf);

        // Add named args
        args.named.insert(
            "path".to_string(),
            ExtensionValue::String("data/*.parquet".to_string()),
        );
        args.named
            .insert("batch_size".to_string(), ExtensionValue::Integer(1024));

        // Add positional args
        args.positional.push(ExtensionValue::Reference(0));

        // Add output columns
        args.output_columns.push(ExtensionColumn::Named {
            name: "col1".to_string(),
            r#type: crate::fixtures::parse_type("i32"),
        });

        // Test retrieval - use extractor
        let mut extractor = args.extractor();

        match extractor.get_named_arg("path") {
            Some(ExtensionValue::String(s)) => assert_eq!(s, "data/*.parquet"),
            _ => panic!("Expected String for path"),
        }

        match extractor.get_named_arg("batch_size") {
            Some(ExtensionValue::Integer(i)) => assert_eq!(*i, 1024),
            _ => panic!("Expected Integer for batch_size"),
        }

        // Verify they were consumed
        assert!(extractor.check_exhausted().is_ok());

        assert_eq!(args.positional.len(), 1);
        assert_eq!(args.output_columns.len(), 1);
    }

    #[test]
    fn test_extension_error_cases() {
        let registry = ExtensionRegistry::new();

        // Extension not found
        let args = ExtensionArgs::new(ExtensionRelationType::Leaf);
        let ext = SimpleExtensions::default();
        let result = registry.parse_extension("NonExistent", &args, &ext);
        assert!(matches!(result, Err(ExtensionError::NotFound { .. })));

        // Missing argument
        let args = ExtensionArgs::new(ExtensionRelationType::Leaf);
        let mut extractor = args.extractor();
        let result = extractor.get_named_arg("missing");
        assert!(result.is_none());
        assert!(extractor.check_exhausted().is_ok());

        // Type check example
        let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf);
        args.named
            .insert("test".to_string(), ExtensionValue::Integer(42));
        let mut extractor = args.extractor();
        let result = extractor.get_named_arg("test");
        match result {
            Some(ExtensionValue::Integer(42)) => {} // Expected
            _ => panic!("Expected Integer(42), got {result:?}"),
        }
        assert!(extractor.check_exhausted().is_ok());
    }

    // Mock enhancement type for testing namespace separation
    struct TestEnhancement {
        hint: String,
    }

    impl AnyConvertible for TestEnhancement {
        fn to_any(&self) -> Result<Any, ExtensionError> {
            let json_str = format!(r#"{{"hint":"{}"}}"#, self.hint);
            Ok(Any::new(Self::type_url(), json_str.into_bytes()))
        }

        fn type_url() -> String {
            // Same type URL as TestExtension to test namespace separation
            "test.TestExtension".to_string()
        }

        fn from_any<'a>(any: AnyRef<'a>) -> Result<Self, ExtensionError> {
            let json_str = String::from_utf8(any.value.to_vec())
                .map_err(|e| ExtensionError::Custom(format!("Invalid UTF-8: {e}")))?;
            if json_str.contains("hint") {
                Ok(TestEnhancement {
                    hint: "test_hint".to_string(),
                })
            } else {
                Err(ExtensionError::Custom("Missing hint field".to_string()))
            }
        }
    }

    impl Explainable for TestEnhancement {
        fn name() -> &'static str {
            "TestEnhancement"
        }

        fn from_args(args: &ExtensionArgs, _ctx: &ExplainContext) -> Result<Self, ExtensionError> {
            let mut extractor = args.extractor();
            let hint: String = extractor.expect_named_arg::<&str>("hint")?.to_string();
            extractor.check_exhausted()?;
            Ok(TestEnhancement { hint })
        }

        fn to_args(&self, _ctx: &ExplainContext) -> Result<ExtensionArgs, ExtensionError> {
            let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf);
            args.named.insert(
                "hint".to_string(),
                ExtensionValue::String(self.hint.clone()),
            );
            Ok(args)
        }
    }

    #[test]
    fn test_namespace_separation() {
        let mut registry = ExtensionRegistry::new();

        // Register same type URL in both namespaces - should not conflict
        registry.register_relation::<TestExtension>().unwrap();
        registry.register_enhancement::<TestEnhancement>().unwrap();

        // Verify both are registered
        assert!(registry.has_extension(ExtensionType::Relation, "TestExtension"));
        assert!(registry.has_extension(ExtensionType::Enhancement, "TestEnhancement"));
        assert_eq!(registry.extension_names(ExtensionType::Relation).len(), 1);
        assert_eq!(
            registry.extension_names(ExtensionType::Enhancement).len(),
            1
        );

        // Test that extension namespace works
        let ext = SimpleExtensions::default();
        let mut ext_args = ExtensionArgs::new(ExtensionRelationType::Leaf);
        ext_args.named.insert(
            "path".to_string(),
            ExtensionValue::String("data.parquet".to_string()),
        );
        ext_args
            .named
            .insert("batch_size".to_string(), ExtensionValue::Integer(2048));

        let ext_any = registry
            .parse_extension("TestExtension", &ext_args, &ext)
            .unwrap();
        assert_eq!(ext_any.type_url, "test.TestExtension");

        // Test that enhancement namespace works
        let mut enh_args = ExtensionArgs::new(ExtensionRelationType::Leaf);
        enh_args.named.insert(
            "hint".to_string(),
            ExtensionValue::String("optimize".to_string()),
        );

        let enh_any = registry
            .parse_enhancement("TestEnhancement", &enh_args, &ext)
            .unwrap();
        assert_eq!(enh_any.type_url, "test.TestExtension"); // Same type URL!

        // Test decode_enhancement
        let enh_ref = enh_any.as_ref();
        let (name, args) = registry.decode_enhancement(enh_ref, &ext).unwrap();
        assert_eq!(name, "TestEnhancement");
        match args.named.get("hint") {
            Some(ExtensionValue::String(s)) => assert_eq!(s, "test_hint"), // Due to test impl
            _ => panic!("Expected String for hint"),
        }
    }

    #[test]
    fn test_enhancement_duplicate_registration_returns_error() {
        let mut registry = ExtensionRegistry::new();
        registry.register_enhancement::<TestEnhancement>().unwrap();
        let result = registry.register_enhancement::<TestEnhancement>();
        assert!(matches!(
            result,
            Err(RegistrationError::DuplicateName { .. })
        ));
    }

    #[test]
    fn test_enhancement_not_found_error() {
        let registry = ExtensionRegistry::new();
        let args = ExtensionArgs::new(ExtensionRelationType::Leaf);
        let ext = SimpleExtensions::default();
        let result = registry.parse_enhancement("NonExistentEnhancement", &args, &ext);
        assert!(matches!(result, Err(ExtensionError::NotFound { .. })));
    }

    // Extension with same type URL as TestExtension but different name,
    // used to test that conflicting type URLs don't leave stale state.
    struct ConflictingExtension;

    impl AnyConvertible for ConflictingExtension {
        fn to_any(&self) -> Result<Any, ExtensionError> {
            Ok(Any::new(Self::type_url(), vec![]))
        }

        fn type_url() -> String {
            // Same type URL as TestExtension — will conflict in the same namespace
            "test.TestExtension".to_string()
        }

        fn from_any<'a>(_any: AnyRef<'a>) -> Result<Self, ExtensionError> {
            Ok(ConflictingExtension)
        }
    }

    impl Explainable for ConflictingExtension {
        fn name() -> &'static str {
            "ConflictingExtension"
        }

        fn from_args(_args: &ExtensionArgs, _ctx: &ExplainContext) -> Result<Self, ExtensionError> {
            Ok(ConflictingExtension)
        }

        fn to_args(&self, _ctx: &ExplainContext) -> Result<ExtensionArgs, ExtensionError> {
            Ok(ExtensionArgs::new(ExtensionRelationType::Leaf))
        }
    }

    #[test]
    fn test_conflicting_type_url_leaves_registry_unchanged() {
        let mut registry = ExtensionRegistry::new();
        registry.register_relation::<TestExtension>().unwrap();

        // Attempt to register a different extension with the same type URL
        let result = registry.register_relation::<ConflictingExtension>();
        assert!(matches!(
            result,
            Err(RegistrationError::ConflictingTypeUrl { .. })
        ));

        // Registry should still only know about the original extension
        assert!(registry.has_extension(ExtensionType::Relation, "TestExtension"));
        assert!(!registry.has_extension(ExtensionType::Relation, "ConflictingExtension"));
        assert_eq!(
            registry.extension_names(ExtensionType::Relation),
            vec!["TestExtension"]
        );
    }
}
