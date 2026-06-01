//! Registry for custom Substrait advanced extension payloads.
//!
//! This module lets users register handlers for advanced extensions that carry
//! `google.protobuf.Any` detail payloads: custom relation types, relation
//! enhancements, and optimization hints.
//!
//! # Overview
//!
//! The extension registry allows users to:
//! - Register custom extension handlers in relation, enhancement, or
//!   optimization namespaces
//! - Parse extension arguments/named arguments into `google.protobuf.Any`
//!   detail fields
//! - Textify extension detail fields back into readable text format
//! - Keep each registered payload's protobuf type URL associated with its
//!   canonical text-format name
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
//!     Any, AnyConvertible, AnyRef, Explainable, ExtensionArgs, ExtensionError, ExtensionRegistry,
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
//!     fn from_args(args: &ExtensionArgs) -> Result<Self, ExtensionError> {
//!         let mut extractor = args.extractor();
//!         let path: &str = extractor.expect_named_arg("path")?;
//!         extractor.check_exhausted()?;
//!         Ok(CustomScanConfig {
//!             path: path.to_string(),
//!         })
//!     }
//!
//!     fn to_args(&self) -> Result<ExtensionArgs, ExtensionError> {
//!         let mut args = ExtensionArgs::default();
//!         args.insert("path", self.path.clone());
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

use substrait::proto::NamedStruct;
use substrait::proto::r#type::{Nullability, Struct};
use thiserror::Error;

use crate::extensions::any::{Any, AnyRef};
use crate::extensions::args::{ExtensionArgs, ExtensionColumn, ExtensionValueKind};

/// Type of extension in the registry, used for namespace separation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExtensionType {
    /// Relation extension (e.g., ExtensionLeaf, ExtensionSingle, ExtensionMulti)
    Relation,
    /// ExtensionTable detail attached to a ReadRel (uses `+ Ext:` prefix in text format)
    ExtensionTable,
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

    /// Invalid argument type found while extracting an extension argument.
    #[error("Invalid argument: expected {expected}, got {actual}")]
    InvalidArgumentType {
        expected: ExtensionValueKind,
        actual: ExtensionValueKind,
    },

    /// Invalid argument value with a custom diagnostic.
    ///
    /// Prefer structured variants for common mechanical validation failures.
    /// Use this for domain-specific validation from `Explainable`
    /// implementations.
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

/// Conversion between extension arguments and Substrait protobuf values.
///
/// Extension arguments are the structured values exposed to [`Explainable`]
/// implementations, such as [`ExtensionArgs`],
/// [`ExtensionValue`](crate::extensions::ExtensionValue), and
/// [`ExtensionColumn`]. This trait adapts those values to and from protobuf
/// types without going through text.
///
/// Implementations may convert in either direction; the target type `T`
/// determines the direction.
pub trait ExtensionProtoConvert<T> {
    /// Convert this value into `T`.
    fn convert(&self) -> Result<T, ExtensionError>;
}

impl ExtensionProtoConvert<NamedStruct> for [ExtensionColumn] {
    fn convert(&self) -> Result<NamedStruct, ExtensionError> {
        let mut names = Vec::with_capacity(self.len());
        let mut types = Vec::with_capacity(self.len());
        for col in self {
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
            r#struct: Some(Struct {
                types,
                type_variation_reference: 0,
                // In Substrait, the schema of a type is defined as
                // non-nullable; you can have an empty schema (no columns), but
                // not a null schema.
                nullability: Nullability::Required as i32,
            }),
        })
    }
}

impl ExtensionProtoConvert<Vec<ExtensionColumn>> for NamedStruct {
    fn convert(&self) -> Result<Vec<ExtensionColumn>, ExtensionError> {
        let types = self
            .r#struct
            .as_ref()
            .map(|s| s.types.as_slice())
            .unwrap_or_default();
        if self.names.len() != types.len() {
            return Err(ExtensionError::InvalidArgument(format!(
                "NamedStruct has {} names but {} types",
                self.names.len(),
                types.len()
            )));
        }
        Ok(self
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

    /// Parse extension arguments into this type
    fn from_args(args: &ExtensionArgs) -> Result<Self, ExtensionError>;

    /// Convert this type to extension arguments
    fn to_args(&self) -> Result<ExtensionArgs, ExtensionError>;
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
/// - The protobuf Any messages stored in Substrait advanced extension payloads
///
/// This design allows the registry to work with any type while maintaining type safety
/// through the AnyConvertible and Explainable traits that users implement.
trait ExtensionConverter: Send + Sync {
    fn parse_detail(&self, args: &ExtensionArgs) -> Result<Any, ExtensionError>;

    fn textify_detail(&self, detail: AnyRef<'_>) -> Result<ExtensionArgs, ExtensionError>;
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
        T::from_args(args)?.to_any()
    }

    fn textify_detail(&self, detail: AnyRef<'_>) -> Result<ExtensionArgs, ExtensionError> {
        let owned_any = Any::new(detail.type_url.to_string(), detail.value.to_vec());
        T::from_any(owned_any.as_ref())?.to_args()
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

    /// Register an ExtensionTable detail type that implements both AnyConvertible and Explainable
    ///
    /// ExtensionTable details are registered in a separate namespace from
    /// extension relations, allowing the same type URL to exist in both namespaces
    /// without conflict.
    ///
    /// The canonical textual name comes from `T::name()`.
    pub fn register_extension_table<T>(&mut self) -> Result<(), RegistrationError>
    where
        T: Extension,
    {
        self.register::<T>(ExtensionType::ExtensionTable)
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
    ) -> Result<Any, ExtensionError> {
        self.parse_with_type(ExtensionType::Relation, extension_name, args)
    }

    /// Parse ExtensionTable arguments into a protobuf Any message
    ///
    /// Looks up the ExtensionTable detail handler in the ExtensionTable namespace
    /// and parses the arguments into a protobuf Any message.
    pub fn parse_extension_table(
        &self,
        extension_table_name: &str,
        args: &ExtensionArgs,
    ) -> Result<Any, ExtensionError> {
        self.parse_with_type(ExtensionType::ExtensionTable, extension_table_name, args)
    }

    /// Parse enhancement arguments into a protobuf Any message
    ///
    /// Looks up the enhancement handler in the enhancement namespace and parses
    /// the arguments into a protobuf Any message.
    pub fn parse_enhancement(
        &self,
        enhancement_name: &str,
        args: &ExtensionArgs,
    ) -> Result<Any, ExtensionError> {
        self.parse_with_type(ExtensionType::Enhancement, enhancement_name, args)
    }

    /// Parse optimization arguments into a protobuf Any message
    ///
    /// Looks up the optimization handler in the optimization namespace and parses
    /// the arguments into a protobuf Any message.
    pub fn parse_optimization(
        &self,
        optimization_name: &str,
        args: &ExtensionArgs,
    ) -> Result<Any, ExtensionError> {
        self.parse_with_type(ExtensionType::Optimization, optimization_name, args)
    }

    /// Internal method to parse extension arguments with a specific ExtensionType
    fn parse_with_type(
        &self,
        ext_type: ExtensionType,
        name: &str,
        args: &ExtensionArgs,
    ) -> Result<Any, ExtensionError> {
        let key = (ext_type, name.to_string());
        let handler = self
            .handlers
            .get(&key)
            .ok_or_else(|| ExtensionError::NotFound {
                name: name.to_string(),
            })?;
        handler.parse_detail(args)
    }

    /// Decode extension detail to extension name and ExtensionArgs
    /// This is the primary method for textification - given an AnyRef with extension detail,
    /// decode it to the extension name and appropriate ExtensionArgs for display
    pub fn decode(&self, detail: AnyRef<'_>) -> Result<(String, ExtensionArgs), ExtensionError> {
        self.decode_with_type(ExtensionType::Relation, detail)
    }

    /// Decode ExtensionTable detail to extension name and ExtensionArgs
    ///
    /// This is the primary method for textification of ExtensionTable reads -
    /// given an AnyRef with ExtensionTable detail, decode it to the extension
    /// name and appropriate ExtensionArgs for display.
    pub fn decode_extension_table(
        &self,
        detail: AnyRef<'_>,
    ) -> Result<(String, ExtensionArgs), ExtensionError> {
        self.decode_with_type(ExtensionType::ExtensionTable, detail)
    }

    /// Decode enhancement detail to enhancement name and ExtensionArgs
    ///
    /// This is the primary method for textification of enhancements - given an AnyRef
    /// with enhancement detail, decode it to the enhancement name and appropriate
    /// ExtensionArgs for display.
    ///
    /// Looks up the enhancement handler in the enhancement namespace by type URL.
    pub fn decode_enhancement(
        &self,
        detail: AnyRef<'_>,
    ) -> Result<(String, ExtensionArgs), ExtensionError> {
        self.decode_with_type(ExtensionType::Enhancement, detail)
    }

    /// Decode optimization detail to optimization name and ExtensionArgs
    ///
    /// This is the primary method for textification of optimizations - given an AnyRef
    /// with optimization detail, decode it to the optimization name and appropriate
    /// ExtensionArgs for display.
    ///
    /// Looks up the optimization handler in the optimization namespace by type URL.
    pub fn decode_optimization(
        &self,
        detail: AnyRef<'_>,
    ) -> Result<(String, ExtensionArgs), ExtensionError> {
        self.decode_with_type(ExtensionType::Optimization, detail)
    }

    /// Internal method to decode extension detail with a specific ExtensionType
    fn decode_with_type(
        &self,
        ext_type: ExtensionType,
        detail: AnyRef<'_>,
    ) -> Result<(String, ExtensionArgs), ExtensionError> {
        // Find extension name by type URL in the specified namespace
        let type_url_key = (ext_type, detail.type_url.to_string());
        let extension_name =
            self.type_urls
                .get(&type_url_key)
                .ok_or_else(|| ExtensionError::NotFound {
                    name: detail.type_url.to_string(),
                })?;

        // Get handler and textify the detail
        let name_key = (ext_type, extension_name.clone());
        let handler = self
            .handlers
            .get(&name_key)
            .ok_or_else(|| ExtensionError::NotFound {
                name: extension_name.clone(),
            })?;

        let args = handler.textify_detail(detail)?;

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
    use crate::extensions::ExtensionColumn;

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

        fn from_args(args: &ExtensionArgs) -> Result<Self, ExtensionError> {
            let mut extractor = args.extractor();
            let path: String = extractor.expect_named_arg::<&str>("path")?.to_string();
            let batch_size: i64 = extractor.expect_named_arg("batch_size")?;
            extractor.check_exhausted()?;

            Ok(TestExtension {
                path: path.to_string(),
                batch_size,
            })
        }

        fn to_args(&self) -> Result<ExtensionArgs, ExtensionError> {
            let mut args = ExtensionArgs::default();
            args.insert("path", self.path.clone());
            args.insert("batch_size", self.batch_size);
            Ok(args)
        }
    }

    #[test]
    fn test_extension_registry_basic() {
        let mut registry = ExtensionRegistry::new();

        // Initially empty
        assert_eq!(registry.extension_names(ExtensionType::Relation).len(), 0);
        assert_eq!(
            registry
                .extension_names(ExtensionType::ExtensionTable)
                .len(),
            0
        );
        assert!(!registry.has_extension(ExtensionType::Relation, "TestExtension"));

        // Register extension type
        registry.register_relation::<TestExtension>().unwrap();

        // Now has extension
        assert_eq!(registry.extension_names(ExtensionType::Relation).len(), 1);
        assert!(registry.has_extension(ExtensionType::Relation, "TestExtension"));

        // Test parse and textify
        let mut args = ExtensionArgs::default();
        args.insert("path", "data.parquet");
        args.insert("batch_size", 2048_i64);

        let any = registry.parse_extension("TestExtension", &args).unwrap();
        assert_eq!(any.type_url, "test.TestExtension");

        let any_ref = any.as_ref();
        let result = registry.decode(any_ref).unwrap();
        assert_eq!(result.0, "TestExtension");
        assert_eq!(
            <&str>::try_from(result.1.named.get("path").unwrap()).unwrap(),
            "test.parquet"
        );
    }

    #[test]
    fn test_extension_table_registry_basic() {
        let mut registry = ExtensionRegistry::new();

        registry
            .register_extension_table::<TestExtension>()
            .unwrap();

        assert_eq!(
            registry.extension_names(ExtensionType::ExtensionTable),
            vec!["TestExtension"]
        );
        assert!(registry.has_extension(ExtensionType::ExtensionTable, "TestExtension"));

        let mut args = ExtensionArgs::default();
        args.insert("path", "data.parquet");
        args.insert("batch_size", 2048_i64);

        let any = registry
            .parse_extension_table("TestExtension", &args)
            .unwrap();
        assert_eq!(any.type_url, "test.TestExtension");

        let (name, decoded_args) = registry.decode_extension_table(any.as_ref()).unwrap();
        assert_eq!(name, "TestExtension");
        assert_eq!(
            <&str>::try_from(decoded_args.named.get("path").unwrap()).unwrap(),
            "test.parquet"
        );
    }

    #[test]
    fn test_extension_args() {
        let mut args = ExtensionArgs::default();

        // Add named args
        args.insert("path", "data/*.parquet");
        args.insert("batch_size", 1024_i64);

        // Add positional args
        args.push(crate::textify::expressions::Reference(0));

        // Add output columns
        args.output_columns.push(ExtensionColumn::Named {
            name: "col1".to_string(),
            r#type: crate::fixtures::parse_type("i32"),
        });

        // Test retrieval - use extractor
        let mut extractor = args.extractor();

        let path = extractor.get_named_arg("path").unwrap();
        assert_eq!(<&str>::try_from(path).unwrap(), "data/*.parquet");

        let batch_size = extractor.get_named_arg("batch_size").unwrap();
        assert_eq!(i64::try_from(batch_size).unwrap(), 1024);

        // Verify they were consumed
        assert!(extractor.check_exhausted().is_ok());

        assert_eq!(args.positional.len(), 1);
        assert_eq!(args.output_columns.len(), 1);
    }

    #[test]
    fn test_extension_error_cases() {
        let registry = ExtensionRegistry::new();

        // Extension not found
        let args = ExtensionArgs::default();
        let result = registry.parse_extension("NonExistent", &args);
        assert!(matches!(result, Err(ExtensionError::NotFound { .. })));

        // Missing argument
        let args = ExtensionArgs::default();
        let mut extractor = args.extractor();
        let result = extractor.get_named_arg("missing");
        assert!(result.is_none());
        assert!(extractor.check_exhausted().is_ok());

        // Type check example
        let mut args = ExtensionArgs::default();
        args.insert("test", 42_i64);
        let mut extractor = args.extractor();
        let result = extractor.get_named_arg("test");
        assert_eq!(i64::try_from(result.unwrap()).unwrap(), 42);
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

        fn from_args(args: &ExtensionArgs) -> Result<Self, ExtensionError> {
            let mut extractor = args.extractor();
            let hint: String = extractor.expect_named_arg::<&str>("hint")?.to_string();
            extractor.check_exhausted()?;
            Ok(TestEnhancement { hint })
        }

        fn to_args(&self) -> Result<ExtensionArgs, ExtensionError> {
            let mut args = ExtensionArgs::default();
            args.insert("hint", self.hint.clone());
            Ok(args)
        }
    }

    #[test]
    fn test_namespace_separation() {
        let mut registry = ExtensionRegistry::new();

        // Register same type URL in multiple namespaces - should not conflict
        registry.register_relation::<TestExtension>().unwrap();
        registry
            .register_extension_table::<TestExtension>()
            .unwrap();
        registry.register_enhancement::<TestEnhancement>().unwrap();

        // Verify all are registered
        assert!(registry.has_extension(ExtensionType::Relation, "TestExtension"));
        assert!(registry.has_extension(ExtensionType::ExtensionTable, "TestExtension"));
        assert!(registry.has_extension(ExtensionType::Enhancement, "TestEnhancement"));
        assert_eq!(registry.extension_names(ExtensionType::Relation).len(), 1);
        assert_eq!(
            registry
                .extension_names(ExtensionType::ExtensionTable)
                .len(),
            1
        );
        assert_eq!(
            registry.extension_names(ExtensionType::Enhancement).len(),
            1
        );

        // Test that extension namespace works
        let mut ext_args = ExtensionArgs::default();
        ext_args.insert("path", "data.parquet");
        ext_args.insert("batch_size", 2048_i64);

        let ext_any = registry
            .parse_extension("TestExtension", &ext_args)
            .unwrap();
        assert_eq!(ext_any.type_url, "test.TestExtension");

        // Test that ExtensionTable namespace works independently
        let table_any = registry
            .parse_extension_table("TestExtension", &ext_args)
            .unwrap();
        assert_eq!(table_any.type_url, "test.TestExtension");

        // Test that enhancement namespace works
        let mut enh_args = ExtensionArgs::default();
        enh_args.insert("hint", "optimize");

        let enh_any = registry
            .parse_enhancement("TestEnhancement", &enh_args)
            .unwrap();
        assert_eq!(enh_any.type_url, "test.TestExtension"); // Same type URL!

        // Test decode_enhancement
        let enh_ref = enh_any.as_ref();
        let (name, args) = registry.decode_enhancement(enh_ref).unwrap();
        assert_eq!(name, "TestEnhancement");
        assert_eq!(
            <&str>::try_from(args.named.get("hint").unwrap()).unwrap(),
            "test_hint"
        );
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
    fn test_extension_table_duplicate_registration_returns_error() {
        let mut registry = ExtensionRegistry::new();
        registry
            .register_extension_table::<TestExtension>()
            .unwrap();
        let result = registry.register_extension_table::<TestExtension>();
        assert!(matches!(
            result,
            Err(RegistrationError::DuplicateName { .. })
        ));
    }

    #[test]
    fn test_extension_table_not_found_error() {
        let registry = ExtensionRegistry::new();
        let args = ExtensionArgs::default();
        let result = registry.parse_extension_table("NonExistentExtensionTable", &args);
        assert!(matches!(result, Err(ExtensionError::NotFound { .. })));
    }

    #[test]
    fn test_enhancement_not_found_error() {
        let registry = ExtensionRegistry::new();
        let args = ExtensionArgs::default();
        let result = registry.parse_enhancement("NonExistentEnhancement", &args);
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

        fn from_args(_args: &ExtensionArgs) -> Result<Self, ExtensionError> {
            Ok(ConflictingExtension)
        }

        fn to_args(&self) -> Result<ExtensionArgs, ExtensionError> {
            Ok(ExtensionArgs::default())
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

    #[test]
    fn test_extension_table_conflicting_type_url_leaves_registry_unchanged() {
        let mut registry = ExtensionRegistry::new();
        registry
            .register_extension_table::<TestExtension>()
            .unwrap();

        // Attempt to register a different extension table with the same type URL
        let result = registry.register_extension_table::<ConflictingExtension>();
        assert!(matches!(
            result,
            Err(RegistrationError::ConflictingTypeUrl { .. })
        ));

        // Registry should still only know about the original extension table
        assert!(registry.has_extension(ExtensionType::ExtensionTable, "TestExtension"));
        assert!(!registry.has_extension(ExtensionType::ExtensionTable, "ConflictingExtension"));
        assert_eq!(
            registry.extension_names(ExtensionType::ExtensionTable),
            vec!["TestExtension"]
        );
    }
}
