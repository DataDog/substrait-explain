//! Support for Substrait simple and advanced extensions.
//!
//! Simple extensions describe YAML/URN-backed extension entities such as
//! functions, types, and type variations. Advanced extensions carry
//! `google.protobuf.Any` payloads for custom relation types, relation
//! enhancements, and optimization hints.
//!
//! The registry APIs connect those protobuf payloads to text-format arguments
//! by way of [`Explainable`] implementations.

pub mod any;
pub mod args;
pub mod examples;
pub mod registry;
pub mod simple;

pub use any::{Any, AnyRef};
pub(crate) use args::AddendumKind;
pub use args::{
    EnumValue, Expr, ExtensionArgs, ExtensionColumn, ExtensionValue, ExtensionValueKind, TupleValue,
};
pub use registry::{
    AnyConvertible, Explainable, Extension, ExtensionError, ExtensionProtoConvert,
    ExtensionRegistry, ExtensionType, RegistrationError,
};
pub use simple::{InsertError, SimpleExtension, SimpleExtensions};
