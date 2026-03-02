//! Classes for handling extensions, including the simple extensions that
//! represent functions, types, and type variations that can appear in Substrait
//! simple extension YAML files, and the extension registry system for custom
//! extension relations.

pub mod any;
pub mod args;
pub mod registry;
pub mod simple;

pub use any::{Any, AnyRef};
pub(crate) use args::RawExpression;
pub use args::{ExtensionArgs, ExtensionColumn, ExtensionRelationType, ExtensionValue};
pub use registry::{
    AnyConvertible, Explainable, Extension, ExtensionError, ExtensionRegistry, ExtensionType,
    RegistrationError,
};
pub use simple::{InsertError, SimpleExtension, SimpleExtensions};
