//! Classes for handling extensions, including the simple extensions that
//! represent functions, types, and type variations that can appear in Substrait
//! simple extension YAML files.

pub mod simple;

pub use simple::{ExtensionError, SimpleExtension, SimpleExtensions};
