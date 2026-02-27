//! Expression fragment parsing and lowering.
//!
//! This module contains typed-node parsers for literals, references, scalar
//! functions, and full expressions, plus `ParseFragment` implementations for
//! expression-related protobuf messages.

pub(crate) mod core;
