//! Standard relation parsing and lowering.
//!
//! `ast` defines semantic line payloads parsed from typed grammar nodes.
//! `core` parses typed relation nodes into that AST and lowers them into
//! Substrait `Rel` values.

pub(crate) mod ast;
pub(crate) mod core;

pub(crate) use core::*;
