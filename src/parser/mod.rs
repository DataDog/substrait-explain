//! Parser entrypoints and parser-internal module wiring.
//!
//! The public parser API is centered on [`Parser`] for plan parsing and
//! [`ParseFragment`] for fragment parsing (`Type`, `Expression`, etc.).
//! Internally, modules are split by responsibility:
//! - `structural`: plan section/state machine and tree assembly
//! - `relations`, `expressions`, `types`, `extensions`: typed lowering
//! - `common`, `convert`, `errors`: shared parser infrastructure
//!
//! This module re-exports the public error and parser types while keeping most
//! parser internals crate-private.

pub(crate) mod common;
pub(crate) mod convert;
pub(crate) mod errors;
pub(crate) mod expressions;
pub(crate) mod extensions;
pub(crate) mod relations;
pub(crate) mod structural;
pub(crate) mod types;

pub use common::MessageParseError;
pub use errors::{ParseContext, ParseError, ParseResult};
pub use structural::{PLAN_HEADER, Parser};

use crate::extensions::SimpleExtensions;

/// Parse a non-plan Substrait value from text.
///
/// Implement this for additional fragment-like values that can be parsed from
/// text with a [`SimpleExtensions`] context. [`Parser::parse_fragment`] is the
/// primary entrypoint that dispatches to this trait.
pub trait ParseFragment: Sized {
    /// Parse a fragment value from text using the provided simple extensions.
    fn parse_fragment(
        extensions: &SimpleExtensions,
        input: &str,
    ) -> Result<Self, MessageParseError>;
}
