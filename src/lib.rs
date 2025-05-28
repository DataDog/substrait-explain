//! This crate provides a library for displaying Substrait types in a human
//! readable, EXPLAIN-like format. This crate provides a library for displaying
//! Substrait types in a human readable, EXPLAIN-like format.
//!
//! ### Goals
//!
//!   1. Be easy to understand:
//!      - Use a simple, human-readable format
//!      - Keep it concise - ideally, one line per relational operation
//!      - References should be resolved to human-readable names
//!   2. Be easy to parse:
//!      - There should be a consistency in the way the output is written that
//!        is reasonably parseable
//!   3. Be easy to extend:
//!      - Extensions should be as easy to use with this crate as with Substrait
//!        itself
//!   4. Be semantically complete:
//!      - The output should be semantically complete, and should map to a
//!        semantically identical Substrait plan.
//!      - The output should map structurally to Substrait when it does not
//!        compromise readability; when it does conflict, default output should
//!        be semantically equivalent, if not structurally, with 'verbose' mode
//!        providing exact structural equivalence. As an example, `Emit` is
//!        generally not shown via references, instead reordering the columns as
//!        shown to show what the `Emit` would resolve to.
//!
//! ### Design Goals
//!
//!   1. Error accumulation:
//!      - Any unimplemented or failed features not cause translation to fail,
//!        but should be accumulated, and returned to the user.

mod expressions;
#[cfg(test)]
mod fixtures;
mod textify;
mod types;

pub use textify::{ExtensionLookup, OutputOptions, SimpleExtensions, Textify, TextifyError};
