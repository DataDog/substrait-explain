//! Substrait protobuf → human-readable text conversion.
//!
//! The core pattern: each protobuf type implements [`Textify`], writing itself
//! into a [`std::fmt::Write`] with a [`Scope`] context that provides formatting
//! options, extension lookups, and error accumulation. [`plan::PlanWriter`]
//! is the top-level entry point.

pub mod expressions;
pub mod extensions;
pub mod foundation;
pub mod plan;
pub mod rels;
pub mod types;

pub use foundation::{
    ErrorQueue, FormatErrorType, OutputOptions, PlanError, Scope, ScopedContext, Textify,
    Visibility,
};
