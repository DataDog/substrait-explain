//! Output Substrait values in text format.
//!
//! Use [`Writer`] for fragment values (`Type`, `Expression`, etc.) and
//! [`plan::PlanWriter`] for full-plan rendering with extension sections.

pub mod expressions;
pub mod extensions;
pub mod foundation;
pub mod plan;
pub mod rels;
pub mod types;
pub mod writer;

pub use foundation::{
    ErrorQueue, FormatErrorType, OutputOptions, PlanError, Scope, ScopedContext, Textify,
    Visibility,
};
pub use writer::Writer;
