//! Output a plan in text format.

mod addenda;
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
