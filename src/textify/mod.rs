//! Output a plan in text format.

mod addenda;
pub(crate) mod expressions;
pub(crate) mod extensions;
pub(crate) mod foundation;
pub(crate) mod plan;
pub(crate) mod rels;
pub(crate) mod types;

#[cfg(test)]
pub(crate) use foundation::ErrorQueue;
pub(crate) use foundation::{OutputOptions, PlanError, Scope, ScopedContext, Textify, Visibility};
