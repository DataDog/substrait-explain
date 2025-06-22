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
//!   5. Graceful errors:
//!      - Errors should not prevent the output from being generated, but should
//!        be returned.
//!
//! ### Design Goals
//!
//!   1. Error accumulation:
//!      - Any unimplemented or failed features not cause translation to fail,
//!        but should be accumulated, and returned to the user.

pub mod extensions;
pub mod fixtures;
pub mod parser;
pub mod textify;

// Re-export commonly used types for easier access
pub use parser::{ParseError, Parser};
use substrait::proto::Plan;
use textify::foundation::ErrorQueue;
pub use textify::foundation::{Error as TextifyError, OutputOptions};
use textify::plan::PlanWriter;

/// Format a Substrait plan to text format.
///
/// This is the main entry point for formatting plans to text.
/// The plan may not be entirely well-formed - errors will be marked in the output
/// with `!{error_type}` placeholders, but the function will still produce output
/// for everything that can be formatted.
///
/// Returns both the formatted text and any errors that occurred during formatting.
/// Users can ignore the errors if they don't need them.
///
/// # Example
/// ```rust
/// use substrait_explain::format;
/// use substrait::proto::Plan;
///
/// let plan = Plan::default();
/// let (text, errors) = format(&plan);
/// println!("{}", text);
///
/// if !errors.is_empty() {
///     println!("Formatting errors:");
///     for error in errors {
///         println!("  {}", error);
///     }
/// }
/// ```
pub fn format(plan: &Plan) -> (String, Vec<TextifyError>) {
    let options = OutputOptions::default();
    let (writer, error_queue) = PlanWriter::<ErrorQueue>::new(&options, plan);
    let text = format!("{writer}");

    // Collect errors from the error queue
    let errors: Vec<TextifyError> = error_queue.into_iter().collect();

    (text, errors)
}

/// Format a Substrait plan to text format with custom options.
///
/// This allows you to customize the output format (e.g., verbose mode).
/// Returns both the formatted text and any errors that occurred during formatting.
///
/// # Example
/// ```rust
/// use substrait_explain::{format_with_options, OutputOptions};
/// use substrait::proto::Plan;
///
/// let plan = Plan::default();
/// let options = OutputOptions::verbose();
/// let (text, errors) = format_with_options(&plan, &options);
/// println!("{}", text);
///
/// if !errors.is_empty() {
///     println!("Formatting errors:");
///     for error in errors {
///         println!("  {}", error);
///     }
/// }
/// ```
pub fn format_with_options(plan: &Plan, options: &OutputOptions) -> (String, Vec<TextifyError>) {
    let (writer, error_queue) = PlanWriter::<ErrorQueue>::new(options, plan);
    let text = format!("{writer}");

    // Collect errors from the error queue
    let errors: Vec<TextifyError> = error_queue.into_iter().collect();

    (text, errors)
}
