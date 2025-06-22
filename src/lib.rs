//! A Rust library for displaying Substrait query plans in a human-readable,
//! EXPLAIN-like format.
//!
//! This crate converts Substrait protobuf plans into a concise, readable text
//! format that's similar to SQL EXPLAIN output. It provides both parsing and
//! formatting capabilities, making it easy to work with Substrait plans in a
//! human-friendly way.
//!
//! ## Quick Start
//!
//! ### Basic Usage
//!
//! ```rust
//! use substrait_explain::{parse, format};
//! use substrait::proto;
//!
//! // Parse a Substrait plan from text format
//! let plan_text = r#"
//! === Plan
//! Root[c, d]
//!   Project[$1, 42]
//!     Read[schema.table => a:i64, b:string?]
//! "#;
//!
//! // Parse the plan. Builds a complete Substrait plan.
//! let plan: substrait::proto::Plan = parse(plan_text).unwrap();
//!
//! // Convert back to text format
//! let (text, errors) = format(&plan);
//! println!("{}", text);
//!
//! // Check for any formatting errors
//! if !errors.is_empty() {
//!     eprintln!("Formatting warnings:");
//!     for error in errors {
//!         eprintln!("  {}", error);
//!     }
//! }
//! ```
//!
//! ### Working with Simple Extensions
//!
//! Substrait uses [Simple
//! Extensions](https://substrait.io/extensions/#simple-extensions) to define
//! the basic types and functions used.
//!
//! This example shows how to parse a plan that uses simple extensions.
//!
//! ```rust
//! use substrait_explain::{parse, format};
//!
//! // This plan reads quantity and price from the orders table, and multiplies
//! // them to get revenue.
//!
//! let plan_text = r#"
//! === Extensions
//!
//! URIs:
//!   @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
//!
//! Functions:
//!   ## 10 @  1: multiply
//!
//! === Plan
//!
//! Root[revenue]
//!   Project[$0, $1, multiply($0, $1)]
//!     Read[orders => quantity:i32?, price:i64]
//! "#;
//!
//! let plan = parse(plan_text).unwrap();
//! let (text, _errors) = format(&plan);
//! println!("{}", text);
//! ```
//!
//! ### Custom Formatting Options
//!
//! ```rust
//! use substrait_explain::{parse, format_with_options, OutputOptions, Visibility};
//!
//! let plan_text = r#"
//! === Plan
//! Root[name, num]
//!   Project[$1, 42, 54:i16]
//!     Read[schema.table => name:string?, num:i64]
//! "#;
//!
//! let plan = parse(plan_text).unwrap();
//!
//! // Use verbose formatting
//! let options = OutputOptions::verbose();
//! let (text, _errors) = format_with_options(&plan, &options);
//! println!("Verbose output:\n{}", text);
//!
//! // Or customize specific options
//! let custom_options = OutputOptions {
//!     literal_types: Visibility::Required,  // Show literal types when needed
//!     ..OutputOptions::default()
//! };
//! let (text, _errors) = format_with_options(&plan, &custom_options);
//! println!("Custom output:\n{}", text);
//! ```
//!
//! ### Error Handling
//!
//! ```rust
//! use substrait_explain::{parse, format};
//!
//! let invalid_plan = r#"
//! === Plan
//! InvalidRelation[invalid syntax]
//! "#;
//!
//! match parse(invalid_plan) {
//!     Ok(plan) => {
//!         let (text, errors) = format(&plan);
//!         println!("Formatted plan:\n{}", text);
//!
//!         if !errors.is_empty() {
//!             println!("Formatting errors: {:?}", errors);
//!         }
//!     }
//!     Err(e) => println!("Parse error: {}", e),
//! }
//! ```
//!
//! ## Output Format
//!
//! The library produces a structured text format that's easy to read and parse:
//!
//! ### Basic Plan Structure
//!
//! ```text
//! === Extensions
//! URIs:
//!   @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
//! Functions:
//!   # 10 @  1: add
//! === Plan
//! Root[c, d]
//!   Project[$1, 42]
//!     Read[schema.table => a:i64, b:string?]
//! ```
//!
//! Each relation is displayed on a single line with indentation showing the
//! hierarchy. Field references use `$0`, `$1`, etc., and function calls use
//! `function_name(arg1, arg2)`.
//!
//! ## Goals
//!
//! 1. **Easy to understand**: Use a simple, human-readable format that's
//!    concise and clear
//! 2. **Easy to parse**: Consistent output format that's reasonably parseable
//! 3. **Easy to extend**: Extensions should be as easy to use as with Substrait
//!    itself
//! 4. **Semantically complete**: Output should map to semantically identical
//!    Substrait plans
//! 5. **Graceful errors**: Errors don't prevent output generation but are
//!    returned for inspection

pub mod extensions;
pub mod fixtures;
pub mod parser;
pub mod textify;

// Re-export commonly used types for easier access
pub use parser::ParseError;
use substrait::proto::Plan;
use textify::foundation::ErrorQueue;
pub use textify::foundation::{FormatError as TextifyError, OutputOptions, Visibility};
use textify::plan::PlanWriter;

/// Parse a Substrait plan from text format.
///
/// This is the main entry point for parsing well-formed plans.
/// Returns a clear error if parsing fails.
///
/// The input should be in the Substrait text format, which consists of:
/// - An optional extensions section starting with "=== Extensions"
/// - A plan section starting with "=== Plan"
/// - Indented relation definitions
///
/// # Example
/// ```rust
/// use substrait_explain::parse;
///
/// let plan_text = r#"
/// === Plan
/// Root[c, d]
///   Project[$1, 42]
///     Read[schema.table => a:i64, b:string?]
/// "#;
///
/// // Parse the plan. Builds a complete Substrait plan.
/// let plan = parse(plan_text).unwrap();
/// ```
///
/// # Errors
///
/// Returns a [`ParseError`] if the input cannot be parsed. The error includes
/// the line number and content where parsing failed, along with a description
/// of what went wrong.
pub fn parse(input: &str) -> Result<Plan, ParseError> {
    parser::Parser::parse(input)
}

/// Format a Substrait plan to text format.
///
/// This is the main entry point for formatting plans to text.
/// The plan may not be entirely well-formed - errors will be marked in the
/// output with `!{error_type}` placeholders, but the function will still
/// produce output for everything that can be formatted.
///
/// Returns both the formatted text and any errors that occurred during
/// formatting. Users can abort, log, or ignore the errors depending on their
/// needs.
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
/// Returns both the formatted text and any errors that occurred during
/// formatting.
///
/// # Example
/// ```rust
/// use substrait_explain::{parse, format_with_options, OutputOptions};
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
