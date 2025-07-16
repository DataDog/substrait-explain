#![doc = include_str!("../API.md")]

pub mod extensions;
pub mod fixtures;
pub mod grammar;
pub mod parser;
pub mod textify;

#[cfg(feature = "cli")]
pub mod cli;

// Re-export commonly used types for easier access
pub use parser::ParseError;
use substrait::proto::Plan;
use textify::foundation::ErrorQueue;
pub use textify::foundation::{FormatError, OutputOptions, Visibility};
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
/// Returns a `ParseError` if the input cannot be parsed as a valid Substrait plan.
/// The error includes details about what went wrong and where in the input.
///
/// ```rust
/// use substrait_explain::parse;
///
/// let invalid_plan = r#"
/// === Plan
/// InvalidRelation[invalid syntax]
/// "#;
///
/// match parse(invalid_plan) {
///     Ok(_) => println!("Valid plan"),
///     Err(e) => println!("Parse error: {}", e),
/// }
/// ```
pub fn parse(input: &str) -> Result<Plan, ParseError> {
    parser::structural::Parser::parse(input)
}

/// Format a Substrait plan as human-readable text.
///
/// This is the main entry point for formatting plans. It uses default
/// formatting options that produce concise, readable output.
///
/// Returns a tuple of `(formatted_text, errors)`. The text is always generated,
/// even if there are formatting errors. Errors are collected and returned for
/// inspection.
///
/// # Example
/// ```rust
/// use substrait_explain::{parse, format};
/// use substrait::proto::Plan;
///
/// let plan: Plan = parse(r#"
/// === Plan
/// Root[result]
///   Project[$0, $1]
///     Read[data => a:i64, b:string]
/// "#).unwrap();
///
/// let (text, errors) = format(&plan);
/// println!("{}", text);
///
/// if !errors.is_empty() {
///     println!("Formatting warnings: {:?}", errors);
/// }
/// ```
///
/// # Output Format
///
/// The output follows the Substrait text format specification, with relations
/// displayed in a hierarchical structure using indentation.
pub fn format(plan: &Plan) -> (String, Vec<FormatError>) {
    let options = OutputOptions::default();
    format_with_options(plan, &options)
}

/// Format a Substrait plan with custom options.
///
/// This function allows you to customize the formatting behavior, such as
/// showing more or less detail, changing indentation, or controlling
/// type visibility.
///
/// # Example
/// ```rust
/// use substrait_explain::{parse, format_with_options, OutputOptions, Visibility};
///
/// let plan = parse(r#"
/// === Plan
/// Root[result]
///   Project[$0, 42]
///     Read[data => a:i64]
/// "#).unwrap();
///
/// // Use verbose formatting
/// let verbose_options = OutputOptions::verbose();
/// let (text, _errors) = format_with_options(&plan, &verbose_options);
/// println!("Verbose output:\n{}", text);
///
/// // Custom options
/// let custom_options = OutputOptions {
///     literal_types: Visibility::Always,
///     indent: "    ".to_string(),
///     ..OutputOptions::default()
/// };
/// let (text, _errors) = format_with_options(&plan, &custom_options);
/// println!("Custom output:\n{}", text);
/// ```
///
/// # Options
///
/// See [`OutputOptions`] for all available configuration options.
pub fn format_with_options(plan: &Plan, options: &OutputOptions) -> (String, Vec<FormatError>) {
    let (writer, error_queue) = PlanWriter::<ErrorQueue>::new(options, plan);
    let output = format!("{writer}");
    let errors = error_queue.into_iter().collect();
    (output, errors)
}
