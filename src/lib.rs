//! # Substrait-Explain
//!
//! **Transform complex Substrait protobuf plans into readable, SQL EXPLAIN-like text**
//!
//! ## What does it do?
//!
//! Ever tried to debug a Substrait query plan? Outputting the raw protobuf as text ends up quite difficult to read:
//!
//! ```yaml
//! extensionUris:
//!   - extensionUriAnchor: 1
//!     uri: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
//! extensions:
//!   - extensionFunction:
//!       extensionUriReference: 1
//!       functionAnchor: 10
//!       name: gt
//!   - extensionFunction:
//!       extensionUriReference: 1
//!       functionAnchor: 11
//!       name: multiply
//! relations:
//!   - root:
//!       input:
//!         filter:
//!           common:
//!             emit:
//!               outputMapping:
//!                 - 0
//!                 - 1
//!           input:
//!             project:
//!               common:
//!                 emit:
//!                   outputMapping:
//!                     - 0
//!                     - 1
//!                     - 2
//!               input:
//!                 read:
//!                   baseSchema:
//!                     names:
//!                       - quantity
//!                       - price
//!                     struct:
//!                       types:
//!                         - i32:
//!                             nullability: NULLABILITY_NULLABLE
//!                         - fp64:
//!                             nullability: NULLABILITY_NULLABLE
//!                       nullability: NULLABILITY_REQUIRED
//!                   namedTable:
//!                     names:
//!                       - orders
//!               expressions:
//!                 - scalarFunction:
//!                     functionReference: 11
//!                     arguments:
//!                       - value:
//!                           selection:
//!                             directReference:
//!                               structField: {}
//!                       - value:
//!                           selection:
//!                             directReference:
//!                               structField:
//!                                 field: 1
//!           condition:
//!             scalarFunction:
//!               functionReference: 10
//!               arguments:
//!                 - value:
//!                     selection:
//!                       directReference:
//!                         structField:
//!                           field: 2
//!                 - value:
//!                     literal:
//!                       i64: "100"
//!       names:
//!         - revenue
//! ```
//!
//! **Substrait-Explain converts this into:**
//!
//! ```text
//! === Extensions
//! URIs:
//!   @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
//! Functions:
//!   # 10 @  1: gt
//!   # 11 @  1: multiply
//!
//! === Plan
//! Root[revenue]
//!   Filter[gt($2, 100) => $0, $1]
//!     Project[$0, $1, multiply($0, $1)]
//!       Read[orders => quantity:i32?, price:fp64?]
//! ```
//!
//! Suddenly you can see exactly what the query does: filter orders where some calculated value is greater than 100, then project the quantity, price, and their product.
//!
//! ## Key Features
//!
//! - **Human-readable output**: Convert complex Substrait plans into simple, readable text
//! - **Bidirectional conversion**: Parse text format back into Substrait plans
//! - **Extension support**: Full support for Substrait extensions and custom functions
//! - **Error handling**: Graceful error handling that doesn't prevent output generation
//! - **Flexible formatting**: Configurable output options for different use cases
//!
//! ## Installation
//!
//! Add this to your `Cargo.toml`:
//!
//! ```toml
//! [dependencies]
//! substrait-explain = "0.1.0"
//! substrait = "0.57.0"  # For protobuf types
//! ```
//!
//! ## Quick Start
//!
//! ### Basic Usage
//!
//! ```rust
//! use substrait_explain::{parse, format};
//!
//! // Parse a Substrait plan from text format
//! let plan_text = r#"
//! === Extensions
//! URIs:
//!   @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
//! Functions:
//!   ## 10 @  1: add
//! === Plan
//! Project[$0, $1, add($0, $1)]
//!   Read[table1 => col1:i32?, col2:i32?]
//! "#;
//!
//! let plan = parse(plan_text).unwrap();
//!
//! // Convert to human-readable format
//! let (output, errors) = format(&plan);
//! println!("{}", output);
//!
//! // Check for any errors during conversion
//! if !errors.is_empty() {
//!     println!("Warnings during conversion: {:?}", errors);
//! }
//! ```
//!
//! ### Working with Substrait Protobuf Plans
//!
//! ```rust
//! use substrait_explain::{parse, format};
//! use substrait::proto::Plan;
//!
//! // If you have a Substrait protobuf plan
//! let plan: Plan = Plan::default(); // or your own plan
//! # let plan = parse("=== Plan\nRead[schema.table => a:i64, b:string?]").unwrap();
//!
//! // Convert it to readable text
//! let (output, errors) = format(&plan);
//!
//! // Check for any errors during conversion
//! if !errors.is_empty() {
//!     println!("Warnings during conversion: {:?}", errors);
//! }
//! ```
//!
//! ### Parsing Individual Components
//!
//! ```rust
//! use substrait_explain::parser::{Parse, ScopedParse};
//! use substrait_explain::extensions::SimpleExtensions;
//! use substrait_explain::extensions::simple::ExtensionKind;
//! use substrait::proto::{Type, Expression};
//!
//! // Parse and format types
//! let extensions = SimpleExtensions::new();
//! let typ = Type::parse(&extensions, "i32?").unwrap();
//!
//! // Register the 'add' function for the context
//! let mut extensions = SimpleExtensions::new();
//! extensions.add_extension_uri("https://example.com/functions".to_string(), 1).unwrap();
//! extensions.add_extension(ExtensionKind::Function, 1, 10, "add".to_string()).unwrap();
//!
//! // Parse and format expressions
//! let expr = Expression::parse(&extensions, "add($0, $1)").unwrap();
//! ```
//!
//! ### Working with Extensions
//!
//! ```rust
//! use substrait_explain::extensions::SimpleExtensions;
//! use substrait_explain::extensions::simple::ExtensionKind;
//! use substrait_explain::parser::ScopedParse;
//! use substrait::proto::Expression;
//!
//! // Create extensions context
//! let mut extensions = SimpleExtensions::new();
//! extensions.add_extension_uri("https://example.com/functions".to_string(), 1).unwrap();
//! extensions.add_extension(ExtensionKind::Function, 1, 10, "my_function".to_string()).unwrap();
//!
//! // Use extensions in parsing
//! let expr = Expression::parse(&extensions, "my_function#10($0, $1)").unwrap();
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
//! Project[$0, $1, add($0, $1)]
//!   Read[table1 => col1:i32?, col2:i32?]
//! ```
//!
//! ### Relation Format
//!
//! Each relation is displayed on a single line with the format:
//! `RelationName[arguments => columns]`
//!
//! - **arguments**: Input expressions, field references, or function calls
//! - **columns**: Output column names and types
//! - **indentation**: Shows the relationship hierarchy
//!
//! ### Expression Format
//!
//! - **Field references**: `$0`, `$1`, etc.
//! - **Literals**: `42`, `"hello"`, `true`
//! - **Function calls**: `add($0, $1)`, `sum($2)`
//! - **Types**: `i32`, `string?`, `list<i64>`
//!
//! ## Configuration Options
//!
//! ```rust
//! use substrait_explain::{OutputOptions, Visibility};
//!
//! // Default options - concise output
//! let options = OutputOptions::default();
//!
//! // Verbose options - show all details
//! let verbose_options = OutputOptions::verbose();
//!
//! // Custom options
//! let custom_options = OutputOptions {
//!     show_extension_uris: true,
//!     show_simple_extensions: true,
//!     show_simple_extension_anchors: Visibility::Always,
//!     literal_types: Visibility::Required,
//!     show_emit: false,
//!     read_types: true,
//!     fn_types: true,
//!     nullability: true,
//!     indent: "  ".to_string(),
//!     show_literal_binaries: false,
//! };
//! ```
//!
//! ## Examples
//!
//! ### Basic Usage
//!
//! For a simple introduction, see the `basic_usage` example:
//!
//! ```bash
//! cargo run --example basic_usage
//! ```
//!
//! ### Advanced Usage
//!
//! For a comprehensive demonstration including different output options and protobuf comparison, see the `advanced_usage` example:
//!
//! ```bash
//! cargo run --example advanced_usage --features serde
//! ```
//!
//! This example shows:
//!
//! - Standard vs verbose output formatting
//! - Custom output options (different indentation, type visibility)
//! - Comparison with the original YAML representation
//! - Explanation of what the query transformation accomplishes
//!
//! ### Simple Query Plan
//!
//! ```rust
//! use substrait_explain::{parse, format};
//!
//! let plan_text = r#"
//! === Extensions
//! URIs:
//!   @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
//! Functions:
//!   ## 10 @  1: add
//! === Plan
//! Project[$0, $1, add($0, $1)]
//!   Read[table1 => col1:i32?, col2:i32?]
//! "#;
//!
//! let plan = parse(plan_text).unwrap();
//! let (output, _errors) = format(&plan);
//! println!("{}", output);
//! ```
//!
//! ### Complex Query with Extensions
//!
//! ```rust
//! use substrait_explain::{parse, format};
//!
//! let plan_text = r#"
//! === Extensions
//! URIs:
//!   @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
//! Functions:
//!   ## 10 @  1: multiply
//!   ## 11 @  1: gt
//! === Plan
//! Filter[gt($2, 100) => $0, $1]
//!   Project[$0, $1, multiply($0, $1)]
//!     Read[orders => quantity:i32?, price:fp64?]
//! "#;
//!
//! let plan = parse(plan_text).unwrap();
//! let (output, _errors) = format(&plan);
//! println!("{}", output);
//! ```
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

pub mod docs;
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
pub fn format(plan: &Plan) -> (String, Vec<TextifyError>) {
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
pub fn format_with_options(plan: &Plan, options: &OutputOptions) -> (String, Vec<TextifyError>) {
    let (writer, error_queue) = PlanWriter::<ErrorQueue>::new(options, plan);
    let output = format!("{writer}");
    let errors = error_queue.into_iter().collect();
    (output, errors)
}
