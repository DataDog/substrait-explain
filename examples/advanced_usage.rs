//! Advanced usage example for substrait-explain
//!
//! This example shows how to parse a Substrait plan and format it with
//! different output options.

use substrait::proto::Plan;
use substrait_explain::{OutputOptions, Parser, Visibility, format_with_options};

/// Helper function to format a plan with given options and print the result with error handling
fn print_with_errors(
    plan: &Plan,
    options: Option<&OutputOptions>,
) -> Result<(), Box<dyn std::error::Error>> {
    let default_options;
    let options = match options {
        Some(options) => options,
        None => {
            default_options = OutputOptions::default();
            &default_options
        }
    };
    let (formatted, errors) = format_with_options(plan, options);

    println!("{formatted}");

    if errors.is_empty() {
        Ok(())
    } else {
        Err(format!("formatting produced errors: {errors:?}").into())
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Parse a plan with extensions
    let plan_text = r#"
=== Extensions
URNs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
Functions:
  # 10 @  1: gt
  # 11 @  1: multiply
=== Plan
Root[revenue]
  Filter[gt($2, 100) => $0, $1]
    Project[$0, $1, multiply($0, $1)]
      Read[orders => quantity:i32?, price:fp64?]
"#;

    let plan = Parser::parse(plan_text)?;
    // Show the plan in YAML format
    println!("Plan Structure (YAML):");
    match serde_yaml::to_string(&plan) {
        Ok(yaml) => println!("{yaml}"),
        Err(e) => println!("Error formatting YAML: {e}"),
    }
    println!();

    // Standard output (concise)
    println!("Standard Output:");
    print_with_errors(&plan, None)?;

    // Verbose output (shows all details)
    println!("Verbose Output:");
    print_with_errors(&plan, Some(&OutputOptions::verbose()))?;

    // Custom output options
    let custom_options = OutputOptions {
        literal_types: Visibility::Always, // Show types for all literals
        indent: "    ".to_string(),        // 4 spaces instead of 2
        ..OutputOptions::default()
    };

    println!("Custom Output (4-space indent, always show types):");
    print_with_errors(&plan, Some(&custom_options))?;

    Ok(())
}
