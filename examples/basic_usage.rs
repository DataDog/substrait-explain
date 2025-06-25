//! Basic usage example for substrait-explain
//!
//! This example shows how to parse a Substrait plan and format it with different output options.

use substrait_explain::parser::Parser;
use substrait_explain::textify::plan::PlanWriter;
use substrait_explain::textify::{ErrorQueue, OutputOptions};

/// Helper function to create a writer and print output with error checking
fn print_plan(plan: &substrait::proto::Plan, options: &OutputOptions, title: &str) {
    println!("{}:", title);
    let (writer, errors) = PlanWriter::<ErrorQueue>::new(options, plan);
    println!("{}", writer);

    // Check for errors
    let errors: Vec<_> = errors.into();
    if !errors.is_empty() {
        println!("Warnings during conversion:");
        for error in errors {
            println!("  - {}", error);
        }
    }
}

fn main() {
    println!("=== Substrait-Explain Basic Usage ===\n");

    // Parse a plan with extensions
    let plan_text = r#"
=== Extensions
URIs:
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

    match Parser::parse_plan(plan_text) {
        Ok(plan) => {
            // Output with standard options
            print_plan(&plan, &OutputOptions::default(), "Standard output");

            // Output with verbose options
            print_plan(&plan, &OutputOptions::verbose(), "Verbose output");
        }
        Err(e) => println!("Error parsing plan: {}", e),
    }
}
