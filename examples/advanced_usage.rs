//! Advanced usage example for substrait-explain
//!
//! This example shows how to parse a Substrait plan and format it with
//! different output options.

use serde_yaml;
use substrait::proto::Plan;
use substrait_explain::parser::Parser;
use substrait_explain::textify::plan::PlanWriter;
use substrait_explain::textify::{ErrorQueue, OutputOptions, Visibility};

/// Helper function to create a writer and print output with error checking
fn print_plan(plan: &Plan, options: &OutputOptions, title: &str) {
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
    println!();
}

fn main() {
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
            // Show the plan in YAML format
            println!("Plan Structure (YAML):");
            match serde_yaml::to_string(&plan) {
                Ok(yaml) => println!("{}", yaml),
                Err(e) => println!("Error formatting YAML: {}", e),
            }
            println!();

            // Standard output (concise)
            print_plan(&plan, &OutputOptions::default(), "Standard Output");

            // Verbose output (shows all details)
            print_plan(&plan, &OutputOptions::verbose(), "Verbose Output");

            // Custom output options
            let mut custom_options = OutputOptions::default();
            custom_options.show_extension_uris = true;
            custom_options.show_simple_extensions = true;
            custom_options.literal_types = Visibility::Always;
            custom_options.indent = "    ".to_string(); // 4 spaces instead of 2
            print_plan(
                &plan,
                &custom_options,
                "Custom Output (4-space indent, always show types)",
            );
        }
        Err(e) => println!("Error parsing plan: {}", e),
    }
}
