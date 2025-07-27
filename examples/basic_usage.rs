//! Basic usage example for substrait-explain
//!
//! This example shows how to parse a Substrait plan and format it with different output options.

use substrait_explain::parser::Parser;
use substrait_explain::{OutputOptions, format, format_with_options};

fn main() {
    println!("=== Substrait-Explain Basic Usage ===\n");

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

    match Parser::parse(plan_text) {
        Ok(result) => {
            // Show parse warnings if any
            if !result.warnings.is_empty() {
                println!("Parse warnings:");
                for warning in &result.warnings {
                    println!("  - {warning}");
                }
                println!();
            }

            // Output with standard options
            let (text, errors) = format(&result.plan);
            println!("== Standard output ==");
            println!("{text}");

            if !errors.is_empty() {
                println!("Warnings during conversion:");
                for error in errors {
                    println!("  - {error}");
                }
            }

            // Output with verbose options
            let (verbose_text, verbose_errors) =
                format_with_options(&result.plan, &OutputOptions::verbose());
            println!("\n== Verbose output ==");
            println!("{verbose_text}");

            if !verbose_errors.is_empty() {
                println!("Warnings during verbose conversion:");
                for error in verbose_errors {
                    println!("  - {error}");
                }
            }
        }
        Err(e) => println!("Error parsing plan: {e}"),
    }
}
