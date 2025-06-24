//! Basic usage example for substrait-explain
//!
//! This example shows how to parse a Substrait plan and format it with different output options.

use substrait_explain::parser::Parser;
use substrait_explain::textify::plan::PlanWriter;
use substrait_explain::textify::{ErrorQueue, OutputOptions};

fn main() {
    println!("=== Substrait-Explain Basic Usage ===\n");

    // Parse a plan with extensions
    let plan_text = r#"
=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
Functions:
  # 10 @  1: add
  # 11 @  1: multiply
=== Plan
Root[result]
  Filter[*] condition=gt:fp64_fp64($2, 100)
    Project[$0, $1, multiply:fp64($0, $1)]
      Read[quantity::i32?, price::fp64?] table=orders
"#;

    match Parser::parse_plan(plan_text) {
        Ok(plan) => {
            // Output with standard options
            println!("Standard output:");
            let options = OutputOptions::default();
            let writer = PlanWriter::<ErrorQueue>::new(&options, &plan);
            println!("{}", writer);

            // Output with verbose options
            println!("\nVerbose output:");
            let verbose_options = OutputOptions::verbose();
            let verbose_writer = PlanWriter::<ErrorQueue>::new(&verbose_options, &plan);
            println!("{}", verbose_writer);
        }
        Err(e) => println!("Error parsing plan: {}", e),
    }
}
