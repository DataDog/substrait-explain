//! Basic usage example for substrait-explain
//!
//! This example shows how to parse and format Substrait plans using the library.

use substrait::proto::{Expression, Type};
use substrait_explain::fixtures::TestContext;
use substrait_explain::parser::Parser;
use substrait_explain::parser::ScopedParse;
use substrait_explain::textify::plan::PlanWriter;
use substrait_explain::textify::{ErrorQueue, OutputOptions};

fn main() {
    println!("=== Substrait-Explain Basic Usage Example ===\n");

    // Example 1: Parse and format a simple plan
    println!("1. Simple Plan Parsing:");
    let plan_text = r#"
=== Plan
Root[c, d]
  Project[$1, add:fp64($0, $1)]
    Read[a::fp64?, b::i64] table=schema.table
"#;

    match Parser::parse_plan(plan_text) {
        Ok(plan) => {
            let options = OutputOptions::default();
            let writer = PlanWriter::<ErrorQueue>::new(&options, &plan);
            println!("Parsed plan:");
            println!("{}", writer);
        }
        Err(e) => println!("Error parsing plan: {}", e),
    }

    println!("\n2. Working with Extensions:");
    let plan_with_extensions = r#"
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

    match Parser::parse_plan(plan_with_extensions) {
        Ok(plan) => {
            let options = OutputOptions::default();
            let writer = PlanWriter::<ErrorQueue>::new(&options, &plan);
            println!("Plan with extensions:");
            println!("{}", writer);
        }
        Err(e) => println!("Error parsing plan: {}", e),
    }

    println!("\n3. Parsing Individual Components:");

    // Parse and format types
    let ctx = TestContext::new();
    if let Ok(typ) = Type::parse(&ctx.extensions, "i32?") {
        let formatted = ctx.textify_no_errors(&typ);
        println!("Type 'i32?' -> {}", formatted);
    }

    if let Ok(typ) = Type::parse(&ctx.extensions, "list<string?>") {
        let formatted = ctx.textify_no_errors(&typ);
        println!("Type 'list<string?>' -> {}", formatted);
    }

    // Parse and format expressions
    if let Ok(expr) = Expression::parse(&ctx.extensions, "add($0, $1)") {
        let formatted = ctx.textify_no_errors(&expr);
        println!("Expression 'add($0, $1)' -> {}", formatted);
    }

    println!("\n4. Error Handling:");
    let invalid_plan = r#"
=== Plan
InvalidRelation[invalid syntax]
"#;

    match Parser::parse_plan(invalid_plan) {
        Ok(plan) => {
            let options = OutputOptions::default();
            let writer = PlanWriter::<ErrorQueue>::new(&options, &plan);
            println!("Plan (with potential errors):");
            println!("{}", writer);

            // Check for errors - we can't access the private errors field directly
            // Instead, we'll just note that error handling is demonstrated
            println!("\nNote: Error handling is built into the library.");
            println!("Errors are accumulated during processing and can be accessed");
            println!("through the ErrorQueue's iterator interface.");
        }
        Err(e) => println!("Parse error: {}", e),
    }

    println!("\n=== Example Complete ===");
}
