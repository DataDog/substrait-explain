//! Simple interface example for substrait-explain
//!
//! This example demonstrates the new simplified interface for parsing and formatting
//! Substrait plans.

use substrait_explain::{OutputOptions, Parser, format, format_with_options};

fn main() {
    println!("=== Substrait-Explain Simple Interface Example ===\n");

    // Example 1: Parse a well-formed plan
    println!("1. Parsing a well-formed plan:");
    let plan_text = r#"
=== Plan
Root[c, d]
  Project[$1, 42]
    Read[schema.table => a:i64, b:string?]
"#;

    match Parser::parse(plan_text) {
        Ok(plan) => {
            println!("✅ Successfully parsed plan");

            // Format it back to text
            let (formatted, errors) = format(&plan);
            println!("Formatted plan:\n{}", formatted);

            if !errors.is_empty() {
                println!("⚠️  Formatting errors:");
                for error in errors {
                    println!("  {}", error);
                }
            } else {
                println!("✅ No formatting errors");
            }
        }
        Err(e) => {
            println!("❌ Parse error: {}", e);
        }
    }

    println!("\n2. Parsing a plan with extensions:");
    let plan_with_extensions = r#"
=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
Functions:
  # 10 @  1: add
  # 11 @  1: multiply
=== Plan
Root[result]
  Project[$0, $1, add($0, $1)]
    Read[orders => quantity:i32?, price:i64]
"#;

    match Parser::parse(plan_with_extensions) {
        Ok(plan) => {
            println!("✅ Successfully parsed plan with extensions");

            // Format with error collection
            let (formatted, errors) = format(&plan);
            println!("Formatted plan:\n{}", formatted);

            if !errors.is_empty() {
                println!("⚠️  Formatting errors:");
                for error in errors {
                    println!("  {}", error);
                }
            } else {
                println!("✅ No formatting errors");
            }
        }
        Err(e) => {
            println!("❌ Parse error: {}", e);
        }
    }

    println!("\n3. Using custom formatting options:");
    let simple_plan = r#"
=== Plan
Root[name, num]
  Project[$1, 42]
    Read[schema.table => name:string?, num:i64]
"#;

    match Parser::parse(simple_plan) {
        Ok(plan) => {
            println!("✅ Successfully parsed simple plan");

            // Format with verbose options
            let options = OutputOptions::verbose();
            let (formatted, errors) = format_with_options(&plan, &options);
            println!("Verbose formatted plan:\n{}", formatted);

            if !errors.is_empty() {
                println!("⚠️  Formatting errors:");
                for error in errors {
                    println!("  {}", error);
                }
            } else {
                println!("✅ No formatting errors");
            }
        }
        Err(e) => {
            println!("❌ Parse error: {}", e);
        }
    }

    println!("\n4. Handling parse errors:");
    let invalid_plan = r#"
=== Plan
InvalidRelation[invalid syntax]
"#;

    match Parser::parse(invalid_plan) {
        Ok(plan) => {
            println!("✅ Unexpectedly parsed invalid plan");
            let (formatted, _errors) = format(&plan);
            println!("Formatted:\n{}", formatted);
        }
        Err(e) => {
            println!("❌ Expected parse error: {}", e);
        }
    }

    println!("\n=== Example Complete ===");
}
