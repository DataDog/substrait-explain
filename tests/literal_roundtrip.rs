//! Test roundtrip functionality for literal parsing and formatting

use substrait_explain::format;
use substrait_explain::parser::Parser;

/// Roundtrip a plan with literals and verify that the output matches the input
fn roundtrip_plan(input: &str) {
    // Parse the plan
    let plan = match Parser::parse(input) {
        Ok(plan) => plan,
        Err(e) => {
            println!("Error parsing plan:\n{e}");
            panic!("{}", e);
        }
    };

    // Format the plan back to text
    let (actual, errors) = format(&plan);

    // Check for formatting errors
    if !errors.is_empty() {
        println!("Formatting errors:");
        for error in errors {
            println!("  {error}");
        }
        panic!("Formatting errors occurred");
    }

    // Compare the output with the input
    assert_eq!(
        actual.trim(),
        input.trim(),
        "Expected:\n---\n{}\n---\nActual:\n---\n{}\n---",
        input.trim(),
        actual.trim()
    );
}

#[test]
fn test_float_literal_roundtrip() {
    let plan = r#"
=== Plan
Root[result]
  Project[3.14, -2.5, 1]
    Read[data => a:i64]
"#;
    roundtrip_plan(plan);
}

#[test]
fn test_boolean_literal_roundtrip() {
    let plan = r#"
=== Plan
Root[result]
  Project[true, false]
    Read[data => a:i64]
"#;
    roundtrip_plan(plan);
}

#[test]
fn test_mixed_literal_roundtrip() {
    let plan = r#"
=== Plan
Root[result]
  Project[42, 3.14, true, 'hello']
    Read[data => a:i64]
"#;
    roundtrip_plan(plan);
}

#[test]
fn test_negative_literals_roundtrip() {
    let plan = r#"
=== Plan
Root[result]
  Project[-42, -3.14, false]
    Read[data => a:i64]
"#;
    roundtrip_plan(plan);
}
