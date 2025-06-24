//! Test that we can roundtrip a plan from text to a Substrait plan and back to
//! text. Many features can be tested this way - in general, if it can survive a
//! round-trip, it's probably correct; it could be stored incorrectly and both
//! parser and formatter handle it the same incorrect way, but that is probably
//! rare, and more likely a reflection of the author's misunderstanding of the
//! Substrait plan format.

use substrait_explain::parser::Parser;
use substrait_explain::textify::plan::PlanWriter;
use substrait_explain::textify::{ErrorQueue, OutputOptions};

/// Roundtrip a plan and verify that the output is the same as the input, after
/// being parsed to a Substrait plan and then back to text.
fn roundtrip_plan(input: &str) {
    let errors = ErrorQueue::default();

    // Parse the plan
    let plan = match Parser::parse_plan(input) {
        Ok(plan) => plan,
        Err(e) => {
            println!("Error parsing plan:\n{}", e);
            panic!("{}", e);
        }
    };
    if let Err(e) = errors.errs() {
        println!("Error parsing plan:\n{}", e);
        panic!("{}", e);
    }

    // We have a Substrait plan, let's convert back to text
    let options = OutputOptions::default();
    let writer = PlanWriter::<ErrorQueue>::new(&options, &plan);
    let actual = format!("{}", writer);

    // Compare the output with the input, printing the difference.
    assert_eq!(
        actual.trim(),
        input.trim(),
        "Expected:\n---\n{}\n---\nActual:\n---\n{}\n---",
        input.trim(),
        actual.trim()
    );
}

/// Roundtrip a plan and verify that the output is the same as the input, after
/// being parsed to a Substrait plan and then back to text.
fn roundtrip_plan_with_verbose(input: &str, verbose_input: &str) {
    // Parse the simple plan
    let simple_errors = ErrorQueue::default();
    let simple_plan = match Parser::parse_plan(input) {
        Ok(plan) => plan,
        Err(e) => {
            println!("Error parsing simple plan:\n{}", e);
            panic!("{}", e);
        }
    };
    simple_errors
        .errs()
        .expect("Failure while parsing simple plan");

    // Parse the verbose plan
    let verbose_errors = ErrorQueue::default();
    let verbose_plan = match Parser::parse_plan(verbose_input) {
        Ok(plan) => plan,
        Err(e) => {
            println!("Error parsing verbose plan:\n{}", e);
            panic!("{}", e);
        }
    };
    verbose_errors
        .errs()
        .expect("Failure while parsing verbose plan");

    // Test verbose output from both plans
    let verbose_options = OutputOptions::verbose();
    let simple_verbose_writer = PlanWriter::<ErrorQueue>::new(&verbose_options, &simple_plan);
    let simple_verbose_actual = format!("{}", simple_verbose_writer);

    let verbose_verbose_writer = PlanWriter::<ErrorQueue>::new(&verbose_options, &verbose_plan);
    let verbose_verbose_actual = format!("{}", verbose_verbose_writer);

    assert_eq!(
        simple_verbose_actual.trim(),
        verbose_verbose_actual.trim(),
        "Expected verbose outputs to match:\n---\n{}\n---\n---\n{}\n---",
        simple_verbose_actual.trim(),
        verbose_verbose_actual.trim()
    );

    // Test simple output from both plans
    let simple_options = OutputOptions::default();
    let simple_simple_writer = PlanWriter::<ErrorQueue>::new(&simple_options, &simple_plan);
    let simple_simple_actual = format!("{}", simple_simple_writer);

    let verbose_simple_writer = PlanWriter::<ErrorQueue>::new(&simple_options, &verbose_plan);
    let verbose_simple_actual = format!("{}", verbose_simple_writer);

    assert_eq!(
        simple_simple_actual.trim(),
        verbose_simple_actual.trim(),
        "Expected simple outputs to match:\n---\n{}\n---\n---\n{}\n---",
        simple_simple_actual.trim(),
        verbose_simple_actual.trim()
    );

    // Verify that the simple output matches the expected simple input
    assert_eq!(
        simple_simple_actual.trim(),
        input.trim(),
        "Expected simple output to match simple input:\n---\n{}\n---\n---\n{}\n---",
        input.trim(),
        simple_simple_actual.trim()
    );

    // Verify that the verbose output matches the expected verbose input
    assert_eq!(
        verbose_verbose_actual.trim(),
        verbose_input.trim(),
        "Expected verbose output to match verbose input:\n---\n{}\n---\n---\n{}\n---",
        verbose_input.trim(),
        verbose_verbose_actual.trim()
    );
}

#[test]
fn test_simple_plan_roundtrip() {
    let plan = r#"=== Plan
Root[c, d]
  Filter[$0 => $0, $1]
    Project[$1, 42]
      Read[my.table => a:i32, b:string?, c:boolean]"#;

    roundtrip_plan(plan);
}

#[test]
fn test_plan_with_extensions_roundtrip() {
    let plan = r#"=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
Functions:
  # 10 @  1: add
  # 11 @  1: multiply

=== Plan
Root[result]
  Project[$0, $1, add($0, $1)]
    Read[table1 => col1:i32?, col2:i32?]"#;

    roundtrip_plan(plan);
}

#[test]
fn test_complex_plan_roundtrip() {
    let plan = r#"=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
  @  2: https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml
Functions:
  # 10 @  1: add
  # 11 @  2: sum
  # 12 @  2: count

=== Plan
Root[name, parent, sum, count]
  Project[$0, $3, $1, $2]
    Read[schema.table => name:string?, parent:string?, sum:fp64?, count:fp64?]"#;

    roundtrip_plan(plan);
}

#[test]
fn test_plan_with_verbose_and_simple_output() {
    let simple_plan = r#"=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
Functions:
  # 10 @  1: coalesce

=== Plan
Root[name, num]
  Project[$1, coalesce($1, $2)]
    Read[schema.table => name:string?, num:fp64?, other_num:fp64?, id:i64]"#;

    let verbose_plan = r#"=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
Functions:
  # 10 @  1: coalesce

=== Plan
Root[name, num]
  Project[$1, coalesce#10($1, $2)]
    Read[schema.table => name:string?, num:fp64?, other_num:fp64?, id:i64]"#;

    roundtrip_plan_with_verbose(simple_plan, verbose_plan);
}

#[test]
fn test_multiple_relations_roundtrip() {
    let plan = r#"=== Plan
Root[name, num, id]
  Project[$0, $1, $2]
    Read[t1 => name:string?, num:fp64, id:i64]
Root[name, num, id]
  Filter[$0 => $0, $1, $2]
    Read[schema.table => name:string?, num:fp64?, id:i64]"#;

    roundtrip_plan(plan);
}

#[test]
fn test_plan_with_fetch_and_sort_roundtrip() {
    let plan = r#"=== Plan
Root[name, num, id]
  Project[$0, $1, $2]
    Read[schema.table => name:string?, num:fp64?, id:i64]"#;

    roundtrip_plan(plan);
}

#[test]
fn test_plan_with_extension_leaf_roundtrip() {
    let plan = r#"=== Plan
Root[name, some_value]
  Project[$0, $1]
    Read[schema.table => name:string?, some_value:fp64?]"#;

    roundtrip_plan(plan);
}
