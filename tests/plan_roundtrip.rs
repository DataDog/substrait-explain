//! Test that we can roundtrip a plan from text to a Substrait plan and back to
//! text. Many features can be tested this way - in general, if it can survive a
//! round-trip, it's probably correct; it could be stored incorrectly and both
//! parser and formatter handle it the same incorrect way, but that is probably
//! rare, and more likely a reflection of the author's misunderstanding of the
//! Substrait plan format.

use substrait_explain::format;
use substrait_explain::parser::Parser;
use substrait_explain::textify::plan::PlanWriter;
use substrait_explain::textify::{ErrorQueue, OutputOptions};

/// Roundtrip a plan and verify that the output is the same as the input, after
/// being parsed to a Substrait plan and then back to text.
fn roundtrip_plan(input: &str) {
    // Parse the plan using the simplified interface
    let plan = match Parser::parse(input) {
        Ok(plan) => plan,
        Err(e) => {
            println!("Error parsing plan:\n{e}");
            panic!("{}", e);
        }
    };

    // Format the plan back to text using the simplified interface
    let (actual, errors) = format(&plan);

    // Check for formatting errors
    if !errors.is_empty() {
        println!("Formatting errors:");
        for error in errors {
            println!("  {error}");
        }
        panic!("Formatting errors occurred");
    }

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
    let simple_plan = match Parser::parse(input) {
        Ok(plan) => plan,
        Err(e) => {
            println!("Error parsing simple plan:\n{e}");
            panic!("{}", e);
        }
    };

    // Parse the verbose plan
    let verbose_plan = match Parser::parse(verbose_input) {
        Ok(plan) => plan,
        Err(e) => {
            println!("Error parsing verbose plan:\n{e}");
            panic!("{}", e);
        }
    };

    // Test verbose output from both plans
    let verbose_options = OutputOptions::verbose();
    let (simple_verbose_writer, simple_verbose_errors) =
        PlanWriter::<ErrorQueue>::new(&verbose_options, &simple_plan);
    let simple_verbose_actual = format!("{simple_verbose_writer}");
    simple_verbose_errors
        .errs()
        .expect("Errors during simple -> verbose conversion");

    let (verbose_verbose_writer, verbose_verbose_errors) =
        PlanWriter::<ErrorQueue>::new(&verbose_options, &verbose_plan);
    let verbose_verbose_actual = format!("{verbose_verbose_writer}");
    verbose_verbose_errors
        .errs()
        .expect("Errors during verbose -> verbose conversion");

    assert_eq!(
        simple_verbose_actual.trim(),
        verbose_verbose_actual.trim(),
        "Expected verbose outputs to match:\n---\n{}\n---\n---\n{}\n---",
        simple_verbose_actual.trim(),
        verbose_verbose_actual.trim()
    );

    // Test simple output from both plans
    let simple_options = OutputOptions::default();
    let (simple_simple_writer, simple_simple_errors) =
        PlanWriter::<ErrorQueue>::new(&simple_options, &simple_plan);
    let simple_simple_actual = format!("{simple_simple_writer}");
    simple_simple_errors
        .errs()
        .expect("Errors during simple -> simple conversion");

    let (verbose_simple_writer, verbose_simple_errors) =
        PlanWriter::<ErrorQueue>::new(&simple_options, &verbose_plan);
    let verbose_simple_actual = format!("{verbose_simple_writer}");
    verbose_simple_errors
        .errs()
        .expect("Errors during verbose -> simple conversion");

    assert_eq!(
        simple_simple_actual.trim(),
        verbose_simple_actual.trim(),
        "Expected simple outputs to match:\n---\n{}\n---\n---\n{}\n---",
        simple_simple_actual.trim(),
        verbose_simple_actual.trim()
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

#[test]
fn test_aggregate_relation_roundtrip() {
    let plan = r#"=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml
Functions:
  # 10 @  1: sum
  # 11 @  1: count

=== Plan
Root[category, total, count]
  Aggregate[$0 => sum($1), count($1)]
    Read[orders => category:string?, amount:fp64?]"#;

    roundtrip_plan(plan);
}

#[test]
fn test_aggregate_relation() {
    let plan = r#"=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml
Functions:
  # 10 @  1: sum
  # 11 @  1: count

=== Plan
Root[category, region, total, count]
  Aggregate[$0, $1 => sum($2), $0, count($2)]
    Read[orders => category:string?, region:string?, amount:fp64?]"#;

    roundtrip_plan(plan);
}
