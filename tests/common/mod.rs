//! Common test utilities for roundtrip testing

use substrait_explain::format;
use substrait_explain::parser::Parser;
use substrait_explain::textify::plan::PlanWriter;
use substrait_explain::textify::{ErrorQueue, OutputOptions};

/// Roundtrip a plan and verify that the output is the same as the input, after
/// being parsed to a Substrait plan and then back to text.
pub fn roundtrip_plan(input: &str) {
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
pub fn roundtrip_plan_with_verbose(input: &str, verbose_input: &str) {
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
