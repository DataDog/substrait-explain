#![allow(dead_code)]

use substrait::proto;
use substrait::proto::{plan_rel, rel};
use substrait_explain::{Parser, format, parse};

/// Roundtrip a plan and verify that the output is the same as the input, after
/// being parsed to a Substrait plan and then back to text.
pub fn roundtrip_plan(input: &str) {
    let plan = Parser::parse(input).unwrap_or_else(|e| {
        println!("Error parsing plan:\n{e}");
        panic!("{e}");
    });

    let (actual, errors) = format(&plan);

    if !errors.is_empty() {
        println!("Formatting errors:");
        for error in errors {
            println!("  {error}");
        }
        panic!("Formatting errors occurred");
    }

    assert_eq!(
        actual.trim(),
        input.trim(),
        "Expected:\n---\n{}\n---\nActual:\n---\n{}\n---",
        input.trim(),
        actual.trim()
    );
}

/// Parse a built-in type string (e.g. `"i64"`, `"string?"`) into a
/// `proto::Type`. Panics on invalid type names.
pub fn parse_type(s: &str) -> proto::Type {
    let plan = parse(&format!("=== Plan\nRoot[x]\n  Read[types => x:{s}]"))
        .unwrap_or_else(|e| panic!("failed to parse type '{s}': {e}"));
    let plan_rel = plan.relations.first().expect("expected one relation");
    let root = match plan_rel.rel_type.as_ref() {
        Some(plan_rel::RelType::Root(root)) => root,
        other => panic!("expected Root relation, got {other:?}"),
    };
    let read = match root.input.as_ref().and_then(|rel| rel.rel_type.as_ref()) {
        Some(rel::RelType::Read(read)) => read,
        other => panic!("expected Read input, got {other:?}"),
    };
    read.base_schema
        .as_ref()
        .and_then(|schema| schema.r#struct.as_ref())
        .and_then(|schema| schema.types.first())
        .unwrap_or_else(|| panic!("parsed plan did not contain a type for '{s}'"))
        .clone()
}
