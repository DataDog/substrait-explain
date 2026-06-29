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

/// Assert that both `canonical` and `equivalent` parse and pretty-print to the
/// canonical form. Use this when two different textual inputs should produce the
/// same plan (e.g. a verbose form and its compact rendering, or a multi-line
/// relation and its inline equivalent).
pub fn assert_roundtrip_canonical(canonical: &str, equivalent: &str) {
    let plan1 = Parser::parse(canonical).expect("canonical parse failed");
    let plan2 = Parser::parse(equivalent).expect("equivalent parse failed");

    let (text1, errors1) = format(&plan1);
    let (text2, errors2) = format(&plan2);
    assert!(
        errors1.is_empty(),
        "Formatting errors for canonical: {errors1:?}"
    );
    assert!(
        errors2.is_empty(),
        "Formatting errors for equivalent: {errors2:?}"
    );

    assert_eq!(
        text1.trim(),
        canonical.trim(),
        "Canonical did not roundtrip to itself"
    );
    assert_eq!(
        text2.trim(),
        canonical.trim(),
        "Equivalent did not roundtrip to canonical"
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
