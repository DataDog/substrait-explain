//! Test roundtrip functionality for literal parsing and formatting

mod common;

use common::roundtrip_plan;

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
