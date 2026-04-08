//! Test roundtrip functionality for literal parsing and formatting

use substrait_explain::fixtures::roundtrip_plan;

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

#[test]
fn test_date_literal_roundtrip() {
    let plan = r#"
=== Plan
Root[result]
  Project['2023-12-25':date]
    Read[data => a:i64]
"#;
    roundtrip_plan(plan);
}

#[test]
fn test_time_literal_roundtrip() {
    let plan = r#"
=== Plan
Root[result]
  Project['14:30:45':time]
    Read[data => a:i64]
"#;
    roundtrip_plan(plan);
}

#[test]
fn test_timestamp_literal_roundtrip() {
    let plan = r#"
=== Plan
Root[result]
  Project['2023-01-01T12:00:00':timestamp]
    Read[data => a:i64]
"#;
    roundtrip_plan(plan);
}

#[test]
fn test_nullable_integer_literal_roundtrip() {
    let plan = r#"
=== Plan
Root[result]
  Project[78:i32?, 42:i64?]
    Read[data => a:i64]
"#;
    roundtrip_plan(plan);
}

#[test]
fn test_nullable_boolean_literal_roundtrip() {
    let plan = r#"
=== Plan
Root[result]
  Project[true:boolean?, false:boolean?]
    Read[data => a:i64]
"#;
    roundtrip_plan(plan);
}

#[test]
fn test_nullable_float_literal_roundtrip() {
    let plan = r#"
=== Plan
Root[result]
  Project[3.14:fp64?, 2.5:fp32?]
    Read[data => a:i64]
"#;
    roundtrip_plan(plan);
}

#[test]
fn test_if_then_expression_roundtrip() {
    let plan = r#"
=== Plan
Root[statusq]
  Fetch[limit=10, offset=0 => ]
    Project[if_then(true -> $0, false -> $1, _ -> $2)]
      Read[events.logs => status:string?]
"#;
    roundtrip_plan(plan);
}
