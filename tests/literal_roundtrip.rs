//! Test roundtrip functionality for literal parsing and formatting

use substrait::proto::expression::RexType;
use substrait::proto::expression::literal::LiteralType;
use substrait::proto::plan_rel::RelType as PlanRelType;
use substrait::proto::rel::RelType;
use substrait_explain::fixtures::roundtrip_plan;
use substrait_explain::parser::Parser;

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

#[test]
fn test_nullable_string_typed_literal_sets_nullable() {
    let plan = r#"
=== Plan
Root[result]
  Project['hello':string?]
    Read[data => a:i64]
"#;

    roundtrip_plan(plan);

    let plan = Parser::parse(plan).expect("parse should succeed");
    let plan_rel = plan.relations.first().expect("plan relation");
    let PlanRelType::Root(root) = plan_rel.rel_type.as_ref().expect("plan rel type") else {
        panic!("expected root relation");
    };
    let RelType::Project(project) = root
        .input
        .as_ref()
        .and_then(|r| r.rel_type.as_ref())
        .expect("project relation")
    else {
        panic!("expected project relation");
    };

    let literal = project
        .expressions
        .first()
        .and_then(|e| e.rex_type.as_ref())
        .and_then(|rex| match rex {
            RexType::Literal(lit) => Some(lit),
            _ => None,
        })
        .expect("literal expression");

    assert!(
        literal.nullable,
        "typed string nullability should be preserved"
    );
    assert!(matches!(
        literal.literal_type.as_ref(),
        Some(LiteralType::String(s)) if s == "hello"
    ));
}

#[test]
fn test_unsupported_typed_string_literal_errors() {
    let plan = r#"
=== Plan
Root[result]
  Project['abc':uuid]
    Read[data => a:i64]
"#;

    let err = Parser::parse(plan).expect_err("parse should fail");
    let message = err.to_string();
    assert!(
        message.contains("Invalid type for string literal"),
        "unexpected error: {message}"
    );
}
