use substrait_explain::fixtures::{roundtrip_plan, roundtrip_plan_with_verbose};

#[test]
fn test_types_roundtrip() {
    let cases = [
        "i32",
        "i32?",
        "binary",
        "binary?",
        "timestamp",
        "timestamp?",
        "timestamp_tz",
        "timestamp_tz?",
        "date",
        "date?",
        "time",
        "time?",
        "interval_year",
        "interval_year?",
        "uuid",
        "uuid?",
        "list<i64>",
        "list?<i64?>",
    ];

    for (idx, typ) in cases.iter().enumerate() {
        let plan = format!("=== Plan\nRoot[c{idx}]\n  Read[t{idx} => c{idx}:{typ}]");
        roundtrip_plan(&plan);
    }
}

#[test]
fn test_user_defined_type_roundtrip() {
    let simple = r#"=== Extensions
URNs:
  @  4: some_source
Types:
  # 12 @  4: MyType

=== Plan
Root[result]
  Read[t => col:MyType<i32?>]"#;

    let verbose = r#"=== Extensions
URNs:
  @  4: some_source
Types:
  # 12 @  4: MyType

=== Plan
Root[result]
  Read[t => col:MyType#12<i32?>]"#;

    roundtrip_plan_with_verbose(simple, verbose);
}

#[test]
fn test_expression_roundtrip() {
    let plan = r#"=== Extensions
URNs:
  @  4: some_source
Functions:
  # 12 @  4: foo
  # 14 @  4: bar

=== Plan
Root[result]
  Project[foo(bar(12:i16, 18), -4:i16), foo($2, bar($5, 18), -4:i16)]
    Read[t => a:i64, b:i64, c:i64, d:i64, e:i64, f:i64]"#;

    roundtrip_plan(plan);
}

#[test]
fn test_verbose_and_simple_output() {
    let simple = r#"=== Extensions
URNs:
  @  4: some_source
Functions:
  # 12 @  4: foo
  # 14 @  4: bar

=== Plan
Root[result]
  Project[foo(), foo(3:i32, 5), foo(bar(5, $2))]
    Read[t => a:i64, b:i64, c:i64]"#;

    let verbose = r#"=== Extensions
URNs:
  @  4: some_source
Functions:
  # 12 @  4: foo
  # 14 @  4: bar

=== Plan
Root[result]
  Project[foo#12(), foo#12(3:i32, 5:i64), foo#12(bar#14(5:i64, $2))]
    Read[t => a:i64, b:i64, c:i64]"#;

    roundtrip_plan_with_verbose(simple, verbose);
}
