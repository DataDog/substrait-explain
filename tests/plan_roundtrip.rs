//! Test that we can roundtrip a plan from text to a Substrait plan and back to
//! text. Many features can be tested this way - in general, if it can survive a
//! round-trip, it's probably correct; it could be stored incorrectly and both
//! parser and formatter handle it the same incorrect way, but that is probably
//! rare, and more likely a reflection of the author's misunderstanding of the
//! Substrait plan format.

mod common;

use common::{roundtrip_plan, roundtrip_plan_with_verbose};
use substrait_explain::format;
use substrait_explain::parser::Parser;

/// Assert that both canonical and equivalent plans parse and pretty-print to the canonical form.
fn assert_roundtrip_canonical(canonical: &str, equivalent: &str) {
    // Parse both
    let plan1 = Parser::parse(canonical).expect("canonical parse failed");
    let plan2 = Parser::parse(equivalent).expect("equivalent parse failed");

    // Format both
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

    // Both should match the canonical text
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

#[test]
fn test_sort_relation_roundtrip() {
    let plan = r#"=== Plan
Root[a, b]
  Sort[($0, &AscNullsFirst), ($1, &DescNullsLast) => $0, $1]
    Read[table => a:i32, b:string]"#;
    roundtrip_plan(plan);
}

#[test]
fn test_fetch_relation_roundtrip() {
    let plan_both = r#"=== Plan
Root[a, b]
  Fetch[limit=10, offset=5 => $0, $1]
    Read[table => a:i32, b:string]"#;

    let plan_offset_first = r#"=== Plan
Root[a, b]
  Fetch[offset=5, limit=10 => $0, $1]
    Read[table => a:i32, b:string]"#;

    assert_roundtrip_canonical(plan_both, plan_offset_first);

    let plan_limit_only = r#"=== Plan
Root[a, b]
  Fetch[limit=10 => $0, $1]
    Read[table => a:i32, b:string]"#;
    roundtrip_plan(plan_limit_only);

    let plan_offset_only = r#"=== Plan
Root[a, b]
  Fetch[offset=5 => $0, $1]
    Read[table => a:i32, b:string]"#;
    roundtrip_plan(plan_offset_only);

    let plan_empty = r#"=== Plan
Root[a, b]
  Fetch[_ => $0, $1]
    Read[table => a:i32, b:string]"#;
    roundtrip_plan(plan_empty);
}

#[test]
fn test_join_relation_roundtrip() {
    let plan = r#"=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_comparison.yaml
Functions:
  # 10 @  1: eq

=== Plan
Root[id, name, amount]
  Join[&Inner, eq($0, $2) => $0, $1, $3]
    Read[users => id:i64, name:string]
    Read[orders => user_id:i64, amount:i32]"#;

    roundtrip_plan(plan);
}

#[test]
fn test_join_relation_semi_types_roundtrip() {
    // Test LeftSemi join - should output only left columns
    let plan_semi = r#"=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_comparison.yaml
Functions:
  # 10 @  1: eq

=== Plan
Root[a, b]
  Join[&LeftSemi, eq($0, $2) => $0, $1]
    Read[left_table => a:i32, b:string]
    Read[right_table => c:i32, d:string]"#;
    roundtrip_plan(plan_semi);
}

#[test]
fn test_join_relation_right_mark_roundtrip() {
    // Test RightMark join - should output right columns plus mark column
    // Left: 2 columns, Right: 3 columns, RightMark outputs 4 total: $0, $1, $2, $3
    // We output just the last right column ($2) and the mark ($3)
    let plan_right_mark = r#"=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_comparison.yaml
Functions:
  # 10 @  1: eq

=== Plan
Root[d, mark]
  Join[&RightMark, eq($0, $2) => $2, $3]
    Read[left_table => a:i32, b:string]
    Read[right_table => c:i32, d:string, e:boolean]"#;
    roundtrip_plan(plan_right_mark);
}
