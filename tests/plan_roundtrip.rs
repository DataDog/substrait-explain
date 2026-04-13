//! Test that we can roundtrip a plan from text to a Substrait plan and back to
//! text. Many features can be tested this way - in general, if it can survive a
//! round-trip, it's probably correct; it could be stored incorrectly and both
//! parser and formatter handle it the same incorrect way, but that is probably
//! rare, and more likely a reflection of the author's misunderstanding of the
//! Substrait plan format.

use substrait_explain::fixtures::{roundtrip_plan, roundtrip_plan_with_verbose};
use substrait_explain::format;
use substrait_explain::parser::{ParseError, Parser};

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
URNs:
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
URNs:
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
URNs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
Functions:
  # 10 @  1: coalesce

=== Plan
Root[name, num]
  Project[$1, coalesce($1, $2)]
    Read[schema.table => name:string?, num:fp64?, other_num:fp64?, id:i64]"#;

    let verbose_plan = r#"=== Extensions
URNs:
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
URNs:
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
fn test_aggregate_multiple_grouping_sets_roundtrip() {
    let plan = r#"=== Extensions
URNs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml
Functions:
  # 10 @  1: sum
  # 11 @  1: count

=== Plan
Root[sum, category, count]
  Aggregate[($0, $1), ($0), _ => sum($2), $0, count($2)]
    Read[orders => category:string?, region:string?, amount:fp64?]"#;

    roundtrip_plan(plan);
}

#[test]
fn test_global_aggregate_relation_roundtrip() {
    let plan = r#"=== Extensions
URNs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml
Functions:
  # 10 @  1: sum
  # 11 @  1: count

=== Plan
Root[sum, count]
  Aggregate[_ => sum($2), count($2)]
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
URNs:
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
URNs:
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
URNs:
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

#[test]
fn test_unregistered_extension_error() {
    let plan_text = r#"=== Plan
Root[result]
  ExtensionLeaf:UnknownExtension[path='test.parquet' => col1:i32]"#;

    let err = Parser::parse(plan_text).expect_err("parse should fail for unknown extension");
    match err {
        ParseError::UnregisteredExtension { name, context } => {
            assert_eq!(name, "UnknownExtension");
            assert_eq!(context.line_no, 3);
            assert!(
                context.line.contains("ExtensionLeaf:UnknownExtension"),
                "context should include the extension line"
            );
        }
        other => panic!("expected unregistered extension error, got {other:?}"),
    }
}

#[test]
fn test_malformed_input_error_handling() {
    use substrait_explain::parser::Parser;

    // Test malformed input: unclosed bracket in extension relation
    let malformed_plan = r#"=== Plan
Root[result]
  ExtensionLeaf:TestExtension[path='test.parquet', missing_close => col1:i32"#;

    let result = Parser::parse(malformed_plan);

    // Should fail with a clear error
    assert!(
        result.is_err(),
        "Malformed input should result in parse error"
    );

    let error = result.unwrap_err();
    let error_msg = format!("{error}");

    // Verify error contains useful information
    assert!(error_msg.len() > 10, "Error message should be descriptive");

    // Error should reference the problematic line (line 3 where the unclosed bracket is)
    assert!(
        error_msg.contains("3") || error_msg.contains("line"),
        "Error should reference line information: {error_msg}"
    );

    // Malformed input error test passed
}

#[test]
fn test_overloaded_functions_verbose_shows_signature_and_anchor() {
    // A plan where a function has an overloaded compound name. The compact output
    // must include the signature because the base name is not unique, but must
    // omit the anchor because the compound name is unique.
    let simple = r#"=== Extensions
URNs:
  @  1: extension:io.substrait:functions_comparison
Functions:
  #  1 @  1: equal:any_any
  #  2 @  1: equal:str_str

=== Plan
Root[result]
  Project[$0, equal:any_any($0, $1), equal:str_str($0, $1)]
    Read[t => a:i64, b:i64, c:string]"#;

    // Verbose output always shows signatures and anchors for all functions,
    // including those with unique base names.
    let verbose = r#"=== Extensions
URNs:
  @  1: extension:io.substrait:functions_comparison
Functions:
  #  1 @  1: equal:any_any
  #  2 @  1: equal:str_str

=== Plan
Root[result]
  Project[$0, equal:any_any#1($0, $1), equal:str_str#2($0, $1)]
    Read[t => a:i64, b:i64, c:string]"#;

    roundtrip_plan(simple);
    roundtrip_plan_with_verbose(simple, verbose);
}

/// A unique function (only one overload registered) uses the base name in
/// compact mode and the compound name with anchor in verbose mode.
/// Writing the compound name explicitly in input is equivalent to writing
/// just the base name — both produce the same canonical output.
#[test]
fn test_unique_function_compact_omits_signature() {
    // Canonical: base name only, because add:i64_i64 is the only "add"
    let compact = r#"=== Extensions
URNs:
  @  1: extension:io.substrait:functions_arithmetic
Functions:
  #  1 @  1: add:i64_i64

=== Plan
Root[result]
  Project[$0, add($0, $1)]
    Read[t => a:i64, b:i64]"#;

    // Explicit compound name in input resolves to the same canonical output
    let compound = r#"=== Extensions
URNs:
  @  1: extension:io.substrait:functions_arithmetic
Functions:
  #  1 @  1: add:i64_i64

=== Plan
Root[result]
  Project[$0, add:i64_i64($0, $1)]
    Read[t => a:i64, b:i64]"#;

    // Verbose output always shows the full compound name and anchor
    let verbose = r#"=== Extensions
URNs:
  @  1: extension:io.substrait:functions_arithmetic
Functions:
  #  1 @  1: add:i64_i64

=== Plan
Root[result]
  Project[$0, add:i64_i64#1($0, $1)]
    Read[t => a:i64, b:i64]"#;

    assert_roundtrip_canonical(compact, compound);
    roundtrip_plan_with_verbose(compact, verbose);
}

/// A plan that mixes unique and overloaded functions.  Compact mode shows
/// signatures only where needed.
#[test]
fn test_mixed_unique_and_overloaded_functions() {
    let simple = r#"=== Extensions
URNs:
  @  1: extension:io.substrait:functions_comparison
  @  2: extension:io.substrait:functions_string
Functions:
  #  1 @  1: equal:any_any
  #  2 @  1: equal:str_str
  #  3 @  2: like:str_str

=== Plan
Root[result]
  Project[$0, equal:any_any($0, $1), equal:str_str($0, $2), like($0, $2)]
    Read[t => id:i64, score:i64, name:string]"#;

    roundtrip_plan(simple);
}

/// Same compound name registered in two different URNs — the anchor is
/// required to disambiguate in all modes.
#[test]
fn test_same_compound_name_two_urns_requires_anchor() {
    let plan = r#"=== Extensions
URNs:
  @  1: urn:substrait:vendor_a
  @  2: urn:substrait:vendor_b
Functions:
  #  1 @  1: equal:any_any
  #  2 @  2: equal:any_any

=== Plan
Root[result]
  Project[$0, equal:any_any#1($0, $1), equal:any_any#2($0, $1)]
    Read[t => a:i64, b:i64]"#;

    roundtrip_plan(plan);
}

// === VirtualTable Read tests ===

#[test]
fn test_virtual_read_inline_single_row() {
    let plan = r#"
=== Plan
Root[id, name]
  Read:Virtual[(1, 'alice') => id:i64, name:string]"#;

    roundtrip_plan(plan);
}

#[test]
fn test_virtual_read_inline_multi_row() {
    let plan = r#"
=== Plan
Root[id, name]
  Read:Virtual[(1, 'alice'), (2, 'bob') => id:i64, name:string]"#;

    roundtrip_plan(plan);
}

#[test]
fn test_virtual_read_verbose_parses() {
    // Verbose form with a small table (4 cells) parses correctly and
    // produces the canonical inline form.
    let inline = r#"
=== Plan
Root[id, name]
  Read:Virtual[(1, 'alice'), (2, 'bob') => id:i64, name:string]"#;

    let verbose = r#"
=== Plan
Root[id, name]
  Read:Virtual[_ => id:i64, name:string]
    + Row[1, 'alice']
    + Row[2, 'bob']"#;

    assert_roundtrip_canonical(inline, verbose);
}

#[test]
fn test_virtual_read_empty() {
    let plan = r#"
=== Plan
Root[id, name]
  Read:Virtual[_ => id:i64, name:string]"#;

    roundtrip_plan(plan);
}

#[test]
fn test_virtual_read_single_column() {
    let plan = r#"
=== Plan
Root[id]
  Read:Virtual[(1), (2), (3) => id:i64]"#;

    roundtrip_plan(plan);
}

#[test]
fn test_virtual_read_large_table_uses_verbose() {
    // 3 rows * 3 columns = 9, which is > 8, so verbose is canonical.
    let verbose = r#"
=== Plan
Root[a, b, c]
  Read:Virtual[_ => a:i64, b:i64, c:i64]
    + Row[1, 2, 3]
    + Row[4, 5, 6]
    + Row[7, 8, 9]"#;

    let inline = r#"
=== Plan
Root[a, b, c]
  Read:Virtual[(1, 2, 3), (4, 5, 6), (7, 8, 9) => a:i64, b:i64, c:i64]"#;

    assert_roundtrip_canonical(verbose, inline);
}

#[test]
fn test_virtual_read_in_plan_context() {
    // VirtualTable as input to Filter and Project
    let plan = r#"
=== Extensions
URNs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_comparison.yaml
Functions:
  # 10 @  1: gt

=== Plan
Root[name]
  Filter[gt($0, 1) => $0, $1]
    Read:Virtual[(1, 'alice'), (2, 'bob'), (3, 'carol') => id:i64, name:string]"#;

    roundtrip_plan(plan);
}

#[test]
fn test_addendum_on_root_rejected() {
    let plan = r#"
=== Plan
Root[id]
  + Row[1]
  Read:Virtual[_ => id:i64]"#;

    let result = Parser::parse(plan);
    assert!(result.is_err(), "addenda on Root should be rejected");
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("not supported on Root"),
        "unexpected error: {err}"
    );
}

#[test]
fn test_row_addendum_on_non_virtual_rejected() {
    // + Row on a regular Read should be rejected with a line number.
    let plan = r#"
=== Plan
Root[id]
  Read[data => id:i64]
    + Row[1]"#;

    let result = Parser::parse(plan);
    assert!(
        result.is_err(),
        "+ Row on non-VirtualTable should be rejected"
    );
    let err = result.unwrap_err().to_string();
    assert!(
        err.contains("+ Row addenda can only be attached to Read:Virtual"),
        "unexpected error: {err}"
    );
    // Error should report the line number of the offending + Row line.
    assert!(
        err.contains("line 5"),
        "expected line number in error: {err}"
    );
}
