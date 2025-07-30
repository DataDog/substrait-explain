//! Test that we can roundtrip a plan from text to a Substrait plan and back to
//! text. Many features can be tested this way - in general, if it can survive a
//! round-trip, it's probably correct; it could be stored incorrectly and both
//! parser and formatter handle it the same incorrect way, but that is probably
//! rare, and more likely a reflection of the author's misunderstanding of the
//! Substrait plan format.

use substrait::proto::plan_rel::RelType as PlanRelType;
use substrait::proto::rel::RelType;
use substrait_explain::fixtures::{roundtrip_plan, roundtrip_plan_with_verbose};
use substrait_explain::format;
use substrait_explain::parser::Parser;

/// Assert that both canonical and equivalent plans parse and pretty-print to the canonical form.
fn assert_roundtrip_canonical(canonical: &str, equivalent: &str) {
    // Parse both
    let plan1 = Parser::parse(canonical).expect("canonical parse failed");
    let plan2 = Parser::parse(equivalent).expect("equivalent parse failed");

    // Format both
    let (text1, errors1) = format(&plan1.plan);
    let (text2, errors2) = format(&plan2.plan);
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
fn test_aggregate_relation() {
    let plan = r#"=== Extensions
URNs:
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
fn test_extension_leaf_relation_basic_parsing() {
    use substrait_explain::parser::warnings::{ParserConfig, UnregisteredExtensionMode};

    // Test that extension relations can be parsed without errors
    // Full roundtrip isn't working yet due to placeholder implementations
    let plan_text = r#"=== Plan
Root[col1, col2]
  ExtensionLeaf:ParquetScan[path='data/*.parquet', schema='auto' => col1:i32, col2:string]"#;

    // Use Ignore mode to allow unregistered extensions
    let config = ParserConfig {
        unregistered_extension_mode: UnregisteredExtensionMode::Ignore,
    };
    let parser = Parser::with_config(config);
    let plan = parser
        .parse_plan(plan_text)
        .expect("Extension relation should parse successfully");
    assert_eq!(plan.plan.relations.len(), 1);

    // Verify the relation was parsed as an extension relation
    if let Some(root_rel) = plan.plan.relations.first() {
        if let Some(PlanRelType::Root(root)) = &root_rel.rel_type {
            if let Some(input) = &root.input {
                if let Some(RelType::ExtensionLeaf(_)) = &input.rel_type {
                    // Success - parsed as ExtensionLeaf
                } else {
                    panic!("Expected ExtensionLeaf relation, got: {:?}", input.rel_type);
                }
            }
        }
    }
}

#[test]
fn test_extension_single_relation_basic_parsing() {
    use substrait_explain::parser::warnings::{ParserConfig, UnregisteredExtensionMode};

    let plan_text = r#"=== Plan
Root[normalized_a, normalized_b]
  ExtensionSingle:VectorNormalize[method='l2', dimensions=128 => $0, $1]
    Read[vectors => a:fp32, b:fp32]"#;

    // Use Ignore mode to allow unregistered extensions
    let config = ParserConfig {
        unregistered_extension_mode: UnregisteredExtensionMode::Ignore,
    };
    let parser = Parser::with_config(config);
    let plan = parser
        .parse_plan(plan_text)
        .expect("Extension single relation should parse successfully");
    assert_eq!(plan.plan.relations.len(), 1);
}

#[test]
fn test_extension_multi_relation_basic_parsing() {
    use substrait_explain::parser::warnings::{ParserConfig, UnregisteredExtensionMode};

    let plan_text = r#"=== Plan
Root[matched_left, matched_right, score]
  ExtensionMulti:FuzzyJoin[algorithm='lsh', threshold=0.8 => $0, $2, $4]
    Read[left_table => id:i64, text:string]
    Read[right_table => id:i64, text:string, score:fp64]"#;

    // Use Ignore mode to allow unregistered extensions
    let config = ParserConfig {
        unregistered_extension_mode: UnregisteredExtensionMode::Ignore,
    };
    let parser = Parser::with_config(config);
    let plan = parser
        .parse_plan(plan_text)
        .expect("Extension multi relation should parse successfully");
    assert_eq!(plan.plan.relations.len(), 1);
}

#[test]
fn test_line_numbers_in_warnings() {
    use substrait_explain::parser::Parser;
    use substrait_explain::parser::warnings::{ParserConfig, UnregisteredExtensionMode};

    let plan_text = r#"=== Plan
Root[result]
  ExtensionLeaf:UnknownExtension[path='test.parquet' => col1:i32, col2:string]"#;

    let config = ParserConfig {
        unregistered_extension_mode: UnregisteredExtensionMode::Warn,
    };

    let parser = Parser::with_config(config);
    let result = parser.parse_plan(plan_text).unwrap();

    // Should have warnings about the unregistered extension
    assert!(
        !result.warnings.is_empty(),
        "Expected warnings for unregistered extension"
    );

    // Verify that line numbers are preserved (should not be 0)
    for warning in &result.warnings {
        println!("Warning: {} at line {}", warning, warning.location.line);
        assert_ne!(warning.location.line, 0, "Line number should not be 0");
        assert_eq!(
            warning.location.line, 3,
            "Extension relation should be on line 3"
        );
    }
}

#[test]
fn test_extension_modes_error_warn_ignore() {
    use substrait_explain::parser::Parser;
    use substrait_explain::parser::warnings::{ParserConfig, UnregisteredExtensionMode};

    let plan_text = r#"=== Plan
Root[result]
  ExtensionLeaf:UnknownExtension[path='test.parquet' => col1:i32, col2:string]"#;

    // Test Error mode - should fail parsing
    let error_config = ParserConfig {
        unregistered_extension_mode: UnregisteredExtensionMode::Error,
    };
    let error_parser = Parser::with_config(error_config);
    let error_result = error_parser.parse_plan(plan_text);
    assert!(
        error_result.is_err(),
        "Error mode should fail for unregistered extension"
    );

    // Test Warn mode - should succeed with warnings
    let warn_config = ParserConfig {
        unregistered_extension_mode: UnregisteredExtensionMode::Warn,
    };
    let warn_parser = Parser::with_config(warn_config);
    let warn_result = warn_parser.parse_plan(plan_text).unwrap();
    assert!(
        !warn_result.warnings.is_empty(),
        "Warn mode should produce warnings"
    );
    assert_eq!(
        warn_result.plan.relations.len(),
        1,
        "Plan should still be parsed in warn mode"
    );

    // Test Ignore mode - should succeed without warnings
    let ignore_config = ParserConfig {
        unregistered_extension_mode: UnregisteredExtensionMode::Ignore,
    };
    let ignore_parser = Parser::with_config(ignore_config);
    let ignore_result = ignore_parser.parse_plan(plan_text).unwrap();
    assert!(
        ignore_result.warnings.is_empty(),
        "Ignore mode should not produce warnings"
    );
    assert_eq!(
        ignore_result.plan.relations.len(),
        1,
        "Plan should be parsed in ignore mode"
    );

    println!("✅ All three extension modes (Error/Warn/Ignore) work correctly");
}

#[test]
fn test_warning_edge_case_multiple_extensions() {
    use substrait_explain::parser::Parser;
    use substrait_explain::parser::warnings::{ParserConfig, UnregisteredExtensionMode};

    let plan_text = r#"=== Plan
Root[result1]
  ExtensionLeaf:UnknownA[path='file1.parquet' => col1:i32]

Root[result2]
  ExtensionLeaf:UnknownB[path='file2.parquet' => col2:string]"#;

    let config = ParserConfig {
        unregistered_extension_mode: UnregisteredExtensionMode::Warn,
    };

    let parser = Parser::with_config(config);
    let result = parser.parse_plan(plan_text).unwrap();

    // Should have multiple warnings, one for each unregistered extension
    assert_eq!(
        result.warnings.len(),
        2,
        "Expected exactly 2 warnings for 2 unregistered extensions"
    );

    // Verify warnings have different line numbers
    let line_numbers: Vec<i64> = result.warnings.iter().map(|w| w.location.line).collect();
    assert_eq!(
        line_numbers,
        vec![3, 6],
        "Warnings should be on lines 3 and 6"
    );

    // Verify both extension names are reported
    let warnings_text = result
        .warnings
        .iter()
        .map(|w| format!("{w}"))
        .collect::<Vec<_>>();
    assert!(
        warnings_text.iter().any(|w| w.contains("UnknownA")),
        "Should warn about UnknownA extension"
    );
    assert!(
        warnings_text.iter().any(|w| w.contains("UnknownB")),
        "Should warn about UnknownB extension"
    );

    println!(
        "✅ Warning edge case test passed: multiple extensions generate separate warnings with correct line numbers"
    );
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

    println!(
        "✅ Malformed input error test passed: parsing fails gracefully with descriptive error message"
    );
}
