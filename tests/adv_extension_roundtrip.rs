//! Integration tests for advanced extension (Enh/Opt) round-trip.
//!
//! Tests parse → textify round-trips for `+ Enh:` and `+ Opt:` annotations
//! on standard relations, using [`PartitionHint`] as the concrete example.

use substrait::proto::plan_rel;
use substrait::proto::rel::RelType;
use substrait_explain::extensions::ExtensionRegistry;
use substrait_explain::extensions::examples::{PartitionHint, PartitionStrategy};
use substrait_explain::extensions::registry::ExtensionError;
use substrait_explain::parser::Parser;
use substrait_explain::{FormatError, format_with_registry};

fn make_registry() -> ExtensionRegistry {
    let mut registry = ExtensionRegistry::new();
    registry
        .register_enhancement::<PartitionHint>()
        .expect("register_enhancement");
    registry
}

// ---------------------------------------------------------------------------
// Round-trip: parse → textify → compare
// ---------------------------------------------------------------------------

#[test]
fn test_partition_hint_roundtrip() {
    let registry = make_registry();

    let plan_text = r#"=== Plan
Root[result]
  Read[my.table => col:i64]
    + Enh:PartitionHint[&HASH, &RANGE]"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

#[test]
fn test_partition_hint_with_count() {
    let registry = make_registry();

    let plan_text = r#"=== Plan
Root[result]
  Read[my.table => col:i64]
    + Enh:PartitionHint[&HASH, count=8]"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

#[test]
fn test_partition_hint_all_strategies() {
    let registry = make_registry();

    let plan_text = r#"=== Plan
Root[result]
  Read[my.table => col:i64]
    + Enh:PartitionHint[&UNSPECIFIED, &HASH, &RANGE, &BROADCAST]"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

#[test]
fn test_partition_hint_empty() {
    let registry = make_registry();

    // Zero strategies, no count → args render as `_`
    let plan_text = r#"=== Plan
Root[result]
  Read[my.table => col:i64]
    + Enh:PartitionHint[_]"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

/// Verify the enhancement attaches to the correct relation when the plan has
/// multiple levels of nesting.
#[test]
fn test_enhancement_with_child_relation() {
    let registry = make_registry();

    // The enhancement annotates the Filter; it appears before the Read child
    // so the annotation visually attaches to Filter and not to Read.
    let plan_text = r#"=== Plan
Root[result]
  Filter[$0 => $0]
    + Enh:PartitionHint[&BROADCAST]
    Read[my.table => col:i64]"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

/// Proto-level verification: enhancement must be attached to the Filter relation,
/// not to the Read child.
#[test]
fn test_enhancement_attaches_to_filter_not_read() {
    let registry = make_registry();

    let plan_text = r#"=== Plan
Root[result]
  Filter[$0 => $0]
    + Enh:PartitionHint[&BROADCAST]
    Read[my.table => col:i64]"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    // Navigate: Plan → PlanRel → Root → Filter
    let plan_rel = plan.relations.first().expect("expected a relation");
    let root = match &plan_rel.rel_type {
        Some(plan_rel::RelType::Root(r)) => r,
        other => panic!("expected Root, got {other:?}"),
    };
    let filter = match root.input.as_ref().and_then(|r| r.rel_type.as_ref()) {
        Some(RelType::Filter(f)) => f,
        other => panic!("expected Filter as Root input, got {other:?}"),
    };

    assert!(
        filter.advanced_extension.is_some(),
        "expected enhancement on Filter, but advanced_extension is None"
    );

    // Read child must NOT carry the enhancement.
    let read = match filter.input.as_deref().and_then(|r| r.rel_type.as_ref()) {
        Some(RelType::Read(r)) => r,
        other => panic!("expected Read as Filter input, got {other:?}"),
    };
    assert!(
        read.advanced_extension.is_none(),
        "expected no enhancement on Read, but advanced_extension is Some"
    );
}

// ---------------------------------------------------------------------------
// Multiple optimizations
// ---------------------------------------------------------------------------

/// A minimal Explainable + prost::Message used to register as an optimization.
mod opt_fixture {
    use prost::Name;
    use substrait_explain::extensions::{
        ExplainContext, Explainable, ExtensionArgs, ExtensionError, ExtensionRelationType,
        ExtensionValue,
    };

    #[derive(Clone, PartialEq, prost::Message)]
    pub struct PlanHint {
        #[prost(string, tag = "1")]
        pub hint: String,
    }

    impl Name for PlanHint {
        const NAME: &'static str = "PlanHint";
        const PACKAGE: &'static str = "test";

        fn full_name() -> String {
            "test.PlanHint".to_owned()
        }

        fn type_url() -> String {
            "type.googleapis.com/test.PlanHint".to_owned()
        }
    }

    impl Explainable for PlanHint {
        fn name() -> &'static str {
            "PlanHint"
        }

        fn from_args(args: &ExtensionArgs, _ctx: &ExplainContext) -> Result<Self, ExtensionError> {
            let mut extractor = args.extractor();
            let hint: String = extractor.expect_named_arg::<&str>("hint")?.to_owned();
            extractor.check_exhausted()?;
            Ok(PlanHint { hint })
        }

        fn to_args(&self, _ctx: &ExplainContext) -> Result<ExtensionArgs, ExtensionError> {
            let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf);
            args.named
                .insert("hint".to_owned(), ExtensionValue::String(self.hint.clone()));
            Ok(args)
        }
    }
}

#[test]
fn test_multiple_optimizations_roundtrip() {
    let mut registry = ExtensionRegistry::new();
    registry
        .register_optimization::<opt_fixture::PlanHint>()
        .expect("register_optimization");

    let plan_text = r#"=== Plan
Root[result]
  Read[my.table => col:i64]
    + Opt:PlanHint[hint='use_index']
    + Opt:PlanHint[hint='parallel']"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

#[test]
fn test_enhancement_and_optimization_on_same_relation() {
    let mut registry = make_registry();
    registry
        .register_optimization::<opt_fixture::PlanHint>()
        .expect("register_optimization");

    let plan_text = r#"=== Plan
Root[result]
  Read[my.table => col:i64]
    + Enh:PartitionHint[&HASH]
    + Opt:PlanHint[hint='use_index']"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

// ---------------------------------------------------------------------------
// Error cases
// ---------------------------------------------------------------------------

/// Parsing a plan with an enhancement fails when the type is not registered.
#[test]
fn test_unregistered_enhancement_fails_to_parse() {
    let registry = ExtensionRegistry::new();

    let plan_text = r#"
=== Plan
Root[result]
  Read[my.table => col:i64]
    + Enh:PartitionHint[&HASH, count=4]
"#;

    let parser = Parser::new().with_extension_registry(registry);
    let result = parser.parse_plan(plan_text);
    assert!(
        result.is_err(),
        "expected parse error for unregistered enhancement"
    );
}

/// Textifying a plan whose enhancement type URL is unknown emits a lenient
/// warning rather than panicking or dropping the line entirely.
#[test]
fn test_unknown_enhancement_url_textify_is_lenient() {
    // Parse with a registry that knows PartitionHint
    let registry = make_registry();
    let parser = Parser::new().with_extension_registry(registry.clone());

    let plan_text = r#"=== Plan
Root[result]
  Read[my.table => col:i64]
    + Enh:PartitionHint[&HASH]"#;

    let plan = parser.parse_plan(plan_text).expect("parse failed");

    // Format with an empty registry — the type URL is no longer known
    let empty_registry = ExtensionRegistry::new();
    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &empty_registry);

    // The rest of the plan (Root, Read) should still be rendered.
    assert!(
        formatted.contains("Root["),
        "expected Root in output, got:\n{formatted}"
    );
    assert!(
        formatted.contains("Read["),
        "expected Read in output, got:\n{formatted}"
    );
    // The enhancement line should appear with the prefix but no name (decode failed).
    assert!(
        formatted.contains("+ Enh["),
        "expected fallback enhancement line in output, got:\n{formatted}"
    );
    // At least one format error should be reported.
    assert!(
        !errors.is_empty(),
        "expected at least one format error for unknown enhancement type URL"
    );
}

/// Parsing a plan with two `+ Enh:` lines on the same relation must fail.
/// Substrait semantics allow at most one enhancement per relation.
#[test]
fn test_multiple_enhancements_fails_to_parse() {
    let registry = make_registry();

    let plan_text = r#"=== Plan
Root[result]
  Read[my.table => col:i64]
    + Enh:PartitionHint[&HASH]
    + Enh:PartitionHint[&RANGE]"#;

    let parser = Parser::new().with_extension_registry(registry);
    let result = parser.parse_plan(plan_text);
    assert!(
        result.is_err(),
        "expected parse error for duplicate enhancement, but parse succeeded"
    );
    let err_msg = result.unwrap_err().to_string();
    assert!(
        err_msg.contains("enhancement") || err_msg.contains("Enhancement"),
        "error message should mention 'enhancement', got: {err_msg}"
    );
}

#[test]
fn test_adv_extension_as_standalone_root_fails_with_error() {
    let registry = make_registry();

    let plan_text = r#"=== Plan
+ Enh:PartitionHint[&HASH]"#;

    let parser = Parser::new().with_extension_registry(registry);
    let result = parser.parse_plan(plan_text);
    assert!(
        result.is_err(),
        "expected parse error for adv_extension as standalone root, but parse succeeded"
    );
}

#[test]
fn test_adv_extension_as_roots_only_child_fails_with_error() {
    let registry = make_registry();

    let plan_text = r#"=== Plan
Root[result]
  + Enh:PartitionHint[&HASH]"#;

    let parser = Parser::new().with_extension_registry(registry);
    let result = parser.parse_plan(plan_text);
    assert!(
        result.is_err(),
        "expected parse error for adv_extension as Root's only child, but parse succeeded"
    );
}

// ---------------------------------------------------------------------------
// Additional relation types
// ---------------------------------------------------------------------------

/// Enhancement round-trip on a Filter relation.
#[test]
fn test_enhancement_on_filter_roundtrip() {
    let registry = make_registry();

    let plan_text = r#"=== Plan
Root[result]
  Filter[$0 => $0]
    + Enh:PartitionHint[&HASH, count=4]
    Read[my.table => col:i64]"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

/// Enhancement round-trip on a Sort relation.
#[test]
fn test_enhancement_on_sort_roundtrip() {
    let registry = make_registry();

    let plan_text = r#"=== Plan
Root[result]
  Sort[($0, &AscNullsFirst) => $0]
    + Enh:PartitionHint[&RANGE]
    Read[my.table => col:i64]"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

// ---------------------------------------------------------------------------
// Additional error cases
// ---------------------------------------------------------------------------

/// Parsing a plan with an unregistered `+ Opt:` type fails at parse time.
#[test]
fn test_unregistered_optimization_fails_to_parse() {
    let registry = ExtensionRegistry::new(); // empty — nothing registered

    let plan_text = r#"=== Plan
Root[result]
  Read[my.table => col:i64]
    + Opt:PlanHint[hint='fast']"#;

    let parser = Parser::new().with_extension_registry(registry);
    let result = parser.parse_plan(plan_text);
    assert!(
        result.is_err(),
        "expected parse error for unregistered optimization"
    );
}

/// Passing a non-enum positional arg (integer) where PartitionHint expects
/// enum values must produce a parse error.
#[test]
fn test_wrong_argument_type_for_enhancement_fails() {
    let registry = make_registry();

    // PartitionHint expects &VARIANT enum values as positional args, not integers.
    let plan_text = r#"=== Plan
Root[result]
  Read[my.table => col:i64]
    + Enh:PartitionHint[5]"#;

    let parser = Parser::new().with_extension_registry(registry);
    let result = parser.parse_plan(plan_text);
    assert!(
        result.is_err(),
        "expected parse error when integer is passed where enum is expected"
    );
}

// ---------------------------------------------------------------------------
// Textify failure token for unregistered enhancement
// ---------------------------------------------------------------------------

/// Verifies the behaviour described in GRAMMAR.md: when an enhancement's type
/// URL is present in the plan proto but is not registered in the registry at
/// textify time, the line is emitted with `!{extension}` in place of the name
/// and arguments, a `FormatError` is collected, and the rest of the plan is
/// unaffected.
///
/// Setup: parse with a registry that knows PartitionHint, then textify with an
/// empty registry so the type URL cannot be resolved.
#[test]
fn test_unregistered_enhancement_produces_failure_token_at_textify_time() {
    // Parse with a full registry so the plan is valid.
    let parse_registry = make_registry();
    let plan_text = r#"=== Plan
Root[result]
  Read[my.table => col:i64]
    + Enh:PartitionHint[&HASH]"#;

    let parser = Parser::new().with_extension_registry(parse_registry);
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    // Textify with an empty registry — the enhancement type URL is unknown.
    let empty_registry = ExtensionRegistry::new();
    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &empty_registry);

    // The rest of the plan must be unaffected.
    assert!(
        formatted.contains("Read[my.table => col:i64]"),
        "expected Read line to be present, got:\n{formatted}"
    );

    // The enhancement line must contain the failure token with no name or args.
    assert!(
        formatted.contains("+ Enh[!{extension}]"),
        "expected failure token '+ Enh[!{{extension}}]' in output, got:\n{formatted}"
    );

    // Exactly one FormatError must have been collected.
    assert_eq!(
        errors.len(),
        1,
        "expected exactly one FormatError, got: {errors:?}"
    );

    // The error must be an extension-not-found error.
    assert!(
        matches!(
            &errors[0],
            FormatError::Extension(ExtensionError::NotFound { .. })
        ),
        "expected FormatError::Extension(ExtensionError::NotFound {{ .. }}), got: {:?}",
        errors[0]
    );
}

// ---------------------------------------------------------------------------
// Project over extension child: get_input_field_count
// ---------------------------------------------------------------------------

/// A minimal two-column extension leaf used to verify that `get_input_field_count`
/// correctly reads the output column count back from the identity emit stored in
/// `RelCommon` when the child of a `Project` is an extension relation.
mod extension_child_fixture {
    use prost::Name;
    use substrait_explain::extensions::{
        ExplainContext, Explainable, ExtensionArgs, ExtensionColumn, ExtensionError,
        ExtensionRelationType,
    };

    #[derive(Clone, PartialEq, prost::Message)]
    pub struct TwoColumnScan {}

    impl Name for TwoColumnScan {
        const NAME: &'static str = "TwoColumnScan";
        const PACKAGE: &'static str = "test";

        fn full_name() -> String {
            "test.TwoColumnScan".to_owned()
        }

        fn type_url() -> String {
            "type.googleapis.com/test.TwoColumnScan".to_owned()
        }
    }

    impl Explainable for TwoColumnScan {
        fn name() -> &'static str {
            "TwoColumnScan"
        }

        fn from_args(args: &ExtensionArgs, _ctx: &ExplainContext) -> Result<Self, ExtensionError> {
            let mut extractor = args.extractor();
            extractor.check_exhausted()?;
            // Output columns are validated by the parser; we just ignore them here.
            let _ = &args.output_columns;
            Ok(TwoColumnScan {})
        }

        fn to_args(&self, _ctx: &ExplainContext) -> Result<ExtensionArgs, ExtensionError> {
            let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf);
            args.output_columns.push(ExtensionColumn::Named {
                name: "col0".to_owned(),
                r#type: parse_type("i64"),
            });
            args.output_columns.push(ExtensionColumn::Named {
                name: "col1".to_owned(),
                r#type: parse_type("i32"),
            });
            Ok(args)
        }
    }
}

/// `Project[$0, $1, 42]` over an `ExtensionLeaf:TwoColumnScan` with 2 output columns.
///
/// Without the `get_input_field_count` fix, the literal `42` would be given emit
/// index 0 (wrapping around) instead of 2, producing `[0, 1, 0]` in the proto.
/// With the fix it must be `[0, 1, 2]`.
#[test]
fn test_project_over_extension_leaf_emit_mapping() {
    use substrait::proto::plan_rel;
    use substrait::proto::rel::RelType;
    use substrait::proto::rel_common::EmitKind;

    let mut registry = substrait_explain::extensions::ExtensionRegistry::new();
    registry
        .register_relation::<extension_child_fixture::TwoColumnScan>()
        .expect("register_relation");

    let plan_text = r#"=== Plan
Root[result]
  Project[$0, $1, 42]
    ExtensionLeaf:TwoColumnScan[_ => col0:i64, col1:i32]"#;

    let parser = substrait_explain::parser::Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    // --- proto-level: verify emit mapping on ProjectRel ---
    let plan_rel = plan.relations.first().expect("expected a relation");
    let root = match &plan_rel.rel_type {
        Some(plan_rel::RelType::Root(r)) => r,
        other => panic!("expected Root, got {other:?}"),
    };
    let project = match root.input.as_ref().and_then(|r| r.rel_type.as_ref()) {
        Some(RelType::Project(p)) => p,
        other => panic!("expected Project as Root input, got {other:?}"),
    };
    let emit_kind = project
        .common
        .as_ref()
        .and_then(|c| c.emit_kind.as_ref())
        .expect("ProjectRel must have an emit");
    // Identity mapping [0, 1, 2] over 3 direct outputs → Direct
    assert!(
        matches!(emit_kind, EmitKind::Direct(_)),
        "Expected Direct for identity emit, got {emit_kind:?}"
    );

    // --- round-trip ---
    let (formatted, errors) =
        substrait_explain::format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

// ---------------------------------------------------------------------------
// Enhancement on all remaining standard relation types
//
// Sort, Filter, and Read are covered by earlier tests.  These four verify that
// from_project, from_fetch, from_aggregate, and from_join each correctly
// populate `advanced_extension` on the `Relation` they build, and that
// `Textify for Relation` subsequently emits the line.
// ---------------------------------------------------------------------------

/// Enhancement round-trip on a Project relation.
#[test]
fn test_enhancement_on_project_roundtrip() {
    let registry = make_registry();
    let plan_text = r#"=== Plan
Root[col]
  Project[$0]
    + Enh:PartitionHint[&HASH]
    Read[my.table => col:i64]"#;
    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");
    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

/// Enhancement round-trip on a Fetch relation.
#[test]
fn test_enhancement_on_fetch_roundtrip() {
    let registry = make_registry();
    let plan_text = r#"=== Plan
Root[col]
  Fetch[limit=5 => $0]
    + Enh:PartitionHint[&HASH]
    Read[my.table => col:i64]"#;
    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");
    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

/// Enhancement round-trip on an Aggregate relation.
///
/// Uses a group-by-only aggregate (no measures) to avoid requiring a function
/// like `sum` to be registered in SimpleExtensions.
#[test]
fn test_enhancement_on_aggregate_roundtrip() {
    let registry = make_registry();
    let plan_text = r#"=== Plan
Root[col]
  Aggregate[$0 => $0]
    + Enh:PartitionHint[&HASH]
    Read[my.table => col:i64]"#;
    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");
    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

/// Enhancement round-trip on a Join relation (two child relations).
///
/// Uses a LeftSemi join with a plain field-reference condition to avoid
/// requiring `eq` to be registered in SimpleExtensions.  The enhancement
/// must appear before both child reads in the text format.
#[test]
fn test_enhancement_on_join_roundtrip() {
    let registry = make_registry();
    let plan_text = r#"=== Plan
Root[id, name]
  Join[&LeftSemi, $0 => $0, $1]
    + Enh:PartitionHint[&HASH]
    Read[users => id:i64, name:string]
    Read[orders => user_id:i64, amount:i32]"#;
    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");
    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

/// Optimization on a Read that is a grandchild of Root.
///
/// Before the fix, the `+ Opt:` line was silently dropped when the Read went
/// through `write_children` → `Textify for Relation`.
#[test]
fn test_optimization_on_nested_read_roundtrip() {
    let mut registry = ExtensionRegistry::new();
    registry
        .register_optimization::<opt_fixture::PlanHint>()
        .expect("register_optimization");

    let plan_text = r#"=== Plan
Root[result]
  Filter[$0 => $0]
    Read[my.table => col:i64]
      + Opt:PlanHint[hint='index']"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");
    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

/// Multiple optimizations on a nested Read (grandchild of Root).
///
/// `test_multiple_optimizations_roundtrip` covers the direct-child-of-Root
/// case (which goes through `Textify for Rel`).  This covers depth ≥ 2,
/// where each opt line must survive `write_children` → `Textify for Relation`.
#[test]
fn test_multiple_optimizations_on_nested_read_roundtrip() {
    let mut registry = ExtensionRegistry::new();
    registry
        .register_optimization::<opt_fixture::PlanHint>()
        .expect("register_optimization");

    let plan_text = r#"=== Plan
Root[result]
  Filter[$0 => $0]
    Read[my.table => col:i64]
      + Opt:PlanHint[hint='use_index']
      + Opt:PlanHint[hint='parallel']"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");
    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

/// Enhancement and optimization together on a nested Read.
#[test]
fn test_enhancement_and_optimization_on_nested_relation_roundtrip() {
    let mut registry = make_registry();
    registry
        .register_optimization::<opt_fixture::PlanHint>()
        .expect("register_optimization");

    let plan_text = r#"=== Plan
Root[result]
  Filter[$0 => $0]
    Read[my.table => col:i64]
      + Enh:PartitionHint[&HASH]
      + Opt:PlanHint[hint='index']"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");
    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

/// Enhancement on a Read that is a grandchild of Root (Read → Filter → Root).
#[test]
fn test_enhancement_on_nested_read_roundtrip() {
    let registry = make_registry();

    let plan_text = r#"=== Plan
Root[result]
  Filter[$0 => $0]
    Read[my.table => col:i64]
      + Enh:PartitionHint[&HASH]"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

/// Enhancement on a Filter that is a grandchild of Root (Filter → Sort → Root).
#[test]
fn test_enhancement_on_nested_filter_roundtrip() {
    let registry = make_registry();

    let plan_text = r#"=== Plan
Root[result]
  Sort[($0, &AscNullsFirst) => $0]
    Filter[$0 => $0]
      + Enh:PartitionHint[&RANGE]
      Read[my.table => col:i64]"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

/// Enhancement on a Read at depth 3 from Root (Root → Sort → Filter → Read).
#[test]
fn test_enhancement_depth_three_nesting_roundtrip() {
    let registry = make_registry();
    let plan_text = r#"=== Plan
Root[result]
  Sort[($0, &AscNullsFirst) => $0]
    Filter[$0 => $0]
      Read[my.table => col:i64]
        + Enh:PartitionHint[&RANGE]"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");
    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

// ---------------------------------------------------------------------------
// PartitionStrategy unit tests
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// output_columns in adv_ext → failure token
// ---------------------------------------------------------------------------

/// A synthetic enhancement whose `to_args()` deliberately returns non-empty
/// `output_columns`.  The `adv_extension` grammar has no `=> columns` clause,
/// so formatting this would produce text the parser cannot read back.
/// The formatter must emit a failure token and a `FormatError` instead.
mod adv_ext_with_columns_fixture {
    use prost::Name;
    use substrait_explain::extensions::{
        ExplainContext, Explainable, ExtensionArgs, ExtensionColumn, ExtensionError,
        ExtensionRelationType,
    };

    #[derive(Clone, PartialEq, prost::Message)]
    pub struct EnhancementWithColumns {}

    impl Name for EnhancementWithColumns {
        const NAME: &'static str = "EnhancementWithColumns";
        const PACKAGE: &'static str = "test";

        fn full_name() -> String {
            "test.EnhancementWithColumns".to_owned()
        }

        fn type_url() -> String {
            "type.googleapis.com/test.EnhancementWithColumns".to_owned()
        }
    }

    impl Explainable for EnhancementWithColumns {
        fn name() -> &'static str {
            "EnhancementWithColumns"
        }

        fn from_args(args: &ExtensionArgs, _ctx: &ExplainContext) -> Result<Self, ExtensionError> {
            let mut extractor = args.extractor();
            extractor.check_exhausted()?;
            Ok(EnhancementWithColumns {})
        }

        fn to_args(&self, _ctx: &ExplainContext) -> Result<ExtensionArgs, ExtensionError> {
            let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf);
            // Deliberately populate output_columns — the adv_extension grammar
            // has no "=> columns" clause, so this cannot be round-tripped.
            args.output_columns.push(ExtensionColumn::Named {
                name: "col".to_owned(),
                r#type: parse_type("i64"),
            });
            Ok(args)
        }
    }
}

/// When a registry's `to_args()` returns non-empty `output_columns` for an
/// enhancement, the formatter must emit a failure token rather than writing
/// `+ Enh:Name[args => col:type]`, which the `adv_extension` grammar cannot parse.
#[test]
fn test_adv_ext_output_columns_produces_failure_token() {
    use adv_ext_with_columns_fixture::EnhancementWithColumns;

    let mut registry = ExtensionRegistry::new();
    registry
        .register_enhancement::<EnhancementWithColumns>()
        .expect("register_enhancement");

    let plan_text = r#"=== Plan
Root[result]
  Read[my.table => col:i64]
    + Enh:EnhancementWithColumns[_]"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);

    // The enhancement line must contain a failure token, not "=> col:i64".
    assert!(
        formatted.contains("+ Enh[!{adv_extension}]"),
        "expected failure token '+ Enh[!{{adv_extension}}]' in output, got:\n{formatted}"
    );
    // No adv_extension line may contain "=>" — that syntax is only valid in
    // extension_relation, not in adv_extension.
    let adv_ext_line = formatted
        .lines()
        .find(|l| l.trim_start().starts_with("+ Enh"))
        .expect("expected a '+ Enh' line in output");
    assert!(
        !adv_ext_line.contains("=>"),
        "adv_extension line must not contain '=>', got: {adv_ext_line:?}"
    );

    // Exactly one FormatError must have been collected.
    assert_eq!(
        errors.len(),
        1,
        "expected exactly one FormatError, got: {errors:?}"
    );

    // The error must be a Format error (not an extension-not-found error).
    assert!(
        matches!(&errors[0], FormatError::Format(_)),
        "expected FormatError::Format, got: {:?}",
        errors[0]
    );
}

#[test]
fn test_partition_strategy_str_names_roundtrip() {
    let strategies = [
        PartitionStrategy::Unspecified,
        PartitionStrategy::Hash,
        PartitionStrategy::Range,
        PartitionStrategy::Broadcast,
    ];
    for s in strategies {
        let name = s.as_str_name();
        assert_eq!(PartitionStrategy::from_str_name(name), Some(s));
    }
}
