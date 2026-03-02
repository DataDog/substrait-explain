//! Integration tests for advanced extension (Enh/Opt) round-trip.
//!
//! Tests parse → textify round-trips for `+ Enh:` and `+ Opt:` annotations
//! on standard relations, using [`ReadRelEnhancement`] as the concrete example.

use substrait_explain::extensions::{ExtensionRegistry, ReadRelEnhancement};
use substrait_explain::format_with_registry;
use substrait_explain::parser::Parser;

fn make_registry() -> ExtensionRegistry {
    let mut registry = ExtensionRegistry::new();
    registry
        .register_enhancement::<ReadRelEnhancement>()
        .expect("register_enhancement");
    registry
}

// ---------------------------------------------------------------------------
// Round-trip: parse → textify → compare
// ---------------------------------------------------------------------------

#[test]
fn test_read_rel_enhancement_roundtrip() {
    let registry = make_registry();

    let plan_text = r#"=== Plan
Root[result]
  Read[my.table => col:i64]
    + Enh:ReadRelEnhancement[&CORE, &CUSTOM]"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

#[test]
fn test_read_rel_enhancement_all_namespaces() {
    let registry = make_registry();

    let plan_text = r#"=== Plan
Root[result]
  Read[my.table => col:i64]
    + Enh:ReadRelEnhancement[&UNKNOWN, &CORE, &CUSTOM, &TAG]"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

#[test]
fn test_read_rel_enhancement_empty_namespaces() {
    let registry = make_registry();

    // Zero namespaces → args render as `_`
    let plan_text = r#"=== Plan
Root[result]
  Read[my.table => col:i64]
    + Enh:ReadRelEnhancement[_]"#;

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

    // The enhancement annotates the Filter; its Read child appears before
    // the annotation line in the textified output.
    let plan_text = r#"=== Plan
Root[result]
  Filter[$0 => $0]
    Read[my.table => col:i64]
    + Enh:ReadRelEnhancement[&CORE]"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

// ---------------------------------------------------------------------------
// Multiple optimizations
// ---------------------------------------------------------------------------

/// A minimal Explainable + prost::Message used to register as an optimization.
mod opt_fixture {
    use prost::Name;
    use substrait_explain::extensions::{
        Explainable, ExtensionArgs, ExtensionError, ExtensionRelationType,
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

        fn from_args(args: &ExtensionArgs) -> Result<Self, ExtensionError> {
            let mut extractor = args.extractor();
            let hint: String = extractor.expect_named_arg::<&str>("hint")?.to_owned();
            extractor.check_exhausted()?;
            Ok(PlanHint { hint })
        }

        fn to_args(&self) -> Result<ExtensionArgs, ExtensionError> {
            let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf);
            args.named.insert(
                "hint".to_owned(),
                substrait_explain::extensions::ExtensionValue::String(self.hint.clone()),
            );
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

    // Enhancement comes before optimizations in textified output
    let plan_text = r#"=== Plan
Root[result]
  Read[my.table => col:i64]
    + Enh:ReadRelEnhancement[&CORE]
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
    // Empty registry — ReadRelEnhancement is unknown
    let registry = ExtensionRegistry::new();

    let plan_text = r#"
=== Plan
Root[result]
  Read[my.table => col:i64]
    + Enh:ReadRelEnhancement[&CORE, &CUSTOM]
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
    // Parse with a registry that knows ReadRelEnhancement
    let registry = make_registry();
    let parser = Parser::new().with_extension_registry(registry.clone());

    let plan_text = r#"=== Plan
Root[result]
  Read[my.table => col:i64]
    + Enh:ReadRelEnhancement[&CORE]"#;

    let plan = parser.parse_plan(plan_text).expect("parse failed");

    // Format with an empty registry — the type URL is no longer known
    let empty_registry = ExtensionRegistry::new();
    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &empty_registry);

    // Output is still produced (lenient) but contains at least one error
    assert!(
        !formatted.is_empty(),
        "expected non-empty output even on decode error"
    );
    assert!(
        !errors.is_empty(),
        "expected at least one format error for unknown enhancement type URL"
    );
    // The enhancement line should still appear in some form
    assert!(
        formatted.contains("+ Enh["),
        "expected enhancement prefix in output, got:\n{formatted}"
    );
}
