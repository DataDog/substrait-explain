//! Integration tests for `Read:Extension` and `+ Ext` parse rules.

use prost::{Message, Name};
use substrait_explain::extensions::{
    Explainable, ExtensionArgs, ExtensionError, ExtensionRegistry, examples,
};
use substrait_explain::{Parser, format_with_registry};

#[derive(Clone, PartialEq, Message)]
struct UserTable {
    #[prost(string, tag = "1")]
    name: String,
}

impl Name for UserTable {
    const NAME: &'static str = "UserTable";
    const PACKAGE: &'static str = "test";

    fn full_name() -> String {
        "test.UserTable".to_string()
    }

    fn type_url() -> String {
        "type.googleapis.com/test.UserTable".to_string()
    }
}

impl Explainable for UserTable {
    fn name() -> &'static str {
        "UserTable"
    }

    fn from_args(args: &ExtensionArgs) -> Result<Self, ExtensionError> {
        let mut extractor = args.extractor();
        let name: &str = extractor.expect_named_arg("name")?;

        extractor.check_exhausted()?;

        Ok(Self {
            name: name.to_string(),
        })
    }

    fn to_args(&self) -> Result<ExtensionArgs, ExtensionError> {
        let mut args = ExtensionArgs::default();
        args.insert("name", self.name.clone());
        Ok(args)
    }
}

fn registry() -> ExtensionRegistry {
    let mut registry = ExtensionRegistry::new();
    registry.register_extension_table::<UserTable>().unwrap();
    registry
}

fn registry_with_relation() -> ExtensionRegistry {
    let mut registry = registry();
    registry.register_relation::<UserTable>().unwrap();
    registry
}

fn assert_parse_error(registry: ExtensionRegistry, plan_text: &str, expected: &str) {
    let parser = Parser::new().with_extension_registry(registry);
    let result = parser.parse_plan(plan_text);
    match result {
        Ok(_) => panic!("expected parse error containing {expected:?}, but parse succeeded"),
        Err(error) => {
            let message = error.to_string();
            assert!(
                message.contains(expected),
                "expected parse error to contain {expected:?}, got: {message}"
            );
        }
    }
}

#[test]
fn read_roundtrip() {
    let registry = examples::registry();

    let plan_text = r#"
=== Plan
Root[id, payload]
  Read:Extension[id:i64, payload:string]
    + Ext:BlobStoreRead['path/to/file', limit=100, include_archived=true]
"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");

    assert_eq!(formatted.trim(), plan_text.trim());
}

#[test]
fn read_with_advanced_extensions_roundtrip() {
    let registry = examples::registry();

    let plan_text = r#"
=== Plan
Root[id]
  Read:Extension[id:i64]
    + Ext:BlobStoreRead['path/to/file']
    + Enh:PartitionHint[&HASH, count=8]
    + Opt:PlanHint[hint='parallel']
"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");

    assert_eq!(formatted.trim(), plan_text.trim());
}

#[test]
fn missing_ext_fails() {
    let plan_text = r#"
=== Plan
Root[id]
  Read:Extension[id:i64]
"#;

    assert_parse_error(
        registry(),
        plan_text,
        "Read:Extension requires exactly one + Ext addendum",
    );
}

#[test]
fn duplicate_ext_fails() {
    let plan_text = r#"
=== Plan
Root[id]
  Read:Extension[id:i64]
    + Ext:UserTable[name='customers']
    + Ext:UserTable[name='other_customers']
"#;

    assert_parse_error(
        registry(),
        plan_text,
        "Read:Extension allows exactly one + Ext addendum",
    );
}

#[test]
fn ext_on_named_read_fails() {
    let plan_text = r#"
=== Plan
Root[id]
  Read[data => id:i64]
    + Ext:UserTable[name='customers']
"#;

    assert_parse_error(
        registry(),
        plan_text,
        "+ Ext addenda can only be used with Read:Extension",
    );
}

#[test]
fn ext_on_relation_fails() {
    let plan_text = r#"
=== Plan
Root[id]
  ExtensionLeaf:UserTable[name='customers' => id:string]
    + Ext:UserTable[name='other_customers']
"#;

    assert_parse_error(
        registry_with_relation(),
        plan_text,
        "extension relations do not support addenda",
    );
}
