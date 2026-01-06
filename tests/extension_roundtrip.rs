//! Integration test for custom extension handlers with roundtrip parsing and formatting

use prost::{Message, Name};
use substrait_explain::extensions::{
    Explainable, ExtensionArgs, ExtensionColumn, ExtensionError, ExtensionRegistry,
    ExtensionRelationType, ExtensionValue,
};
use substrait_explain::format_with_registry;
use substrait_explain::parser::Parser;

/// A custom extension configuration for a hypothetical "UserTable" data source.
/// This differs from the file-based scan in the example by using logical table properties.
#[derive(Clone, PartialEq, Message)]
pub struct UserTableConfig {
    #[prost(string, tag = "1")]
    pub table_name: String,
    #[prost(int64, tag = "2")]
    pub version: i64,
    #[prost(bool, tag = "3")]
    pub is_temporary: bool,
    #[prost(string, repeated, tag = "4")]
    pub tracked_columns: Vec<String>,
}

// Implement Name trait for protobuf type URL
impl Name for UserTableConfig {
    const NAME: &'static str = "UserTableConfig";
    const PACKAGE: &'static str = "test";

    fn full_name() -> String {
        "test.UserTableConfig".to_string()
    }

    fn type_url() -> String {
        "type.googleapis.com/test.UserTableConfig".to_string()
    }
}

// Implement Explainable for text format conversion
impl Explainable for UserTableConfig {
    fn name() -> &'static str {
        "UserTable"
    }

    fn from_args(args: &ExtensionArgs) -> Result<Self, ExtensionError> {
        let mut extractor = args.extractor();
        let table_name: &str = extractor.expect_named_arg("name")?;
        let version: i64 = extractor.get_named_or("version", 1)?;
        let is_temporary: bool = extractor.get_named_or("temp", false)?;

        extractor.check_exhausted()?;

        // Extract columns from output columns to populate tracked_columns
        let mut tracked_columns = Vec::new();
        for col in &args.output_columns {
            match col {
                ExtensionColumn::Named { name, .. } => {
                    tracked_columns.push(name.clone());
                }
                _ => {
                    return Err(ExtensionError::InvalidArgument(
                        "Expected named columns only".to_string(),
                    ));
                }
            }
        }

        Ok(UserTableConfig {
            table_name: table_name.to_string(),
            version,
            is_temporary,
            tracked_columns,
        })
    }

    fn to_args(&self) -> Result<ExtensionArgs, ExtensionError> {
        let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf);

        // Add named arguments
        args.add_named_arg(
            "name".to_string(),
            ExtensionValue::String(self.table_name.clone()),
        );
        args.add_named_arg("version".to_string(), ExtensionValue::Integer(self.version));
        args.add_named_arg(
            "temp".to_string(),
            ExtensionValue::Boolean(self.is_temporary),
        );

        // Add output columns
        for column in &self.tracked_columns {
            args.add_output_column(ExtensionColumn::Named {
                name: column.clone(),
                type_spec: "string".to_string(), // Simplified for test
            });
        }

        Ok(args)
    }

    /// Specify preferred argument order
    fn argument_order() -> Vec<String> {
        vec![
            "name".to_string(),
            "version".to_string(),
            "temp".to_string(),
        ]
    }
}

#[test]
fn test_extension_leaf_roundtrip() {
    let mut registry = ExtensionRegistry::new();
    registry.register_relation::<UserTableConfig>();

    // Test plan with UserTable extension
    let plan_text = r#"
=== Plan
Root[result]
  ExtensionLeaf:UserTable[name='customers', version=2, temp=true => id:string, region:string]
"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("Failed to parse plan");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "Unexpected errors: {errors:?}");

    assert_eq!(formatted.trim(), plan_text.trim());
}

#[test]
fn test_extension_without_registry() {
    // Test that extensions work in different modes when registry is not available
    let plan_text = r#"
=== Plan
Root[result]
  ExtensionLeaf:UnknownExtension[arg1='value1', arg2=42 => col1:i32, col2:string]
"#;

    // Parse without registry - should fail by default
    let parser = Parser::default();
    let result = parser.parse_plan(plan_text);
    assert!(result.is_err());
}

#[test]
fn test_multiple_extensions_in_plan() {
    let mut registry = ExtensionRegistry::new();
    registry.register_relation::<UserTableConfig>();

    // Also register a second type for variety
    #[derive(Clone, PartialEq, Message)]
    pub struct FilterConfig {
        #[prost(string, tag = "1")]
        pub expression: String,
    }

    impl Name for FilterConfig {
        const NAME: &'static str = "FilterConfig";
        const PACKAGE: &'static str = "test";

        fn full_name() -> String {
            "test.FilterConfig".to_string()
        }

        fn type_url() -> String {
            "type.googleapis.com/test.FilterConfig".to_string()
        }
    }

    impl Explainable for FilterConfig {
        fn name() -> &'static str {
            "TestFilter"
        }

        fn from_args(args: &ExtensionArgs) -> Result<Self, ExtensionError> {
            let mut extractor = args.extractor();
            let expression: String = extractor.expect_named_arg::<&str>("expr")?.to_string();
            extractor.check_exhausted()?;

            Ok(FilterConfig { expression })
        }

        fn to_args(&self) -> Result<ExtensionArgs, ExtensionError> {
            let mut args = ExtensionArgs::new(ExtensionRelationType::Single);
            args.add_named_arg(
                "expr".to_string(),
                ExtensionValue::String(self.expression.clone()),
            );
            Ok(args)
        }
    }

    registry.register_relation::<FilterConfig>();

    // Plan with multiple extension types
    let plan_text = r#"
=== Plan
Root[result]
  ExtensionSingle:TestFilter[expr='status = "active"' => $0, $1]
    ExtensionLeaf:UserTable[name='users_prod', version=1, temp=false => id:i64, status:string]
"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("Failed to parse plan");

    assert_eq!(plan.relations.len(), 1);
}

/// Test-only protobuf payload used to verify literal argument round-tripping for
/// extensions. Holds a representative mixture of scalar literal types that the
/// text format should preserve without truncation or allocation leaks.
#[derive(Clone, PartialEq, Message)]
pub struct LiteralConfig {
    #[prost(string, tag = "1")]
    pub path: String,
    #[prost(int64, tag = "2")]
    pub big: i64,
    #[prost(double, tag = "3")]
    pub ratio: f64,
    #[prost(bool, tag = "4")]
    pub enabled: bool,
}

impl Name for LiteralConfig {
    const NAME: &'static str = "LiteralConfig";
    const PACKAGE: &'static str = "test";

    fn full_name() -> String {
        "test.LiteralConfig".to_string()
    }

    fn type_url() -> String {
        "type.googleapis.com/test.LiteralConfig".to_string()
    }
}

impl Explainable for LiteralConfig {
    fn name() -> &'static str {
        "LiteralTest"
    }

    fn from_args(args: &ExtensionArgs) -> Result<Self, ExtensionError> {
        let mut extractor = args.extractor();
        let path: String = extractor.expect_named_arg::<&str>("path")?.to_string();
        let big: i64 = extractor.expect_named_arg("big")?;

        // Manually handle ratio to support both Integer and Float types
        let ratio = match extractor.get_named_arg("ratio") {
            Some(ExtensionValue::Float(f)) => *f,
            Some(ExtensionValue::Integer(i)) => *i as f64,
            Some(v) => {
                return Err(ExtensionError::InvalidArgument(format!(
                    "ratio must be a float, got {v}"
                )));
            }
            None => return Err(ExtensionError::MissingArgument("ratio".to_string())),
        };

        let enabled: bool = extractor.expect_named_arg("enabled")?;

        extractor.check_exhausted()?;

        Ok(LiteralConfig {
            path,
            big,
            ratio,
            enabled,
        })
    }

    fn to_args(&self) -> Result<ExtensionArgs, ExtensionError> {
        let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf);
        args.add_named_arg(
            "path".to_string(),
            ExtensionValue::String(self.path.clone()),
        );
        args.add_named_arg("big".to_string(), ExtensionValue::Integer(self.big));
        args.add_named_arg("ratio".to_string(), ExtensionValue::Float(self.ratio));
        args.add_named_arg("enabled".to_string(), ExtensionValue::Boolean(self.enabled));
        args.add_output_column(ExtensionColumn::Named {
            name: "value".to_string(),
            type_spec: "string".to_string(),
        });
        Ok(args)
    }

    fn argument_order() -> Vec<String> {
        vec![
            "path".to_string(),
            "big".to_string(),
            "ratio".to_string(),
            "enabled".to_string(),
        ]
    }
}

#[test]
fn test_extension_literal_roundtrip() {
    let mut registry = ExtensionRegistry::new();
    registry.register_relation::<LiteralConfig>();

    let plan_text = r#"
=== Plan
Root[result]
  ExtensionLeaf:LiteralTest[path='data/source', big=1099511627776, ratio=3.25, enabled=false => value:string]
"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("Failed to parse plan");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "Unexpected errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

#[test]
fn test_extension_unknown_arguments() {
    let mut registry = ExtensionRegistry::new();
    registry.register_relation::<UserTableConfig>();

    // Test plan with unknown argument 'invalid_arg'
    let plan_text = r#"
=== Plan
Root[result]
  ExtensionLeaf:UserTable[name='customers', version=2, invalid_arg=true => id:string]
"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let result = parser.parse_plan(plan_text);

    // Should fail during parsing when it tries to convert args
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.to_string()
            .contains("Unknown named arguments: invalid_arg")
    );
}
