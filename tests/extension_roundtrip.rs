//! Integration test for custom extension handlers with roundtrip parsing and formatting

use prost::{Message, Name};
use substrait_explain::extensions::{
    Explainable, ExtensionArgs, ExtensionColumn, ExtensionError, ExtensionRegistry,
    ExtensionRelationType, ExtensionValue,
};
use substrait_explain::format_with_registry;
use substrait_explain::parser::{Parser, ParserConfig, UnregisteredExtensionMode};

/// Custom protobuf message for our test extension.
#[derive(Clone, PartialEq, Message)]
pub struct TestScanConfig {
    #[prost(string, tag = "1")]
    pub path: String,
    #[prost(int64, tag = "2")]
    pub batch_size: i64,
    #[prost(string, repeated, tag = "3")]
    pub columns: Vec<String>,
}

// Implement Name trait for protobuf type URL
impl Name for TestScanConfig {
    const NAME: &'static str = "TestScanConfig";
    const PACKAGE: &'static str = "test";

    fn full_name() -> String {
        "test.TestScanConfig".to_string()
    }

    fn type_url() -> String {
        "type.googleapis.com/test.TestScanConfig".to_string()
    }
}

// Implement Explainable for text format conversion
impl Explainable for TestScanConfig {
    fn from_args(args: &ExtensionArgs) -> Result<Self, ExtensionError> {
        let path = match args.get_named_arg("path") {
            Some(ExtensionValue::String(s)) => s.clone(),
            Some(_) => {
                return Err(ExtensionError::InvalidArgument(
                    "path must be a string".to_string(),
                ));
            }
            None => return Err(ExtensionError::MissingArgument("path".to_string())),
        };

        let batch_size = match args.get_named_arg("batch_size") {
            Some(ExtensionValue::Integer(i)) => *i,
            Some(_) => {
                return Err(ExtensionError::InvalidArgument(
                    "batch_size must be an integer".to_string(),
                ));
            }
            None => 1024, // Default value
        };

        // Extract columns from output columns
        let mut columns = Vec::new();
        for col in &args.output_columns {
            match col {
                ExtensionColumn::Named { name, .. } => {
                    columns.push(name.clone());
                }
                _ => {
                    return Err(ExtensionError::InvalidArgument(
                        "Expected named columns only".to_string(),
                    ));
                }
            }
        }

        Ok(TestScanConfig {
            path,
            batch_size,
            columns,
        })
    }

    fn to_args(&self) -> Result<ExtensionArgs, ExtensionError> {
        let mut args =
            ExtensionArgs::new(ExtensionRelationType::Leaf, "ParquetScanConfig".to_string());

        // Add named arguments
        args.add_named_arg(
            "path".to_string(),
            ExtensionValue::String(self.path.clone()),
        );
        args.add_named_arg(
            "batch_size".to_string(),
            ExtensionValue::Integer(self.batch_size),
        );

        // Add output columns
        for column in &self.columns {
            args.add_output_column(ExtensionColumn::Named {
                name: column.clone(),
                type_spec: "string".to_string(), // Simplified for test
            });
        }

        Ok(args)
    }

    /// Specify preferred argument order: path first, then batch_size
    fn argument_order() -> Vec<String> {
        vec!["path".to_string(), "batch_size".to_string()]
    }
}

#[test]
fn test_extension_leaf_roundtrip() {
    // Create and populate extension registry
    let mut registry = ExtensionRegistry::new();
    registry.register::<TestScanConfig>("TestScan");

    // Test plan with custom extension
    let plan_text = r#"
=== Plan
Root[result]
  ExtensionLeaf:TestScan[path='data/test.parquet', batch_size=2048 => id:string, name:string, value:string]
"#;

    // Parse with registry
    let parser = Parser::new().with_extension_registry(registry.clone());
    let (plan, warnings) = parser.parse_plan(plan_text).expect("Failed to parse plan");
    assert!(warnings.is_empty(), "Unexpected warnings: {warnings:?}");

    // Format back to text with registry
    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "Unexpected errors: {errors:?}");

    // Validate that the formatted output matches the input
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

    // Parse with warning mode - should succeed with warnings
    let config = ParserConfig {
        unregistered_extension_mode: UnregisteredExtensionMode::Warn,
    };
    let parser = Parser::with_config(config);
    let (plan, warnings) = parser.parse_plan(plan_text).expect("Failed to parse plan");
    assert!(!warnings.is_empty());
    assert!(format!("{:?}", warnings[0]).contains("UnknownExtension"));

    // Format without registry - should produce error placeholder
    let empty_registry = ExtensionRegistry::default();
    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &empty_registry);

    // Should have formatting errors for unregistered extension
    assert!(!errors.is_empty());
    assert!(
        errors[0]
            .to_string()
            .contains("Extension detail is missing")
    );

    // Should fall back to generic ExtensionLeaf formatting with error placeholder
    assert!(formatted.contains("ExtensionLeaf[!{extension}]"));
}

#[test]
fn test_multiple_extensions_in_plan() {
    // Create registry with multiple extension types
    let mut registry = ExtensionRegistry::new();
    registry.register::<TestScanConfig>("TestScan");

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
        fn from_args(args: &ExtensionArgs) -> Result<Self, ExtensionError> {
            let expression = match args.get_named_arg("expr") {
                Some(ExtensionValue::String(s)) => s.clone(),
                Some(_) => {
                    return Err(ExtensionError::InvalidArgument(
                        "expr must be a string".to_string(),
                    ));
                }
                None => return Err(ExtensionError::MissingArgument("expr".to_string())),
            };

            Ok(FilterConfig { expression })
        }

        fn to_args(&self) -> Result<ExtensionArgs, ExtensionError> {
            let mut args = ExtensionArgs::new(
                ExtensionRelationType::Single,
                "TestFilterConfig".to_string(),
            );
            args.add_named_arg(
                "expr".to_string(),
                ExtensionValue::String(self.expression.clone()),
            );
            Ok(args)
        }
    }

    registry.register::<FilterConfig>("TestFilter");

    // Plan with multiple extension types
    let plan_text = r#"
=== Plan
Root[result]
  ExtensionSingle:TestFilter[expr='value > 100' => $0, $1, $2]
    ExtensionLeaf:TestScan[path='data/test.parquet', batch_size=1024 => id:string, name:string, value:i64]
"#;

    // Parse with registry
    let parser = Parser::new().with_extension_registry(registry.clone());
    let (plan, warnings) = parser.parse_plan(plan_text).expect("Failed to parse plan");
    assert!(warnings.is_empty());

    // Verify both extensions were parsed
    assert_eq!(plan.relations.len(), 1);
}
