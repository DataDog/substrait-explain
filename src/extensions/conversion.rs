//! Conversion utilities between parsed extension arguments and registry types.
//!
//! This module provides helpers to convert between structured extension values
//! and text format fragments used by the formatter.

use crate::extensions::{ExtensionArgs, ExtensionColumn, ExtensionValue};

/// Convert ExtensionArgs back to text format for textification
pub fn format_extension_args_to_text(args: &ExtensionArgs, extension_name: &str) -> String {
    let mut parts = Vec::new();

    // Add extension name
    parts.push(extension_name.to_string());
    parts.push("[".to_string());

    let mut inner_parts = Vec::new();

    // Add positional arguments
    for value in &args.positional {
        inner_parts.push(format_extension_value_to_text(value));
    }

    // Add named arguments
    for (name, value) in &args.named {
        inner_parts.push(format!(
            "{}={}",
            name,
            format_extension_value_to_text(value)
        ));
    }

    // Join the arguments
    let args_str = inner_parts.join(", ");

    // Add output columns if present
    if !args.output_columns.is_empty() {
        let columns_str = args
            .output_columns
            .iter()
            .map(format_extension_column_to_text)
            .collect::<Vec<_>>()
            .join(", ");

        if args_str.is_empty() {
            parts.push(format!("=> {columns_str}"));
        } else {
            parts.push(format!("{args_str} => {columns_str}"));
        }
    } else {
        parts.push(args_str);
    }

    parts.push("]".to_string());

    parts.join("")
}

/// Format an ExtensionValue to text
fn format_extension_value_to_text(value: &ExtensionValue) -> String {
    match value {
        ExtensionValue::String(s) => format!("'{s}'"), // Add quotes for strings
        ExtensionValue::Integer(i) => i.to_string(),
        ExtensionValue::Float(f) => f.to_string(),
        ExtensionValue::Boolean(b) => b.to_string(),
        ExtensionValue::Reference(r) => format!("${r}"),
        ExtensionValue::Expression(e) => e.to_string(),
    }
}

/// Format an ExtensionColumn to text
fn format_extension_column_to_text(column: &ExtensionColumn) -> String {
    match column {
        ExtensionColumn::Named { name, type_spec } => format!("{name}:{type_spec}"),
        ExtensionColumn::Reference(r) => format!("${r}"),
        ExtensionColumn::Expression(e) => e.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::extensions::ExtensionRelationType;
    use crate::parser::{ParseError, Parser};

    #[test]
    fn test_format_extension_args() {
        let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf);
        args.add_positional_arg(ExtensionValue::Reference(42));
        args.add_named_arg(
            "path".to_string(),
            ExtensionValue::String("data/*.parquet".to_string()),
        );
        args.add_named_arg("batch_size".to_string(), ExtensionValue::Integer(1024));
        args.add_output_column(ExtensionColumn::Named {
            name: "col1".to_string(),
            type_spec: "i32".to_string(),
        });

        let text = format_extension_args_to_text(&args, "ExtensionLeaf:ParquetScan");
        assert!(text.contains("$42"));
        assert!(text.contains("path='data/*.parquet'"));
        assert!(text.contains("batch_size=1024"));
        assert!(text.contains("=> col1:i32"));
    }

    #[test]
    fn test_integration_extension_missing_registry_errors() {
        // Test a complete plan with extension that has unregistered extension
        let plan_text = r#"
=== Extensions
URNs:
  @  1: https://example.com/test
Functions:
  # 10 @  1: test_extension

=== Plan
Root[result]
  ExtensionLeaf:TestExtension[$5, test_arg='hello' => col1:i32]
"#;

        let result = Parser::parse(plan_text);

        match result {
            Err(ParseError::UnregisteredExtension { name, .. }) => {
                assert_eq!(name, "TestExtension");
            }
            Err(e) => panic!("Expected UnregisteredExtension error, got {e}"),
            Ok(_) => panic!("Expected parsing to fail without a registry"),
        }
    }
}
