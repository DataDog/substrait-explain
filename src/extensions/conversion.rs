//! Conversion utilities between Pest grammar and extension registry types
//!
//! This module provides functions to convert between the parsed Pest grammar
//! elements (from the text format) and the structured types used by the
//! extension registry system.

use pest::iterators::Pair;

use crate::extensions::SimpleExtensions;
use crate::extensions::registry::{ExtensionArgs, ExtensionColumn, ExtensionError, ExtensionValue};
use crate::parser::Rule;
use crate::parser::common::RuleIter;
use crate::parser::warnings::ParseWarning;

/// Convert parsed extension arguments from Pest grammar to ExtensionArgs
pub fn parse_extension_args_from_pest(
    pair: Pair<Rule>,
    _extensions: &SimpleExtensions,
) -> Result<ExtensionArgs, ExtensionError> {
    assert_eq!(pair.as_rule(), Rule::extension_relation);

    let mut args = ExtensionArgs::new();
    let mut iter = RuleIter::from(pair.into_inner());

    // Skip the extension name (already parsed by caller)
    let _extension_name_pair = iter.pop(Rule::extension_name);

    // Parse positional arguments if present
    if let Some(extension_arguments_pair) = iter.try_pop(Rule::extension_arguments) {
        for arg_pair in extension_arguments_pair.into_inner() {
            if arg_pair.as_rule() == Rule::extension_argument {
                let value = parse_extension_value_from_pest(arg_pair)?;
                args.add_positional_arg(value);
            }
        }
    }

    // Parse named arguments if present
    if let Some(extension_named_arguments_pair) = iter.try_pop(Rule::extension_named_arguments) {
        for named_arg_pair in extension_named_arguments_pair.into_inner() {
            if named_arg_pair.as_rule() == Rule::extension_named_argument {
                let (name, value) = parse_extension_named_arg_from_pest(named_arg_pair)?;
                args.add_named_arg(name, value);
            }
        }
    }

    // Parse output columns if present
    if let Some(extension_columns_pair) = iter.try_pop(Rule::extension_columns) {
        for column_pair in extension_columns_pair.into_inner() {
            if column_pair.as_rule() == Rule::extension_column {
                let column = parse_extension_column_from_pest(column_pair)?;
                args.add_output_column(column);
            }
        }
    }

    iter.done();
    Ok(args)
}

/// Parse a single extension value from a Pest pair
fn parse_extension_value_from_pest(pair: Pair<Rule>) -> Result<ExtensionValue, ExtensionError> {
    assert_eq!(pair.as_rule(), Rule::extension_argument);

    // Get the inner pair (reference, literal, or expression)
    let inner_pair = pair
        .into_inner()
        .next()
        .ok_or_else(|| ExtensionError::ParseError("Empty extension argument".to_string()))?;

    match inner_pair.as_rule() {
        Rule::reference => {
            // Parse reference like $0, $1, etc.
            let ref_str = inner_pair.as_str();
            if let Some(index_str) = ref_str.strip_prefix('$') {
                let index = index_str.parse::<i32>().map_err(|_| {
                    ExtensionError::ParseError(format!("Invalid reference: {ref_str}"))
                })?;
                // Note: Field reference bounds checking is performed later when we have
                // the full context including input_field_count. This is handled in the
                // relation parsing phase where warnings can be collected.
                Ok(ExtensionValue::Reference(index))
            } else {
                Err(ExtensionError::ParseError(format!(
                    "Invalid reference format: {ref_str}"
                )))
            }
        }
        Rule::literal => {
            // Parse literal values
            parse_literal_value_from_pest(inner_pair)
        }
        Rule::expression => {
            // For now, store expressions as strings
            // In a more sophisticated implementation, this could be parsed into a structured expression type
            Ok(ExtensionValue::Expression(inner_pair.as_str().to_string()))
        }
        _ => Err(ExtensionError::ParseError(format!(
            "Unsupported extension argument type: {:?}",
            inner_pair.as_rule()
        ))),
    }
}

/// Parse a literal value from a Pest pair
fn parse_literal_value_from_pest(pair: Pair<Rule>) -> Result<ExtensionValue, ExtensionError> {
    assert_eq!(pair.as_rule(), Rule::literal);

    // Get the inner pair (the actual literal type)
    let inner_pair = pair
        .into_inner()
        .next()
        .ok_or_else(|| ExtensionError::ParseError("Empty literal".to_string()))?;

    match inner_pair.as_rule() {
        Rule::string_literal => {
            // Remove the surrounding quotes and handle escape sequences
            let str_with_quotes = inner_pair.as_str();
            if str_with_quotes.len() >= 2 {
                let unquoted = &str_with_quotes[1..str_with_quotes.len() - 1]; // Remove quotes
                // TODO: Properly handle escape sequences
                Ok(ExtensionValue::String(unquoted.to_string()))
            } else {
                Err(ExtensionError::ParseError(
                    "Invalid string literal".to_string(),
                ))
            }
        }
        Rule::integer => {
            let int_str = inner_pair.as_str();
            let value = int_str
                .parse::<i64>()
                .map_err(|_| ExtensionError::ParseError(format!("Invalid integer: {int_str}")))?;
            Ok(ExtensionValue::Integer(value))
        }
        Rule::float => {
            let float_str = inner_pair.as_str();
            let value = float_str
                .parse::<f64>()
                .map_err(|_| ExtensionError::ParseError(format!("Invalid float: {float_str}")))?;
            Ok(ExtensionValue::Float(value))
        }
        Rule::boolean => {
            let bool_str = inner_pair.as_str();
            let value = bool_str
                .parse::<bool>()
                .map_err(|_| ExtensionError::ParseError(format!("Invalid boolean: {bool_str}")))?;
            Ok(ExtensionValue::Boolean(value))
        }
        _ => Err(ExtensionError::ParseError(format!(
            "Unsupported literal type: {:?}",
            inner_pair.as_rule()
        ))),
    }
}

/// Parse a named argument (name=value pair) from a Pest pair
fn parse_extension_named_arg_from_pest(
    pair: Pair<Rule>,
) -> Result<(String, ExtensionValue), ExtensionError> {
    assert_eq!(pair.as_rule(), Rule::extension_named_argument);

    let mut iter = pair.into_inner();

    // Get the name
    let name_pair = iter
        .next()
        .ok_or_else(|| ExtensionError::ParseError("Missing argument name".to_string()))?;
    let name = match name_pair.as_rule() {
        Rule::name => name_pair.as_str().to_string(),
        _ => {
            return Err(ExtensionError::ParseError(
                "Invalid argument name format".to_string(),
            ));
        }
    };

    // Get the value
    let value_pair = iter
        .next()
        .ok_or_else(|| ExtensionError::ParseError("Missing argument value".to_string()))?;
    let value = parse_extension_value_from_pest(value_pair)?;

    Ok((name, value))
}

/// Parse an extension column from a Pest pair
fn parse_extension_column_from_pest(pair: Pair<Rule>) -> Result<ExtensionColumn, ExtensionError> {
    assert_eq!(pair.as_rule(), Rule::extension_column);

    // Get the inner pair (named_column, reference, or expression)
    let inner_pair = pair
        .into_inner()
        .next()
        .ok_or_else(|| ExtensionError::ParseError("Empty extension column".to_string()))?;

    match inner_pair.as_rule() {
        Rule::named_column => {
            // Parse "name:type" format
            let mut iter = inner_pair.into_inner();
            let name_pair = iter
                .next()
                .ok_or_else(|| ExtensionError::ParseError("Missing column name".to_string()))?;
            let type_pair = iter
                .next()
                .ok_or_else(|| ExtensionError::ParseError("Missing column type".to_string()))?;

            let name = name_pair.as_str().to_string();
            let type_spec = type_pair.as_str().to_string();

            Ok(ExtensionColumn::Named { name, type_spec })
        }
        Rule::reference => {
            // Parse reference like $0, $1, etc.
            let ref_str = inner_pair.as_str();
            if let Some(index_str) = ref_str.strip_prefix('$') {
                let index = index_str.parse::<i32>().map_err(|_| {
                    ExtensionError::ParseError(format!("Invalid reference: {ref_str}"))
                })?;
                Ok(ExtensionColumn::Reference(index))
            } else {
                Err(ExtensionError::ParseError(format!(
                    "Invalid reference format: {ref_str}"
                )))
            }
        }
        Rule::expression => {
            // For now, store expressions as strings
            Ok(ExtensionColumn::Expression(inner_pair.as_str().to_string()))
        }
        _ => Err(ExtensionError::ParseError(format!(
            "Unsupported extension column type: {:?}",
            inner_pair.as_rule()
        ))),
    }
}

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

    // Add output columns if present
    if !args.output_columns.is_empty() {
        // Add the arrow with the first column
        let first_column = format_extension_column_to_text(&args.output_columns[0]);
        inner_parts.push(format!("=> {first_column}"));

        // Add remaining columns
        for column in &args.output_columns[1..] {
            inner_parts.push(format_extension_column_to_text(column));
        }
    }

    parts.push(inner_parts.join(", "));
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
        ExtensionValue::Expression(e) => e.clone(),
    }
}

/// Format an ExtensionColumn to text
fn format_extension_column_to_text(column: &ExtensionColumn) -> String {
    match column {
        ExtensionColumn::Named { name, type_spec } => format!("{name}:{type_spec}"),
        ExtensionColumn::Reference(r) => format!("${r}"),
        ExtensionColumn::Expression(e) => e.clone(),
    }
}

/// Validate field references in extension arguments and collect warnings for out-of-bounds references
pub fn validate_extension_field_references(
    args: &ExtensionArgs,
    input_field_count: usize,
    extension_name: &str,
    line_no: i64,
    warnings: &mut Vec<ParseWarning>,
) {
    let mut validate_ref = |index: i32, context: &str| {
        if index < 0 || index >= input_field_count as i32 {
            let max_index = if input_field_count > 0 {
                input_field_count - 1
            } else {
                0
            };
            warnings.push(ParseWarning::out_of_bounds_field_reference(
                format!("${index}"),
                max_index,
                format!("{context} in extension {extension_name}"),
                line_no,
                "extension relation".to_string(),
            ));
        }
    };

    // Check positional arguments
    for (i, value) in args.positional.iter().enumerate() {
        if let ExtensionValue::Reference(index) = value {
            validate_ref(*index, &format!("positional argument {i}"));
        }
    }

    // Check named arguments
    for (name, value) in &args.named {
        if let ExtensionValue::Reference(index) = value {
            validate_ref(*index, &format!("named argument '{name}'"));
        }
    }

    // Check output columns
    for (i, column) in args.output_columns.iter().enumerate() {
        if let ExtensionColumn::Reference(index) = column {
            validate_ref(*index, &format!("output column {i}"));
        }
    }
}

#[cfg(test)]
mod tests {
    use pest::Parser;

    use super::*;
    use crate::parser::ExpressionParser;

    #[test]
    fn test_parse_extension_value_reference() {
        let input = "$42";
        let pairs = ExpressionParser::parse(Rule::reference, input).unwrap();
        let _pair = pairs.into_iter().next().unwrap();

        // Wrap in extension_argument rule for the parser
        let extension_arg_input = "$42";
        let extension_pairs =
            ExpressionParser::parse(Rule::extension_argument, extension_arg_input).unwrap();
        let extension_pair = extension_pairs.into_iter().next().unwrap();

        let value = parse_extension_value_from_pest(extension_pair).unwrap();
        match value {
            ExtensionValue::Reference(42) => {} // Expected
            other => panic!("Expected Reference(42), got {other:?}"),
        }
    }

    #[test]
    fn test_format_extension_args() {
        let mut args = ExtensionArgs::new();
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
        assert!(text.contains("path='data/*.parquet'"));
        assert!(text.contains("batch_size=1024"));
        assert!(text.contains("=> col1:i32"));
    }

    #[test]
    fn test_validate_extension_field_references_out_of_bounds() {
        let mut args = ExtensionArgs::new();
        // Add positional argument with out-of-bounds reference
        args.add_positional_arg(ExtensionValue::Reference(5)); // input_field_count will be 2, so this is out of bounds
        // Add named argument with valid reference
        args.add_named_arg("valid_ref".to_string(), ExtensionValue::Reference(1));
        // Add named argument with out-of-bounds reference
        args.add_named_arg("invalid_ref".to_string(), ExtensionValue::Reference(-1));
        // Add output column with out-of-bounds reference
        args.add_output_column(ExtensionColumn::Reference(10));

        let mut warnings = Vec::new();
        validate_extension_field_references(
            &args,
            2, // input_field_count = 2, so valid references are $0 and $1
            "TestExtension",
            42, // line_no
            &mut warnings,
        );

        // Should have 3 warnings: positional arg $5, named arg $-1, output column $10
        assert_eq!(warnings.len(), 3);

        // Check that warnings contain expected information
        let warning_messages: Vec<String> = warnings.iter().map(|w| w.to_string()).collect();
        assert!(
            warning_messages
                .iter()
                .any(|msg| msg.contains("$5") && msg.contains("positional argument 0"))
        );
        assert!(
            warning_messages
                .iter()
                .any(|msg| msg.contains("$-1") && msg.contains("named argument 'invalid_ref'"))
        );
        assert!(
            warning_messages
                .iter()
                .any(|msg| msg.contains("$10") && msg.contains("output column 0"))
        );
    }

    #[test]
    fn test_validate_extension_field_references_valid_bounds() {
        let mut args = ExtensionArgs::new();
        // Add arguments with valid references within bounds
        args.add_positional_arg(ExtensionValue::Reference(0));
        args.add_positional_arg(ExtensionValue::Reference(1));
        args.add_named_arg("ref0".to_string(), ExtensionValue::Reference(0));
        args.add_named_arg("ref1".to_string(), ExtensionValue::Reference(1));
        args.add_output_column(ExtensionColumn::Reference(0));
        args.add_output_column(ExtensionColumn::Reference(1));

        let mut warnings = Vec::new();
        validate_extension_field_references(
            &args,
            2, // input_field_count = 2, so valid references are $0 and $1
            "TestExtension",
            42, // line_no
            &mut warnings,
        );

        // Should have no warnings since all references are valid
        assert_eq!(warnings.len(), 0);
    }
}
