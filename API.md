# Substrait-Explain

**Transform complex Substrait protobuf plans into readable, SQL EXPLAIN-like text**

A Rust library that converts Substrait query plans between protobuf format and a human-readable text format. It transforms verbose, nested protobuf structures into concise, SQL-like text that's easy to read and debug.

## Key Features

- **Human-readable output**: Convert complex Substrait plans into simple, readable text
- **Bidirectional conversion**: Parse text format back into Substrait plans
- **Extension support**: Full support for Substrait extensions and custom functions
- **Error handling**: Graceful error handling that doesn't prevent output generation
- **Flexible formatting**: Configurable output options for different use cases
- **Complete grammar**: Full specification of the text format in the [`grammar`] module

For installation instructions, see the [README](README.md).

## Quick Start

### Parse and Format Plans

The main workflow is parsing text format and formatting plans. This example demonstrates both basic usage and extension handling:

```rust
use substrait_explain::{parse, format};

// Parse a plan from text format (includes extensions for custom functions)
let plan_text = r#"
=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
Functions:
  ## 10 @  1: add

=== Plan
Project[$0, $1, add($0, $1)]
  Read[table1 => col1:i32?, col2:i32?]
"#;

let plan = parse(plan_text).unwrap();
let (output, errors) = format(&plan);
println!("{}", output);

// Check for any formatting warnings
if !errors.is_empty() {
    println!("Warnings: {:?}", errors);
}
```

### Custom Formatting

Control output detail with formatting options:

```rust
use substrait_explain::{parse, format_with_options, OutputOptions, Visibility};

let plan = parse(r#"
=== Plan
Project[$0, 42, 54:i16]
  Read[data => name:string?, num:i64]
"#).unwrap();

// Verbose output with all details
let verbose = OutputOptions::verbose();
let (text, _) = format_with_options(&plan, &verbose);

// Custom options
let custom = OutputOptions {
    literal_types: Visibility::Always,
    indent: "    ".to_string(),
    ..OutputOptions::default()
};
let (text, _) = format_with_options(&plan, &custom);
```

### Error Handling

The library provides graceful error handling for formatting, producing best-effort output even if there are errors:

```rust
use substrait_explain::{parse, format};

match parse("=== Plan\nInvalidRelation[invalid]") {
    Ok(plan) => {
        let (text, errors) = format(&plan);
        println!("Formatted: {}", text);
        if !errors.is_empty() {
            println!("Warnings: {:?}", errors);
        }
    }
    Err(e) => println!("Parse error: {}", e),
}
```

## Output Format

The library produces a structured text format that's easy to read and parse. For a complete specification of the text format grammar, see the [`grammar`] module.

### Basic Plan Structure

```text
=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
Functions:
  # 10 @  1: add
=== Plan
Project[$0, $1, add($0, $1)]
  Read[table1 => col1:i32?, col2:i32?]
```

### Relation Format

Each relation is displayed on a single line with the format:
`RelationName[arguments => columns]`

- **arguments**: Input expressions, field references, or function calls
- **columns**: Output column names and types
- **indentation**: Shows the relationship hierarchy

### Expression Format

- **Field references**: `$0`, `$1`, etc.
- **Literals**: `42`, `"hello"`, `true`
- **Function calls**: `add($0, $1)`, `sum($2)`
- **Types**: `i32`, `string?`, `list<i64>`

## Configuration Options

Control output formatting with [`OutputOptions`]:

```rust
use substrait_explain::{OutputOptions, Visibility};

// Default - concise output
let default = OutputOptions::default();

// Verbose - show all details
let verbose = OutputOptions::verbose();

// Custom - show literal types and use 4-space indentation
let custom = OutputOptions {
    literal_types: Visibility::Always,
    indent: "    ".to_string(),
    ..OutputOptions::default()
};
```
