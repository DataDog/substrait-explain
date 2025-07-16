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

For installation instructions, see the [README](https://github.com/DataDog/substrait-explain/blob/main/README.md).

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
  @  2: https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml
Functions:
  # 10 @  1: add
  # 11 @  2: sum
  # 12 @  2: count
=== Plan
Root[result]
  Aggregate[$0 => $0, sum($1), count($1)]
    Project[$0, add($1, $2)]
      Read[table1 => category:string, col1:i32?, col2:i32?]
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
- **Function calls**: `add($0, $1)`, `sum($2)` (scalar and aggregate functions)
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

## Command Line Interface

The library includes a command line interface for converting between different Substrait plan formats and validating plans. The CLI is available behind the `cli` feature flag.

### Installation

Install the CLI with:

```bash
cargo install substrait-explain --features cli
```

Or build from source:

```bash
cargo build --release --features cli
```

### Commands

#### Convert Command

The `convert` command transforms plans between different formats:

```bash
# Convert text format to JSON
substrait-explain convert -f text -t json -i plan.substrait -o plan.json

# Convert JSON back to text
substrait-explain convert -f json -t text -i plan.json -o plan.substrait

# Convert to binary protobuf format
substrait-explain convert -f text -t protobuf -i plan.substrait -o plan.pb

# Use stdin/stdout (default)
cat plan.substrait | substrait-explain convert -f text -t json > plan.json
```

**Supported formats:**

- `text` - Human-readable Substrait text format
- `json` - JSON serialized protobuf
- `yaml` - YAML serialized protobuf
- `protobuf`/`proto`/`pb` - Binary protobuf format

**Options:**

- `-f, --from <FORMAT>` - Input format (default: text)
- `-t, --to <FORMAT>` - Output format (default: text)
- `-i, --input <FILE>` - Input file (default: stdin)
- `-o, --output <FILE>` - Output file (default: stdout)
- `--show-literal-types` - Show type annotations on literals
- `--show-expression-types` - Show type annotations on expressions
- `--verbose` - Show detailed progress information

#### Validate Command

The `validate` command performs a roundtrip test on text format plans:

```bash
# Validate a plan file
substrait-explain validate -i plan.substrait

# Validate from stdin
cat plan.substrait | substrait-explain validate

# Validate with verbose output
substrait-explain validate -i plan.substrait --verbose
```

**Options:**

- `-i, --input <FILE>` - Input file (default: stdin)
- `-o, --output <FILE>` - Output file (default: stdout)
- `--verbose` - Show detailed progress information

### Examples

```bash
# Validate the example plans
substrait-explain validate -i example-plans/basic.substrait
substrait-explain validate -i example-plans/simple.substrait

# Convert with verbose output and type information
substrait-explain convert -f text -t json --show-literal-types --show-expression-types --verbose -i example-plans/basic.substrait

# Roundtrip test: text → protobuf → text
substrait-explain convert -f text -t protobuf -i plan.substrait -o plan.pb
substrait-explain convert -f protobuf -t text -i plan.pb -o plan_roundtrip.substrait
diff plan.substrait plan_roundtrip.substrait
```

### Feature Requirements

To use the CLI, you must build/install with the `cli` feature:

```toml
[dependencies]
substrait-explain = { version = "0.1.0", features = ["cli"] }
```

For JSON/YAML support, also enable the `serde` feature:

```toml
[dependencies]
substrait-explain = { version = "0.1.0", features = ["cli", "serde"] }
```
