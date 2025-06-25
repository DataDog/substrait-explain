# Substrait-Explain

**Transform complex Substrait protobuf plans into readable, SQL EXPLAIN-like text**

## What does it do?

Ever tried to debug a Substrait query plan? Outputting the raw protobuf as text ends up quite difficult to read:

```yaml
extensionUris:
  - extensionUriAnchor: 1
    uri: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
extensions:
  - extensionFunction:
      extensionUriReference: 1
      functionAnchor: 10
      name: gt
  - extensionFunction:
      extensionUriReference: 1
      functionAnchor: 11
      name: multiply
relations:
  - root:
      input:
        filter:
          common:
            emit:
              outputMapping:
                - 0
                - 1
          input:
            project:
              common:
                emit:
                  outputMapping:
                    - 0
                    - 1
                    - 2
              input:
                read:
                  baseSchema:
                    names:
                      - quantity
                      - price
                    struct:
                      types:
                        - i32:
                            nullability: NULLABILITY_NULLABLE
                        - fp64:
                            nullability: NULLABILITY_NULLABLE
                      nullability: NULLABILITY_REQUIRED
                  namedTable:
                    names:
                      - orders
              expressions:
                - scalarFunction:
                    functionReference: 11
                    arguments:
                      - value:
                          selection:
                            directReference:
                              structField: {}
                      - value:
                          selection:
                            directReference:
                              structField:
                                field: 1
          condition:
            scalarFunction:
              functionReference: 10
              arguments:
                - value:
                    selection:
                      directReference:
                        structField:
                          field: 2
                - value:
                    literal:
                      i64: "100"
      names:
        - revenue
```

**Substrait-Explain converts this into:**

```
=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
Functions:
  # 10 @  1: gt
  # 11 @  1: multiply

=== Plan
Root[revenue]
  Filter[gt($2, 100) => $0, $1]
    Project[$0, $1, multiply($0, $1)]
      Read[orders => quantity:i32?, price:fp64?]
```

Suddenly you can see exactly what the query does: filter orders where some calculated value is greater than 100, then project the quantity, price, and their product.

## Key Features

- **Human-readable output**: Convert complex Substrait plans into simple, readable text
- **Bidirectional conversion**: Parse text format back into Substrait plans
- **Extension support**: Full support for Substrait extensions and custom functions
- **Error handling**: Graceful error handling that doesn't prevent output generation
- **Flexible formatting**: Configurable output options for different use cases

### Documentation

- **[Grammar Specification](https://docs.rs/substrait-explain/latest/substrait_explain/docs/)**: Complete reference for the text format grammar (available in the Rust documentation)
- **API Documentation**: Available on [docs.rs](https://docs.rs/substrait-explain) (TODO: Fix these links once this is released!)

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
substrait-explain = "0.1.0"
substrait = "0.57.0"  # For protobuf types
```

## Quick Start

### Basic Usage

```rust
use substrait_explain::textify::{PlanWriter, OutputOptions, ErrorQueue};
use substrait_explain::parser::Parser;

// Parse a Substrait plan from text format
let plan_text = r#"
=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
Functions:
  # 10 @  1: add
=== Plan
Project[$0, $1, add($0, $1)]
  Read[table1 => col1:i32?, col2:i32?]
"#;

let plan = Parser::parse(plan_text).unwrap();

// Convert to human-readable format
let options = OutputOptions::default();
let writer = PlanWriter::<ErrorQueue>::new(&options, &plan);
let output = format!("{}", writer);
println!("{}", output);
```

### Working with Substrait Protobuf Plans

```rust
use substrait_explain::textify::{PlanWriter, OutputOptions, ErrorQueue};
use substrait::proto::Plan;

// If you have a Substrait protobuf plan
let plan: Plan = /* your protobuf plan */;

// Convert it to readable text
let options = OutputOptions::default();
let writer = PlanWriter::<ErrorQueue>::new(&options, &plan);
let output = format!("{}", writer);

// Check for any errors during conversion
let errors = writer.errors;
if !errors.is_empty() {
    println!("Warnings during conversion: {:?}", errors);
}
```

### Parsing Individual Components

```rust
use substrait_explain::parser::{Parse, ScopedParse};
use substrait_explain::textify::{TestContext, OutputOptions};
use substrait::proto::{Type, Expression};

// Parse and format types
let ctx = TestContext::new();
let typ = Type::parse("i32?").unwrap();
let formatted = ctx.textify_no_errors(&typ);
assert_eq!(formatted, "i32?");

// Parse and format expressions
let expr = Expression::parse("add($0, $1)").unwrap();
let formatted = ctx.textify_no_errors(&expr);
assert_eq!(formatted, "add($0, $1)");
```

### Working with Extensions

```rust
use substrait_explain::extensions::SimpleExtensions;
use substrait_explain::extensions::simple::ExtensionKind;
use substrait_explain::textify::TestContext;

// Create extensions context
let mut extensions = SimpleExtensions::new();
extensions.add_extension_uri("https://example.com/functions".to_string(), 1).unwrap();
extensions.add_extension(ExtensionKind::Function, 1, 10, "my_function".to_string()).unwrap();

// Use extensions in parsing
let ctx = TestContext::new().with_function(1, 10, "my_function");
let expr = Expression::parse(&ctx.extensions, "my_function#10($0, $1)").unwrap();
let formatted = ctx.textify_no_errors(&expr);
assert_eq!(formatted, "my_function($0, $1)");
```

## Output Format

The library produces a structured text format that's easy to read and parse:

### Basic Plan Structure

```
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

```rust
use substrait_explain::textify::OutputOptions;

// Default options - concise output
let options = OutputOptions::default();

// Verbose options - show all details
let verbose_options = OutputOptions::verbose();

// Custom options
let custom_options = OutputOptions {
    show_extension_uris: true,
    show_simple_extensions: true,
    show_simple_extension_anchors: Visibility::Always,
    literal_types: Visibility::Required,
    show_emit: false,
    read_types: true,
    fn_types: true,
    nullability: true,
    indent: "  ".to_string(),
    show_literal_binaries: false,
};
```

## Error Handling

The library uses an error accumulation approach where errors don't prevent output generation:

```rust
use substrait_explain::textify::{ErrorQueue, PlanWriter, OutputOptions};

let options = OutputOptions::default();
let writer = PlanWriter::<ErrorQueue>::new(&options, &plan);

// Generate output even if there are errors
let output = format!("{}", writer);

// Check for errors
let errors: Vec<_> = writer.errors.into_iter().collect();
if !errors.is_empty() {
    println!("Warnings during conversion:");
    for error in errors {
        println!("  - {}", error);
    }
}
```

## Examples

### Basic Usage

For a simple introduction, see the `basic_usage` example:

```bash
cargo run --example basic_usage
```

### Advanced Usage

For a comprehensive demonstration including different output options and protobuf comparison, see the `advanced_usage` example:

```bash
cargo run --example advanced_usage --features serde
```

This example shows:

- Standard vs verbose output formatting
- Custom output options (different indentation, type visibility)
- Comparison with the original YAML representation
- Explanation of what the query transformation accomplishes

### Simple Query Plan

```rust
let plan_text = r#"
=== Plan
Project[$0, $1, add($0, $1)]
  Read[table1 => col1:i32?, col2:i32?]
"#;

let plan = Parser::parse_plan(plan_text).unwrap();
let writer = PlanWriter::<ErrorQueue>::new(&OutputOptions::default(), &plan);
println!("{}", writer);
```

### Complex Query with Extensions

```rust
let plan_text = r#"
=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
Functions:
  # 10 @  1: add
  # 11 @  1: multiply
=== Plan
Filter[ | gt($2, 100)]
  Project[ | $0, $1, multiply($0, $1)]
    Read[orders | quantity:i32?, price:fp64?]
"#;

let plan = Parser::parse_plan(plan_text).unwrap();
let writer = PlanWriter::<ErrorQueue>::new(&OutputOptions::default(), &plan);
println!("{}", writer);
```

## Development

### Setup

This project uses a standard Rust setup:

```bash
# Test
cargo test

# Build
cargo build

# Format code
cargo fmt

# Lint with clippy
cargo clippy
```

### Pre-Commit Hooks

Run `bash pre-commit.sh install` to install the pre-commit Git hook, which will run `cargo fmt` and `cargo clippy -- -D warnings` whenever `git commit` is called.

### Advanced Formatting

For more thorough import formatting (requires nightly Rust):

```sh
cargo +nightly fmt -- --unstable-features --config imports_granularity=Module,group_imports=StdExternalCrate
```

## License

This project is licensed under the Apache License, Version 2.0 - see the [LICENSE](LICENSE) file for details.

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](CONTRIBUTING.md) for details on how to get started.
