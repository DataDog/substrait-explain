# Substrait-Explain

**Transform complex Substrait protobuf plans into readable, SQL EXPLAIN-like text**

A Rust library for parsing and formatting Substrait query plans in a human-readable text format.

## What does it do?

Substrait query plans are complex protobuf structures that are difficult to read and debug. This library transforms them into human-readable text that looks similar to SQL EXPLAIN output.

**Before** - Raw Substrait protobuf (72 lines of nested YAML):

<details>
<summary>Raw Substrait YAML (click to expand)</summary>

```yaml
extensionsURNs:
  - extensionUrnAnchor: 1
    urn: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
extensions:
  - extensionFunction:
      extensionUrnReference: 1
      functionAnchor: 10
      name: gt
  - extensionFunction:
      extensionUrnReference: 1
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

</details>

**After** - Human-readable text (13 lines):

```text
=== Extensions
URNs:
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

Now you can easily see what the query does: it reads from an orders table, multiplies quantity and price, filters for orders where the result is greater than 100, and outputs the revenue.

## Design Philosophy

`substrait-explain` is human-first and protobuf-backed. It provides a readable
text representation for inspecting, discussing, testing, and round-tripping
Substrait plans; it is not SQL, an optimizer IR, or a replacement for Substrait
protobuf.

For the project-level design stance, including round-trip expectations, error
handling, extension boundaries, and guidance for changing the format, see
[`DESIGN.md`](DESIGN.md). For the syntax and semantics of the text format itself,
see [`GRAMMAR.md`](GRAMMAR.md).

## Supported Relations

The core text format currently supports these standard relations:

- `Read`
- `Project`
- `Filter`
- `Aggregate`
- `Join`
- `Sort`
- `Root`

The grammar also documents supported read variants, extension relations, and
advanced extension addenda. For the full current syntax contract, see
[`GRAMMAR.md`](GRAMMAR.md).

Support for additional standard relations (e.g., Set, Window, etc.) is planned for future releases.
If you need a specific relation, please open an issue or contribute!

## Quick Start

### Library Usage

```rust
use substrait_explain::{parse, format};

let plan_text = r#"
=== Extensions
URNs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
Functions:
  ## 10 @  1: multiply

=== Plan
Root[revenue]
  Filter[gt($2, 100) => $0, $1]
    Project[$0, $1, multiply($0, $1)]
      Read[orders => quantity:i32?, price:fp64?]
"#;

let plan = parse(plan_text).unwrap();
let (output, _errors) = format(&plan);
println!("{}", output);
```

### CLI Usage

The CLI provides `convert` and `validate` commands for working with Substrait plans:

```bash
# Install CLI
cargo install substrait-explain --features cli

# Convert between formats (text, json, yaml, protobuf)
substrait-explain convert -f text -t json -i plan.substrait -o plan.json

# Validate plans
substrait-explain validate -i example-plans/basic.substrait
```

For detailed examples and API documentation, see the [API documentation](https://github.com/DataDog/substrait-explain/blob/main/API.md).

*Note: the CLI does not support registered advanced extension handlers for relation enhancements, optimization hints, or custom relation types such as `ExtensionMulti`, `ExtensionLeaf`, and `ExtensionSingle`. The protobuf definition is required and cannot be passed in through the CLI.*

## Installation

Add to your `Cargo.toml`:

```toml
[dependencies]
substrait-explain = "0.1.0"
substrait = "0.57.0"  # For protobuf types
```

## Documentation

- **[Design Philosophy](https://github.com/DataDog/substrait-explain/blob/main/DESIGN.md)** - Project-level design principles, compatibility expectations, and change guidance
- **[API Documentation](https://github.com/DataDog/substrait-explain/blob/main/API.md)** - Complete API reference with examples and configuration options
- **[Grammar Specification](https://github.com/DataDog/substrait-explain/blob/main/GRAMMAR.md)** - Detailed specification of the text format grammar
- **[Full Documentation](https://datadoghq.dev/substrait-explain/substrait_explain/)** - Generated API docs using GitHub Pages, including grammar and API docs.

## Examples

```bash
# Basic usage - demonstrates parsing and formatting simple plans
cargo run --example basic_usage

# Advanced usage - shows different output options and protobuf comparison
cargo run --example advanced_usage --features serde
```

## License

This project is licensed under the Apache License, Version 2.0 - see the [LICENSE](https://github.com/DataDog/substrait-explain/blob/main/LICENSE) file for details.

## Contributing

We welcome contributions! Please see [CONTRIBUTING.md](https://github.com/DataDog/substrait-explain/blob/main/CONTRIBUTING.md) for details on how to get started.
