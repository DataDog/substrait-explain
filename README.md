# Substrait-Explain

**Transform complex Substrait protobuf plans into readable, SQL EXPLAIN-like text**

A Rust library for parsing and formatting Substrait query plans in a human-readable text format.

## What does it do?

Substrait query plans are complex protobuf structures that are difficult to read and debug. This library transforms them into human-readable text that looks similar to SQL EXPLAIN output.

**Before** - Raw Substrait protobuf (72 lines of nested YAML):

<details>
<summary>Raw Substrait YAML (click to expand)</summary>

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

</details>

**After** - Human-readable text (13 lines):

```text
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

Now you can easily see what the query does: it reads from an orders table, multiplies quantity and price, filters for orders where the result is greater than 100, and outputs the revenue.

## Supported Relations

The following Substrait relations are currently supported in the text format:

- `Read`
- `Project`
- `Filter`
- `Aggregate`
- `Root`

Support for additional relations (e.g., Join, Sort, Set, etc.) is planned for future releases.
If you need a specific relation, please open an issue or contribute!

## Quick Start

```rust
use substrait_explain::{parse, format};

// Parse a plan from text format
let plan_text = r#"
=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
  @  2: https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml
Functions:
  ## 10 @  1: multiply
  ## 11 @  2: sum
  ## 12 @  2: count
=== Plan
Root[result]
  Aggregate[$0 => $0, sum($1), count($1)]
    Project[$0, multiply($1, $2)]
      Read[orders => category:string, quantity:i32?, price:i64]
"#;

let plan = parse(plan_text).unwrap();
let (output, _errors) = format(&plan);
println!("{}", output);
```

For more detailed examples and API documentation, see the [API documentation](https://github.com/DataDog/substrait-explain/blob/main/API.md).

## Installation

Add to your `Cargo.toml`:

```toml
[dependencies]
substrait-explain = "0.1.0"
substrait = "0.57.0"  # For protobuf types
```

## Documentation

- **[API Documentation](https://github.com/DataDog/substrait-explain/blob/main/API.md)** - Complete API reference with examples and configuration options
- **[Grammar Specification](https://github.com/DataDog/substrait-explain/blob/main/GRAMMAR.md)** - Detailed specification of the text format grammar
- **[Full Documentation](https://fantastic-disco-w6yyrrl.pages.github.io/substrait_explain/)** - Generated API docs on [Github Pages](https://fantastic-disco-w6yyrrl.pages.github.io/substrait_explain/), including grammar and API docs.

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
