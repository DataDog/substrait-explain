# Substrait Text Format Grammar

This document describes the grammar for the human-readable Substrait text format used by `substrait-explain`. This format allows you to write Substrait query plans in a concise, readable text format that can be parsed back into full Substrait protobuf plans.

## Overview

The Substrait text format consists of two main sections:

1. **Extensions Section** (optional) - Defines URIs and function/type extensions
2. **Plan Section** - Contains the actual query plan with relations

## Design Principles

The grammar is designed around several concrete choices that make it practical and consistent:

### 1. Single-Line, Structured Relations

All relations follow the same structure: `Name[arguments => columns]`

- **Name**: The relation type (Read, Filter, Project, etc.)
- **Arguments**: Relation-specific: input expressions, field references, or function calls
  - Arguments follow a regular pattern (tuple, input expression, etc.) or combination, and should map directly to Substrait proto fields. Uses tuples for compound arguments, with literals, expressions, and enums for values.
- **Arrow**: `=>` separates arguments from output columns
- **Columns**: Output column names and types

Every relation fits on one line with indentation showing hierarchy. This uniform pattern makes it easy to parse any relation, understand input/output structure, and add new relation types.

### 2. SQL-Like References, Literals, and Enums

- Field references: `$0`, `$1`, etc.
- Types are shown inline with literals and column names: `42:i64`, `'hello':string`
- Nullability is explicit: `string?` for nullable, `string` for non-nullable

This prevents ambiguity and makes plans self-documenting while being familiar to SQL developers.

### 3. Extension Support and Structured Syntax

- Extensions section defines URIs and function/type mappings.
- Function calls can include anchors: `add#10@1($0, $1)`.
- Clear structural boundaries: `[]` for relations, `<>` for types, `()` for functions.
- Maintains full Substrait compatibility while keeping the text format readable and parseable.

### 4. Hierarchical Organization

- Section headers (`===`) separate major components.
- 2-space indentation shows query plan hierarchy.
- Consistent formatting across all document elements.

The format maps directly to Substrait protobuf messages, with relations, expressions, types, and extensions corresponding to their respective protobuf structures.

## Grammar Notation

This document uses **PEG (Parsing Expression Grammar)** notation:

- **`"text"`** - Literal text
- **`element?`** - Optional element
- **`element*`** - Zero or more repetitions
- **`element+`** - One or more repetitions
- **`element1 / element2`** - Choice (try element1 first)
  - _Implementation Note: Pest uses `|` instead of `/`_
- **`element1 element2`** - Sequence
  - _Implementation Note: Pest uses `~` for explicit concatenation_

## Basic Example

```rust
# use substrait_explain::parser::Parser;
#
# let plan_text = r#"
=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
Functions:
  ## 10 @  1: add
  ## 11 @  1: multiply

=== Plan
Root[result]
  Project[$0, $1, add($0, $1)]
    Read[orders => quantity:i32?, price:i64]
# "#;
#
# let plan = match Parser::parse(plan_text) {
#     Ok(plan) => plan,
#     Err(e) => panic!("{}", e),
# };
# assert_eq!(plan.relations.len(), 1);
```

## Document Structure

A Substrait text format document consists of two main sections with specific formatting rules.

### Sections

The document uses `===` headers to separate major sections:

- **`=== Extensions`** - Defines URIs and function/type mappings (optional)
- **`=== Plan`** - Contains the actual query plan (required)

#### Extension format

```text
=== Extensions
URIs:
  @  uri_anchor: uri
  …
Functions:
  ##  anchor @  uri_anchor: name
  …
Types:
  ##  anchor @  uri_anchor: name
  …
Type Variations:
  ##  anchor @  uri_anchor: name
  …
```

Where `anchor` and `uri_anchor` are integers, `uri` is a text URI, and function, type, and type variation names are identifiers or quoted text.

### Plan Hierarchy and Indentation

Relations use indentation to show the query plan hierarchy:

- **Root level**: No indentation (typically `Root` relation)
- **Child relations**: Indented with 2 spaces per level
- **Each relation**: On its own line with format `Name[arguments => columns]`

#### Example

```rust
# use substrait_explain::parser::Parser;
#
# let plan_text = r#"
=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
Functions:
  ## 10 @  1: gt

=== Plan
Root[result]                   // Level 0 (no indentation)
  Project[$0, $1]              // Level 1 (2 spaces)
    Filter[gt($0, 10) => $0]   // Level 2 (4 spaces)
      Read[data => a:i64]      // Level 3 (6 spaces)
# "#;
#
# let plan = match Parser::parse(plan_text) {
#     Ok(plan) => plan,
#     Err(e) => panic!("{}", e),
# };
# assert_eq!(plan.relations.len(), 1);
```

## Basic Terminals

### Character Classes

- **`letter`**` := [a-zA-Z]` - Alphabetic characters
- **`digit`**` := [0-9]` - Numeric digits

### `name` and `identifier`

- **`name`**` := identifier / quoted_name`
  - Used for column names, function names, etc. It can be unquoted if it's a valid identifier, or using "double quotes" if special characters are required (much like SQL)
  - Examples: `function_name`, `"quoted name"`
- **`identifier`**` := letter (letter / digit / "_")*`
  - Used for columns, function names, etc. that are proper identifiers.
  - Examples: `table_name`, `my_function`, `col1`
- **`quoted_name`**` := '"' ("\\" . / !'"' .)* '"'`
  - Used for columns, function names, etc. that are not valid as identifiers, and thus need quoting.
  - Examples: `"function name"`, `"table.name"`, `"table\.name"`, `"function \"with some\nescapes\""`

### `enum`

Enum fields in arguments are represented as &-prefixed variants (e.g., `&AscNullsFirst`), matching the Substrait proto definition. This applies to all enum fields in relation arguments.

#### Syntax

`enum := "&" identifier`

#### Examples

- `&AscNullsFirst`, `&AscNullsLast`, `&DescNullsFirst`, `&DescNullsLast` - sort directions

### `literal`

A literal can come in the form of an integer, float, boolean, or string, and can have an optional additional type:

`literal := (integer / float / boolean / string) (":" type)?`

- **`integer`**` := "-"? digit+`
  - Examples: `42`, `-10`, `0`
  - Default to `i64` type; other integer types may be assigned
- **`float`**` := "-"? digit+ "." digit+`
  - Examples: `3.14`, `-2.5`, `1.0`
  - Default to `fp64` type; other float types may be assigned
- **`boolean`**` := "true" / "false"`
  - Examples: `true`, `false`
  - May only be boolean type
- **`string`**` := "'" ("\\" . / !"'" .)* "'"`
  - Examples: `'hello'`, `'table name'`, `'C:\path\to\file'`, `'line1\nline2'`, `'quote\'s here'`
  - Default to `string` type; other types may also be assigned
- **`typed_literal`**` := string ":" type`
  - String literals with type annotations for non-primitive types
  - Examples: `'2023-01-01':date`, `'2023-12-25T14:30:45.123':timestamp`

**TODO**: The current Pest grammar only supports `integer` and `string`. The grammar needs to be extended to support `float`, `boolean`, and typed literals as described above.

## Types

The type syntax in this grammar follows the [standard Substrait type definition syntax](https://substrait.io/types/type_parsing/), with extensions to support anchors and URI references for user-defined types.

### Type Syntax Overview

All types follow this general pattern:

```text
type := "u!"? name anchor? uri_anchor? nullability? parameters?
```

Where:

- **`"u!"`** - Optional prefix for user-defined types
- **`name`** - Type name (case-insensitive, lowercase preferred)
- **`anchor`**` := "#" integer` - Extension anchor (e.g., `#10`)
- **`uri_anchor`**` := "@" integer` - URI anchor (e.g., `@1`)
- **`nullability`**` := "?"` - Optional nullability indicator (defaults to non-nullable)
- **`parameters`**` := "<" (param ("," param)*)? ">"` - Optional type parameters
- **`param`**` := type / integer / name` - Type parameter (type, integer, or name)

### Simple Types

Simple types are the basic Substrait types with optional nullability.

#### Syntax

`simple_type_name nullability?`

#### Simple Type Names

From [official Substrait grammar](https://raw.githubusercontent.com/substrait-io/substrait/refs/heads/main/grammar/SubstraitType.g4), `simple_type_name` can be any of these literal strings:

- `boolean`, `i8`, `i16`, `i32`, `i64`
- `fp32`, `fp64`
- `string`, `binary`
- `timestamp`, `timestamp_tz`, `date`, `time`
- `interval_year`, `uuid`

#### Nullability

- `?` - nullable
- `⁉` - unspecified nullability (not generally valid)
- (nothing) - non-nullable

##### Examples:

```rust
# use substrait_explain::parser::Parser;

let plan_text = r#"
=== Plan
Root[result]
  Project[$0, $1]
    Read[data => int_field:i64, string_field:string?]
"#;
#
# let plan = match Parser::parse(plan_text) {
#     Ok(plan) => plan,
#     Err(e) => panic!("{}", e),
# };
# assert_eq!(plan.relations.len(), 1);
```

### Compound Types

Compound types follow the same syntax as standard Substrait parameterized types.

#### Examples

// TODO: This example uses `map` type, which is not yet implemented in the parser.

```text
use substrait_explain::parser::Parser;

let plan_text = r#"
=== Plan
Root[result]
  Project[$0, $1, $2]
    Read[data => list_field:list<i64>, map_field:map<string, i64>, struct_field:struct<i64, string?>]
"#;

let plan = Parser::parse(plan_text).unwrap();
assert_eq!(plan.relations.len(), 1);
```

### User-Defined Types

User-defined types extend the standard Substrait UDT syntax to support anchors and URI references.

#### Syntax

`"u!"? name anchor? uri_anchor? nullability? parameters?`

#### Key differences from standard Substrait

- The `u!` prefix is optional (can be omitted when anchors are present)
- Adds optional `anchor` and `uri_anchor` for extension references
- Maintains compatibility with standard Substrait UDT syntax

#### Examples

```rust
# use substrait_explain::parser::Parser;
#
# let plan_text = r#"
=== Extensions
URIs:
  @  1: https://example.com/types
  @  2: https://example.com/functions
Types:
  ##  8 @  1: point
  ##  9 @  1: custom_type
Functions:
  ## 10 @  2: add

=== Plan
Root[result]
  Project[$0, $1, $2]
    Read[data => point_field:point#8@1?<i8>, custom_field:custom_type#9, prefixed_field:u!custom_type]
# "#;
#
# let plan = Parser::parse(plan_text).unwrap();
# assert_eq!(plan.relations.len(), 1);
```

## Expressions

#### Syntax

`expression := function_call / reference / literal`

### Examples

```text
add($3, 10)            // Simple function call
add#10@2(#3, 10):int   // Function call with anchors and type
```

### Field References

Currently, only references to fields in the Relations' input are supported.

#### Syntax

`reference := "$" integer`

#### Examples

```rust
use substrait_explain::parser::Parser;

let plan_text = r#"
=== Plan
Root[result]
  Project[$0, $1, $42]
    Read[data => field0:i64, field1:string, field42:boolean]
"#;

let plan = Parser::parse(plan_text).unwrap();
assert_eq!(plan.relations.len(), 1);
```

### Function Calls

#### Syntax

`function_call := name anchor? uri_anchor? "(" (expression ("," expression)*)? ")" (":" type)?`

#### Components

- `name` - function name
- `anchor` - optional anchor (e.g., `#10`)
- `uri_anchor` - optional URI anchor (e.g., `@1`)
- `expression` - as above
- `type` - optional output type

### Aggregate Measures

Aggregate measures are used in the output of Aggregate relations. They can be either field references (to pass through existing fields) or aggregate function calls (to compute aggregates).

#### Syntax

- `aggregate_measure := name anchor? uri_anchor? "(" expression ")" (":" type)?` - aggregate function call with optional extension anchors and output type
- Field references: `$0`, `$1`, ...

#### Examples

- `sum($2)`
- `count($1)`
- `avg($3):fp64`
- `$0` (field reference to grouping field)

## Relations

Relations represent the operations in a query plan. Each relation is displayed on a single line with indentation showing the hierarchy.

### General Relation Grammar

All relations follow this general pattern:

#### Syntax

```text
relation := name "[" (arguments ("," named_arguments)? ("=>" columns)?)? "]"
columns := name ("," name)* / reference_list
```

Where:

- **`name`**: The type of operation (Read, Filter, Project, Root, etc.)
- **`arguments`**: Input expressions, field references, function calls, or other parameters (optional)
- **`named_arguments`**: Named arguments (optional)
- **`=>`**: Separator between arguments and output columns (optional, only present when both arguments and columns are specified)
- **`columns`**: Output column names and types, or field references for pass-through (all relations specify outputs, but format varies)

#### Example

```text
RelationName[arguments, named_arguments => columns]
```

#### Special cases

- **Root relation**: Only specifies output column names, no arguments or `=>` separator
- **Project relation**: Only specifies expressions, no `=>` separator or output columns
- Some relations may use '...' instead of column names when they pass through all fields

The exact structure varies by relation type, but all follow this basic pattern.

### Arguments

Arguments in relations can be literals, expressions, enums, or tuples thereof.

#### Syntax

```text
argument := literal / expression / enum / tuple
tuple := "(" argument ("," argument)* ")"
arguments := argument ("," argument)*
named_arguments := name "=" argument ("," name "=" argument)*
```

#### Examples

- Simple arguments: `$0`, `42`, `'hello'`, `&AscNullsFirst`
- Tuple arguments: `($0, &AscNullsFirst)`, `(limit=10, offset=5)`
- Named arguments: `limit=10`, `offset=5`

### Root Relation

#### Syntax

`"Root" "[" (name ("," name)*)? "]"`

#### Example

```rust
# use substrait_explain::parser::Parser;
#
# let plan_text = r#"
=== Plan
Root[c, d]           // root with output columns c and d
  Project[$0, $1]
    Read[data => a:i64, b:string]
# "#;
#
# let plan = Parser::parse(plan_text).unwrap();
# assert_eq!(plan.relations.len(), 1);
```

### Read Relation

#### Syntax

`"Read" "[" table_name "=>" (named_column ("," named_column)*)? "]"`

#### Components

- `table_name := name ("." name)*` - table name, optionally qualified with schema/database
- `named_column := name ":" type` - column name with type annotation

#### Example

```rust
# use substrait_explain::parser::Parser;
#
# let plan_text = r#"
=== Plan
Root[result]
  Project[$0, $1]
    Read[schema.table => a:i64, b:string?]
Root[result2]
  Project[$0, $1]
    Read[orders => quantity:i32?, price:i64]
# "#;
#
# let plan = Parser::parse(plan_text).unwrap();
# assert_eq!(plan.relations.len(), 2);
```

### Filter Relation

#### Syntax

`"Filter" "[" expression "=>" reference_list "]"`

#### Components

- `expression` - boolean expression for filtering
- `reference_list := reference ("," reference)*` - comma-separated list of field references to pass through

#### Example

```rust
# use substrait_explain::parser::Parser;
#
# let plan_text = r#"
=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
Functions:
  ## 10 @  1: gt

=== Plan
Root[result]
  Filter[gt($2, 100) => $0, $1, $2]
    Project[$0, $1, $2]
      Read[data => a:i64, b:string, c:i32]
# "#;
#
# let plan = Parser::parse(plan_text).unwrap();
# assert_eq!(plan.relations.len(), 1);
```

### Project Relation

#### Syntax

`"Project" "[" (expression ("," expression)*)? "]"`

#### Components

- `expression` - field reference, function call, or literal (see Expressions section)

#### Example

```rust
# use substrait_explain::parser::Parser;
#
# let plan_text = r#"
=== Plan
Root[result]
  Project[$1, 42]                    // project field 1 and literal 42
    Read[data => a:i64, b:string]
# "#;
#
# let plan = Parser::parse(plan_text).unwrap();
# assert_eq!(plan.relations.len(), 1);
```

### Aggregate Relation

#### Syntax

`"Aggregate" "[" group_by "=>" aggregate_output "]"`

#### Components

- `group_by := reference_list | "_"` - comma-separated list of field references for grouping, or `_` for global aggregation
- `aggregate_output := (reference | aggregate_measure) ("," (reference | aggregate_measure))*` - comma-separated list of output items
- `aggregate_measure` - field references or aggregate function calls. See [Aggregate Measures section](#aggregate-measures)

#### Example

```rust
# use substrait_explain::parser::Parser;
#
# let plan_text = r#"
=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml
Functions:
  ## 10 @  1: sum
  ## 11 @  1: count

=== Plan
Root[result]
  Aggregate[$0 => $0, sum($1), count($2)]           // Group by field 0
    Read[orders => category:string, amount:i64]
# "#;
#
# let plan = Parser::parse(plan_text).unwrap();
# assert_eq!(plan.relations.len(), 1);
```

### Sort Relation

The Sort relation specifies sort fields and directions for ordering the input:

Sort[($0, &AscNullsFirst), ($1, &DescNullsLast) => $0, $1]

#### Syntax

```text
sort_relation := "Sort" "[" sort_fields "=>" reference_list "]"
sort_fields := sort_field ("," sort_field)*
sort_field := "(" reference "," sort_direction ")"
sort_direction := "&AscNullsFirst" / "&AscNullsLast" / "&DescNullsFirst" / "&DescNullsLast"
```

#### Components

- Each sort field is a tuple: `(reference, sort_direction)`
- Sort directions follow the general `enum` syntax and specify null handling
- The columns after `=>` specify the output field order (typically a reference list)

## Complete Example

A complete query that reads from an orders table, multiplies quantity and price, filters for high-value orders, groups by category, and outputs the total revenue and order count per category:

```rust
# use substrait_explain::parser::Parser;
#
# let plan_text = r#"
=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
  @  2: https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml
Functions:
  ## 10 @  1: multiply
  ## 11 @  1: gt
  ## 12 @  2: sum
  ## 13 @  2: count

=== Plan
Root[category_stats]
  Aggregate[$0 => $0, sum($1), count($2)]
    Filter[gt($1, 100) => $0, $1, $2]
      Project[$0, multiply($1, $2), $2]
        Read[orders => category:string, quantity:i32?, price:i64]
# "#;
#
# let plan = Parser::parse(plan_text).unwrap();
# assert_eq!(plan.relations.len(), 1);
```
