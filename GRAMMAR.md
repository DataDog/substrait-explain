# Substrait Text Format Grammar

This document describes the grammar for the human-readable Substrait text format used by `substrait-explain`. This format allows you to write Substrait query plans in a concise, readable text format that can be parsed back into full Substrait protobuf plans.

## Overview

The Substrait text format consists of two main sections:

1. **Extensions Section** (optional) - Defines URNs and function/type extensions
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

- Extensions section defines URNs and function/type mappings.
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
URNs:
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
# let plan = Parser::parse(plan_text).unwrap();
# assert_eq!(plan.relations.len(), 1);
```

## Document Structure

A Substrait text format document consists of two main sections with specific formatting rules.

### Sections

The document uses `===` headers to separate major sections:

- **`=== Extensions`** - Defines URNs and function/type mappings (optional)
- **`=== Plan`** - Contains the actual query plan (required)

#### Extension format

```text
=== Extensions
URNs:
  @  urn_anchor: urn
  …
Functions:
  ##  anchor @  urn_anchor: name
  …
Types:
  ##  anchor @  urn_anchor: name
  …
Type Variations:
  ##  anchor @  urn_anchor: name
  …
```

Where `anchor` and `urn_anchor` are integers, `urn` is a text URN, and function, type, and type variation names are identifiers or quoted text.

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
URNs:
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
# let plan = Parser::parse(plan_text).unwrap();
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

`literal := (float / integer / boolean / string) (":" type)?`

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

All basic literal types (`integer`, `float`, `boolean`, and `string`) are supported, plus `date`, `time`, and `timestamp` typed literals. Other Substrait literal types (e.g., `interval_year`, `decimal`, `uuid`) are not yet implemented.

## Types

The type syntax in this grammar follows the [standard Substrait type definition syntax](https://substrait.io/types/type_parsing/), with extensions to support anchors and URN references for user-defined types.

### Type Syntax Overview

All types follow this general pattern:

```text
type := "u!"? name anchor? urn_anchor? nullability? parameters?
```

Where:

- **`"u!"`** - Optional prefix for user-defined types
- **`name`** - Type name (case-insensitive, lowercase preferred)
- **`anchor`**` := "#" integer` - Extension anchor (e.g., `#10`)
- **`urn_anchor`**` := "@" integer` - URN anchor (e.g., `@1`)
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
  Project[$0, $1, $2, $3]
    Read[data => int_field:i64, string_field:string?, created_at:timestamp?, user_id:uuid]
"#;
#
# let plan = Parser::parse(plan_text).unwrap();
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

User-defined types extend the standard Substrait UDT syntax to support anchors and URN references.

#### Syntax

`"u!"? name anchor? urn_anchor? nullability? parameters?`

#### Key differences from standard Substrait

- The `u!` prefix is optional (can be omitted when anchors are present)
- Adds optional `anchor` and `urn_anchor` for extension references
- Maintains compatibility with standard Substrait UDT syntax

#### Examples

```rust
# use substrait_explain::parser::Parser;
#
# let plan_text = r#"
=== Extensions
URNs:
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

`expression := function_call / reference / literal / if_then`

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
# use substrait_explain::parser::Parser;
# 
# let plan_text = r#"
=== Plan
Root[result]
  Project[$0, $1, $42]
    Read[data => field0:i64, field1:string, field42:boolean]
# "#;
#
# let plan = Parser::parse(plan_text).unwrap();
# assert_eq!(plan.relations.len(), 1);
```

### Function Calls

#### Syntax

`function_call := name anchor? urn_anchor? "(" (expression ("," expression)*)? ")" (":" type)?`

#### Components

- `name` - function name
- `anchor` - optional anchor (e.g., `#10`)
- `urn_anchor` - optional URN anchor (e.g., `@1`)
- `expression` - as above
- `type` - optional output type

### Aggregate Measures

Aggregate measures are used in the output of Aggregate relations. They can be either field references (to pass through existing fields) or aggregate function calls (to compute aggregates).

#### Syntax

- `aggregate_measure := name anchor? urn_anchor? "(" expression ")" (":" type)?` - aggregate function call with optional extension anchors and output type
- Field references: `$0`, `$1`, ...

#### Examples

- `sum($2)`
- `count($1)`
- `avg($3):fp64`
- `$0` (field reference to grouping field)

### IfThen

An IfThen expression is a conditional function or logical operator that evaluates to a boolean.

#### Syntax

`if_then := "if_then(" (if_clause ",")+ "_ ->" expression ")"`

`if_clause := expression "->" expression`

#### Examples

```rust
# use substrait_explain::parser::Parser;
# 
# let plan_text = r#"
=== Plan
Root[status]
  Fetch[limit=10, offset=0 => ]
    Project[if_then(true -> $0, false -> $1, _ -> $2)]
      Read[events.logs => status:string?]
#  "#;
# 
#  let plan = Parser::parse(plan_text).unwrap();
#  assert_eq!(plan.relations.len(), 1);
```

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

### VirtualTable Read Relation

A VirtualTable read embeds inline data directly in the plan, similar to SQL's `VALUES` clause. Instead of referencing a catalog table, the data rows are specified as part of the relation.

The `Read:Virtual` relation uses the same `ReadRel` protobuf message with `ReadType::VirtualTable`, where each row is a `nested::Struct` containing expressions.

#### Syntax

`"Read:Virtual" "[" (virtual_row ("," virtual_row)*)? "=>" named_column_list "]"`

Where `virtual_row := "(" expression ("," expression)* ")"` — a parenthesized tuple of expressions forming one row. Use `_` in place of rows for an empty virtual table.

#### Components

- `virtual_row` - parenthesized tuple of expressions, one per row
- `expression` - any expression (literal, field reference, function call)
- `named_column_list` - output column names with type annotations
- `_` - empty marker (no rows)

#### Examples

Inline form with two rows:

```rust
# use substrait_explain::parser::Parser;
#
# let plan_text = r#"
=== Plan
Root[id, name]
  Read:Virtual[(1, 'alice'), (2, 'bob') => id:i64, name:string]
# "#;
#
# let plan = Parser::parse(plan_text).unwrap();
# assert_eq!(plan.relations.len(), 1);
```

Empty virtual table (no rows):

```rust
# use substrait_explain::parser::Parser;
#
# let plan_text = r#"
=== Plan
Root[id, name]
  Read:Virtual[_ => id:i64, name:string]
# "#;
#
# let plan = Parser::parse(plan_text).unwrap();
# assert_eq!(plan.relations.len(), 1);
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
URNs:
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

`"Aggregate" "[" grouping_sets "=>" aggregate_output "]"`

#### Components

- `grouping_sets := grouping_set_list / expression_list` - can be a list of grouping sets (each parenthesized), or a single unparenthesized list for the common, single-set case
- `grouping_set_list := grouping_set ("," grouping_set)*`
- `grouping_set := "(" expression_list ")" / "_"` - a grouping set can be (1) a list of expressions, or (2) `_`, the standard we use for empty lists
- `aggregate_output := (reference | aggregate_measure) ("," (reference | aggregate_measure))*` - comma-separated list of output items
- `aggregate_measure` - field references or aggregate function calls. See [Aggregate Measures section](#aggregate-measures)

#### Example

```rust
# use substrait_explain::parser::Parser;
#
# let plan_text = r#"
=== Extensions
URNs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml
Functions:
  ## 10 @  1: sum
  ## 11 @  1: count

=== Plan
Root[result]
  Aggregate[($0), ($0, $1) => $0, $1, sum($2), count($2)]           // Group by field 0, and ($0, $1)
    Read[orders => category:string, region:string?,  amount:i64]
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

### Join Relation

**Syntax**: `"Join" "[" join_type "," expression "=>" reference_list "]"`

**Components**:

- `join_type` - Join type enum with `&` prefix (e.g., `&Inner`, `&Left`, `&Right`, `&Outer`)
- `expression` - Join condition (boolean expression relating left and right inputs)
- `reference_list` - Comma-separated list of field references for output columns

**Field Reference Mapping**:

For joins, field references map to the combined schema of left and right inputs:

- `$0`, `$1`, ... refer to left input fields
- `$n`, `$n+1`, ... refer to right input fields (where n = number of left fields)

**Example**:

```rust
# use substrait_explain::parser::Parser;
#
# let plan_text = r#"
=== Extensions
URNs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_comparison.yaml
Functions:
  ## 10 @  1: eq

=== Plan
Root[user_orders]
  Join[&Inner, eq($0, $2) => $0, $1, $3]
    Read[users => id:i64, name:string]        // Fields $0, $1
    Read[orders => user_id:i64, amount:i32]   // Fields $2, $3
# "#;
#
# let plan = Parser::parse(plan_text).unwrap();
# assert_eq!(plan.relations.len(), 1);
```

### Extension Relations

Extension relations allow custom relation types with user-defined protobuf payloads. They enable integration with custom data sources, optimizations, or specialized operations beyond standard Substrait relations.

#### Types

There are three extension relation types, based on their input cardinality:

- **`ExtensionLeaf`** - No child relations (e.g., custom data sources)
- **`ExtensionSingle`** - Exactly one child relation (e.g., custom transformations)
- **`ExtensionMulti`** - Zero or more child relations (e.g., custom joins)

#### Syntax

```text
extension_relation := extension_type ":" name "[" (empty / extension_args)? ("=>" extension_columns)? "]"
extension_type := "ExtensionLeaf" / "ExtensionSingle" / "ExtensionMulti"
extension_args := (positional_args ("," named_args)?) / named_args
positional_args := extension_arg ("," extension_arg)*
extension_arg := reference / literal / expression
named_args := named_arg ("," named_arg)*
named_arg := name "=" extension_arg
extension_columns := extension_column ("," extension_column)*
extension_column := named_column / reference / expression
```

#### Components

- **`extension_type`** - One of `ExtensionLeaf`, `ExtensionSingle`, or `ExtensionMulti`
- **`name`** - The extension name (registered with `ExtensionRegistry`)
- **`empty`** (`_`) - Explicitly marks an extension with no arguments
- **`extension_args`** - Positional arguments (references, literals, expressions) and/or named arguments (`key=value` pairs); both are optional
- **`extension_columns`** - Output column definitions: named columns (`name:type`), field references (`$0`), or expressions

#### Examples

```text
=== Plan
Root[result]
  ExtensionSingle:CustomFilter[threshold=100 => $0, $1]
    ExtensionLeaf:ParquetScan[path='data/users.parquet', batch_size=1024 => id:i64, name:string]
```

Extension with positional arguments and no output columns:

```text
ExtensionSingle:VectorNormalize[$0, $1, method='l2']
```

Extension with no arguments:

```text
ExtensionLeaf:EmptySource[_]
```

#### Custom Extension Types

To use extension relations with custom protobuf payloads, register them with an `ExtensionRegistry`. See the API documentation for details on implementing the `Explainable` trait.

## Advanced Extensions

Advanced extensions allow attaching enhancement and optimization metadata to any standard relation via the Substrait `AdvancedExtension` protobuf field.

### Overview

Each relation can carry:

- **At most one** enhancement (`+ Enh:`) — extra semantic metadata attached to a relation
- **Zero or more** optimizations (`+ Opt:`) — hints for the query planner

### Syntax

```text
adv_extension := "+" adv_ext_type ":" name "[" (empty | extension_args)? "]"
adv_ext_type  := "Enh" | "Opt"
```

Where:

- **`adv_ext_type`** — `Enh` for an enhancement, `Opt` for an optimization
- **`name`** — the registered type name (e.g. `PartitionHint`)
- **`extension_args`** — positional and/or named arguments; use `_` for empty

Advanced extension lines are **indented one level deeper** than the relation they annotate, just like child relations. Enhancement and optimization lines MUST appear **before** any child relations.

### Argument Syntax

Extension arguments follow the same rules as extension-relation arguments.  Enum values are written with a `&` prefix:

```text
enum_value := "&" identifier
```

#### Examples

- `&HASH`, `&RANGE`, `&BROADCAST` — `PartitionStrategy` variants for `PartitionHint`
- `&AscNullsFirst` — sort direction enum in a relation argument

### Example: Enhancement on a Read Relation

```rust
# use substrait_explain::extensions::ExtensionRegistry;
# use substrait_explain::extensions::examples::PartitionHint;
# use substrait_explain::format_with_registry;
# use substrait_explain::parser::Parser;
#
# let mut registry = ExtensionRegistry::new();
# registry.register_enhancement::<PartitionHint>().unwrap();
# let parser = Parser::new().with_extension_registry(registry.clone());
#
# let plan_text = r#"
=== Plan
Root[result]
  Read[data => col:i64]
    + Enh:PartitionHint[&HASH, count=8]
# "#;
#
# let plan = parser.parse_plan(plan_text).unwrap();
# let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
# assert!(errors.is_empty());
# assert_eq!(formatted.trim(), plan_text.trim());
```

### Example: Enhancement and Multiple Optimizations

```text
=== Plan
Root[result]
  Read[data => col:i64]
    + Enh:PartitionHint[&HASH, count=4]
    + Opt:PlanHint[hint='use_index']
    + Opt:PlanHint[hint='parallel']
```

### Custom Extension Types

To parse or textify advanced extensions with custom protobuf payloads, register them with an `ExtensionRegistry`:

- **Enhancements**: `registry.register_enhancement::<MyEnhancement>()`
- **Optimizations**: `registry.register_optimization::<MyOptimization>()`

Both require implementing the `Explainable` trait, which provides `from_args` / `to_args` for text-format conversion, and `prost::Message + prost::Name` for protobuf serialization.

#### Parse failure behaviour

If a `+ Enh:` or `+ Opt:` name is **not registered** in the registry at parse time, the parser returns a hard error.

If the registry does not know the type URL at **textify** time (e.g. when formatting a plan received from an external source), the line is still emitted with a failure token and a `FormatError` is collected — the rest of the plan is unaffected.

For example, if a plan contains an enhancement whose type URL is not registered, the textified output replaces the name and arguments with `!{extension}`:

```text
=== Plan
Root[result]
  Read[my.table => col:i64]
    + Enh[!{extension}]
```

The collected `FormatError` carries the full detail:

```text
FormatError::Extension(ExtensionError::NotFound { type_url: "type.googleapis.com/acme.PartitionHint" })
```

The `Read` line and everything else in the plan are textified normally; only the unrecognized enhancement line degrades to the failure token.

## Complete Example

A complete query that joins users and orders tables, calculates total order value, filters for high-value orders, and groups by user to show total revenue per customer:

```rust
# use substrait_explain::parser::Parser;
#
# let plan_text = r#"
=== Extensions
URNs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_comparison.yaml
  @  2: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
  @  3: https://github.com/substrait-io/substrait/blob/main/extensions/functions_aggregate.yaml
Functions:
  ## 10 @  1: eq
  ## 11 @  1: gt
  ## 12 @  2: multiply
  ## 13 @  3: sum

=== Plan
Root[customer_revenue]
  Aggregate[$0, $1 => $0, $1, sum($3)]
    Filter[gt($3, 100) => $0, $1, $2, $3]
      Project[$0, $1, $2, multiply($4, $5)]
        Join[&Inner, eq($0, $3) => $0, $1, $2, $3, $4, $5]
          Read[users => id:i64, name:string, region:string]
          Read[orders => user_id:i64, quantity:i32, price:i64]
# "#;
#
# let plan = Parser::parse(plan_text).unwrap();
# assert_eq!(plan.relations.len(), 1);
```
