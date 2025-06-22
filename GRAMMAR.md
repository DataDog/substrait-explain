# Substrait Text Format Grammar

This document describes the grammar for the human-readable Substrait text format used by `substrait-explain`. This format allows you to write Substrait query plans in a concise, readable text format that can be parsed back into full Substrait protobuf plans.

## Overview

The Substrait text format consists of two main sections:

1. **Extensions Section** (optional) - Defines URIs and function/type extensions
2. **Plan Section** - Contains the actual query plan with relations

## Design Principles

The grammar is designed around several concrete choices that make it practical and consistent:

### 1. **Single-Line Relations with Consistent Pattern**

All relations follow the same structure: `Name[arguments => columns]`

- **Name**: The relation type (Read, Filter, Project, etc.)
- **Arguments**: Input expressions, field references, or function calls
- **Arrow**: `=>` separates arguments from output columns
- **Columns**: Output column names and types

Every relation fits on one line with indentation showing hierarchy. This uniform pattern makes it easy to parse any relation, understand input/output structure, and add new relation types.

### 2. **SQL-Like Field References and Literals**

- Uses `$0`, `$1`, `$2` for field references (like SQL's positional references)
- Types are shown inline with literals and column names: `42:i64`, `'hello':string`
- Nullability is explicit: `string?` for nullable, `string` for non-nullable

This prevents ambiguity and makes plans self-documenting while being familiar to SQL developers.

### 3. **Extension Support and Structured Syntax**

- Extensions section defines URIs and function mappings
- Function calls can include anchors: `add#10@1($0, $1)`
- Clear structural boundaries: `[]` for relations, `<>` for types, `()` for functions

This maintains full Substrait compatibility while keeping the text format readable and parseable.

### 4. **Hierarchical Organization**

- Section headers (`===`) separate major components
- 2-space indentation shows query plan hierarchy
- Consistent formatting across all document elements

This ensures the format is consistent, parseable, readable, extensible, and compatible with Substrait protobuf.

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

```
=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
Functions:
  # 10 @  1: add
  # 11 @  1: multiply

=== Plan
Root[result]
  Project[$0, $1, add($0, $1)]
    Read[orders => quantity:i32?, price:i64]
```

## Document Structure

A Substrait text format document consists of two main sections with specific formatting rules.

### Sections

The document uses `===` headers to separate major sections:

- **`=== Extensions`** - Defines URIs and function/type mappings (optional)
- **`=== Plan`** - Contains the actual query plan (required)

#### Extension format

```
=== Extensions
URIs:
  @  uri_anchor: uri
  …
Functions:
  #  anchor @  uri_anchor: name
  …
Types:
  #  anchor @  uri_anchor: name
  …
Type Variations:
  #  anchor @  uri_anchor: name
  …
```

Where `anchor` and `uri_anchor` are integers, `uri` is a text URI, and function, type, and type variation names are identifiers or quoted text.

### Plan Hierarchy and Indentation

Relations use indentation to show the query plan hierarchy:

- **Root level**: No indentation (typically `Root` relation)
- **Child relations**: Indented with 2 spaces per level
- **Each relation**: On its own line with format `Name[arguments => columns]`

**Example**:

```
=== Plan
Root[result]                    // Level 0 (no indentation)
  Project[$0, $1]              // Level 1 (2 spaces)
    Filter[gt($0, 10) => $0]   // Level 2 (4 spaces)
      Read[data => a:i64]      // Level 3 (6 spaces)
```

## Basic Terminals

### `name` and `identifier`

- **`name`**: `identifier / quoted_name`
  - Used for column names, function names, etc. It can be unquoted if it's a valid identifier, or using "double quotes" if special characters are required (much like SQL)
  - Examples: `function_name`, `"quoted name"`
- **`identifier`**: `letter (letter / digit / "_")*`
  - Used for columns, function names, etc. that are proper identifiers.
  - Examples: `table_name`, `my_function`, `col1`
- **`quoted_name`**: `'"' ("\\" . / !'"' .)* '"'`
  - Used for columns, function names, etc. that are not valid as identifiers, and thus need quoting.
  - Examples: `"function name"`, `"table.name"`, `"table\.name"`, `"function \"with some\nescapes\""`

### `literal`

- **`integer`**: `"-"? digit+`
  - Examples: `42`, `-10`, `0`
- **`float`**: `"-"? digit+ "." digit+`
  - Examples: `3.14`, `-2.5`, `1.0`
- **`boolean`**: `"true" / "false"`
  - Examples: `true`, `false`
- **`string_literal`**: `"'" ("\\" . / !"'" .)* "'"`
  - Examples: `'hello'`, `'table name'`, `'C:\path\to\file'`, `'line1\nline2'`, `'quote\'s here'`
- **Typed literals**: `string_literal ":" type`
  - String literals with type annotations for non-primitive types
  - Examples: `'2023-01-01':date`, `'2023-12-25T14:30:45.123':timestamp`

**TODO**: The current Pest grammar only supports `integer` and `string_literal`. The grammar needs to be extended to support `float`, `boolean`, and typed literals as described above.

## Types

The type syntax in this grammar generally follows the [standard Substrait type definition syntax](https://substrait.io/types/type_parsing/), with some modifications for user-defined types (UDTs) and our specific text format.

### Standard Substrait Type Syntax

The standard Substrait type syntax follows this pattern:

```
name?<param0,...,paramN>
```

_Or with PEG Syntax_: `name "?"? "<" (param ("," param)*)? ">"`

Where:

- **name**: Type name (case-insensitive, lowercase preferred)
- **?**: Optional nullability indicator (defaults to non-nullable)
- **<param0,...,paramN>**: Optional parameters (types, integers, etc.)

### Modifications in This Grammar

This grammar makes the following modifications to the standard syntax:

**Common Components**:

- **`anchor`**: `"#" integer` - Extension anchor (e.g., `#10`)
- **`uri_anchor`**: `"@" integer` - URI anchor (e.g., `@1`)
- **`param`**: `type / integer / name` - Type parameter (can be a type, integer, or name)
- **`parameters`**: `"<" (param ("," param)*)? ">"` - Type parameters in angle brackets

**Comparison with Standard Substrait**:

1. **User-Defined Types (UDTs)**: Extended syntax to support anchors and URI references

   - Standard: `"u!" name nullability? parameters?`
   - This grammar: `"u!"? name anchor? uri_anchor? nullability? parameters?`
   - **Same**: `u!` prefix, name, nullability, parameters
   - **Different**: This grammar adds optional `anchor` and `uri_anchor` for extension references, and the `u!` prefix is optional

2. **Struct Syntax**: Follows standard Substrait struct syntax

   - Definition: `"struct" nullability? parameters`

3. **Parameterized Types**: Uses the same syntax and literals as standard Substrait
   - Standard: `FIXEDCHAR`, `VARCHAR`, `FIXEDBINARY`, `DECIMAL`, `INTERVAL_DAY`, `PRECISION_TIME`, `PRECISION_TIMESTAMP`, etc. (case-insensitive)
   - This grammar: Same syntax and literals, but currently only implements `LIST`, `MAP`, `STRUCT` (and outputs lower-case by default)

### Simple Types

Simple types are the basic Substrait types with optional nullability.

**Syntax**: `simple_type_name nullability?`

**Simple Type Names** (from [official Substrait grammar](https://raw.githubusercontent.com/substrait-io/substrait/refs/heads/main/grammar/SubstraitType.g4)):

- `boolean`, `i8`, `i16`, `i32`, `i64`
- `fp32`, `fp64`
- `string`, `binary`
- `timestamp`, `timestamp_tz`, `date`, `time`
- `interval_year`, `uuid`

**Note**: The official grammar is case-insensitive, but lowercase is preferred here.

**Nullability**:

- `?` - nullable
- `⁉` - unspecified nullability
- (nothing) - non-nullable

**Examples**:

```
i64        // non-nullable 64-bit integer
string?    // nullable string
```

### Compound Types

Compound types follow the same syntax as standard Substrait parameterized types.

**Examples**:

```
list<i64>                    // list of non-nullable integers
map<string, i64>             // map from string to integer
struct<i64, string?>         // struct with two fields
```

### User-Defined Types

**Syntax**: `"u!"? name anchor? uri_anchor? nullability? parameters?`

**Examples**:

```
point#8@2?<i8>     // user-defined type with anchor and URI
my_type#10         // user-defined type with anchor only
u!custom_type      // user-defined type with u! prefix
```

## Expressions

### Field References

Currently, only references to fields in the Relations' input are supported.

**Syntax**: `"$" integer`

**Examples**:

```
$0    // reference to field 0
$1    // reference to field 1
$42   // reference to field 42
```

### Function Calls

**Syntax**: `name anchor? uri_anchor? "(" (expression ("," expression)*)? ")" (":" type)?`

**Components**:

- `name` - function name
- `anchor` - optional anchor (e.g., `#10`)
- `uri_anchor` - optional URI anchor (e.g., `@1`)
- `argument_list` - required parentheses with comma-separated expressions
- `type` - optional output type

**Examples**:

```
add($0, $1)                    // simple function call
add#10@1($0, $1)              // function with anchor and URI
multiply($0, $1):i64          // function with output type
```

### Expression Grammar

**Syntax**: `function_call / reference / literal`

**Examples**:

```
$0                    // field reference
42                    // literal
multiply($0, 10)      // function call
```

## Relations

Relations represent the operations in a query plan. Each relation is displayed on a single line with indentation showing the hierarchy.

### General Relation Grammar

All relations follow this general pattern:

```
RelationName[arguments => columns]
```

Where:

- **RelationName**: The type of operation (Read, Filter, Project, Root, etc.)
- **arguments**: Input expressions, field references, function calls, or other parameters (optional)
- **=>**: Separator between arguments and output columns (optional, only present when both arguments and columns are specified)
- **columns**: Output column names and types, or field references for pass-through (all relations specify outputs, but format varies)

**Examples of the pattern**:

```
Read[table_name => col1:type1, col2:type2]           // arguments: table, columns: named types
Filter[boolean_expr => $0, $1, $2]                   // arguments: expression, columns: field refs
Project[expr1, expr2]                                // arguments: expressions, no => separator
Root[output_names]                                   // special case: just output names, no arguments
```

**Special cases**:

- **Root relation**: Only specifies output column names, no arguments or `=>` separator
- **Project relation**: Only specifies expressions, no `=>` separator or output columns
- Some relations may use '...' instead of column names when they pass through all fields

The exact structure varies by relation type, but all follow this basic pattern.

### Root Relation

**Syntax**: `"Root" "[" (name ("," name)*)? "]"`

**Example**:

```
Root[c, d]           // root with output columns c and d
```

### Read Relation

**Syntax**: `"Read" "[" table_name "=>" named_column_list "]"`

**Components**:

- `table_name` - name of the table to read
- `named_column_list` - comma-separated list of named columns with types

**Examples**:

```
Read[schema.table => a:i64, b:string?]
Read[orders => quantity:i32?, price:i64]
```

### Filter Relation

**Syntax**: `"Filter" "[" expression "=>" reference_list "]"`

**Components**:

- `expression` - boolean expression for filtering
- `reference_list` - comma-separated list of field references to pass through

**Example**:

```
Filter[gt($2, 100) => $0, $1, $2]
```

### Project Relation

**Syntax**: `"Project" "[" (expression ("," expression)*)? "]"`

**Components**:

- `expression` - field reference, function call, or literal (see Expressions section)

**Example**:

```
Project[$1, 42]                    // project field 1 and literal 42
```

## Complete Example

A complete query that reads from an orders table, multiplies quantity and price, filters for high-value orders, and outputs the revenue:

```
=== Extensions
URIs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_arithmetic.yaml
Functions:
  # 10 @  1: add
  # 11 @  1: multiply
  # 12 @  1: gt

=== Plan
Root[revenue]
  Filter[gt($2, 100) => $0, $1, $2]
    Project[$0, $1, multiply($0, $1)]
      Read[orders => quantity:i32?, price:i64]
```
