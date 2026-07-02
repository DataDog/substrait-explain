# Architecture

This document describes the structure and design of `substrait-explain` for contributors. It attempts to explains what each module does, how data flows through the system, and why the code is shaped the way it is. For the user-facing text format, see `GRAMMAR.md`. For design philosophy and compatibility expectations, see `DESIGN.md`. For contributor process, see `CONTRIBUTING.md`.

---

## Input and Output Types

`substrait-explain` is a bidirectional converter between two representations of a Substrait query plan:

- **Text format** — the human-readable format defined in `GRAMMAR.md`
- **Substrait protobuf** — the `proto::Plan` type from the `substrait` crate

The CLI (behind the `cli` feature flag) can accept JSON or YAML files. These are converted to `proto::Plan` by `src/json.rs` and    `src/cli.rs` at the CLI boundary before anything else touches them. Internally, `proto::Plan` is the handoff point between the two directions — there is no shared IR that both the parser and textifier use. 

---

## Module Map

```
src/
├── lib.rs              # Public API — parse() and format*() entry points
├── main.rs             # CLI binary
├── cli.rs              # CLI argument handling
├── json.rs             # JSON/YAML → proto::Plan (cli feature only)
├── grammar.rs          # Re-exports GRAMMAR.md as rustdoc / doctests
├── parser/             # Text → proto::Plan
├── textify/            # proto::Plan → Text
└── extensions/         # Extension lookup, registry, argument types
```
---

## `src/json.rs` — JSON to Proto

Substrait plans can be encoded as JSON, but two incompatible JSON formats exist. Rust's pbjson library encodes `google.protobuf.Any` fields as `{"typeUrl": "...", "value": "<base64>"}`. Go's protojson library uses the protobuf standard encoding: `{"@type": "...", field1: val, ...}` where the concrete message's fields are inlined. A plan produced by a Go system will fail to parse with the Rust pbjson deserializer. `json.rs` handles both. It tries the pbjson serde path first; if that fails it falls back to `prost-reflect`, which implements the full protobuf JSON mapping spec and can handle the Go format as long as a `DescriptorPool` containing the schema for every referenced type URL is provided. Both paths produce the same `proto::Plan`. When outputting JSON, the CLI always produces the pbjson format (`{"typeUrl": "...", "value": "<base64>"}`) via `serde_json`. There is no option to emit Go-style protojson.

The Go-format path requires a `DescriptorPool` with the schema for any extension types stored as `google.protobuf.Any` blobs — the Substrait core types are already bundled, but custom extension schemas must be provided. Callers do this by adding their compiled proto descriptor blob to the `ExtensionRegistry` via `add_descriptor`. When the CLI receives JSON input, it builds the pool from those registered blobs before calling `parse_json`. `build.rs` is an example of building the descriptor blob. It compiles the test proto at build time producing the binary descriptor and generated Rust types.

---

## `src/textify/` — Proto to Text

### Textify Summary

1. lib.rs — entry point,  exposes the public API formatting
2. textify/plan.rs —  resolves simple extension declarations, writes the Extensions section, then iterates relations
3. textify/foundation.rs — not a sequential step but the shared infrastructure everything below uses (Textify trait, Scope, error accumulation)
4. textify/rels.rs — writes each relation: header, then addenda, then children recursively
5. textify/expressions.rs — called from rels for Value::Expression and Value::AggregateFunction
6. textify/types.rs — called from expressions and rels for type annotations, names, anchors
7. textify/extensions.rs — called from rels for custom extension argument values (Value::ExtensionArgument, Value::ExtColumn)
8. textify/addenda.rs — called from rels for + Enh:, + Opt:, + Ext: lines, which call into extensions.rs for rendering the decoded args

### Entry Point

The public entry points in `lib.rs` are `format()`, `format_with_options()`, and `format_with_registry()`. All three ultimately call `format_with_registry`, which is the real implementation. `format()` and `format_with_options()` are convenience wrappers that supply a default empty `ExtensionRegistry`. The registry is always required even as a default, because advanced extensions; Enhancements(`+ Enh:`), Optimizations(`+ Opt:`), and extension relations(LeafRel, SingleRel, MultiRel), carry their payload as a `google.protobuf.Any` blob. The registry knows how to decode those blobs into readable text. 

### `textify/plan.rs` — Top-Level Output

This file is responsible for the overall structure of the output text and writes the Extensions and Plan sections.

### `textify/foundation.rs` — Shared Infrastructure

This file defines the core  shared infrastructure that every other textify file depends on:
The `Textify` trait is implemented by every type that can be rendered to text and describes how they should be written to text. 
The `Scope` trait is the context object carried through every `textify` call. Carrying output options, extension registry, error accumulator etc.. 

### `textify/rels.rs` — Relation Output

Substrait proto relation types (Read, Filter, Project...) are converted to a single internal `Relation` shape before rendering. 
Proto relations can be represented in their arguments, columns output mapping(Emit), addenda(advanced extensions), and children; which we store in Relation internal type.

**`Value`** is the universal type for anything that can appear in a relation's argument list or output columns. 

**`Arguments`** separates positional from named arguments. Positional arguments are rendered in order; named arguments render as key=value pairs. Relations use named arguments when field labels are needed for clarity — e.g. fetch=10, offset=5 — and positional when order alone is unambiguous.

**`Emitted`** holds both the full direct-output column list and the `RelCommon` emit mapping. `Emitted` keeps that separation intact and applies the index remapping at render time.

**`ValueEnum`** handles proto enum fields, which arrive as raw `i32` values from prost.

### `textify/expressions.rs` — Expression Rendering

Called from `rels.rs` to render `Value::Expression` and `Value::AggregateFunction`. Handles Substrait expression types.

### `textify/types.rs` — Type and Name Rendering

Called from `expressions.rs` and `rels.rs` whenever a type annotation, column name, or extension name needs to be rendered. 

### `textify/extensions.rs` — Extension Argument Rendering

Called from `rels.rs` and `addenda.rs` to render  advanced extension arguments. This file gives `ExtensionValue`, `ExtensionArgs`, `ExtensionColumn`, and `Expr` their `Textify` implementations.

The key intermediate type is **`ExtensionArgs`** (defined in `src/extensions/args.rs`). When the textifier encounters an advanced extension payload — a `google.protobuf.Any` blob — the `ExtensionRegistry` decodes it into `(name, ExtensionArgs)`: a text-format name and a generic argument bag holding positional `ExtensionValue`s, named `ExtensionValue`s, and `ExtensionColumn`s for output columns.

### `textify/addenda.rs` — Addendum Line Rendering

Called from `rels.rs` after the relation header, before child relations. Addendum lines (`+ Enh:`, `+ Opt:`, `+ Ext:`) attach metadata to a relation without changing its structural position in the plan tree.

---

## `src/parser/` — Text to Proto

The parser converts text to `proto::Plan` in three phases, all orchestrated by `structural.rs`.

**Phase 1 — Extensions**: `structural.rs` reads the `=== Extensions` section first, in full, building the `SimpleExtensions` anchor table. Every function, type, and variation anchor is resolved before any relation is parsed.

**Phase 2 — Tree building**: `structural.rs` reads the `=== Plan` section line by line. For each line, it calls `ChunkCursor` (from `chunks.rs`) to merge any continuation lines into a single complete chunk. Each chunk is then run through the PEG grammar (via [Pest](https://pest.rs)), producing a **`Pair<Rule>`**.

A **`Pair<Rule>`** is Pest's fundamental match type: it records which grammar rule matched, the text it covers, and a list of inner `Pair`s for sub-expressions within that match. For example, parsing `Read[my_table => a:i64, b:string?]` produces a `Pair` for the `read_relation` rule with two inner pairs: a `table_name` pair covering `my_table`, and a `named_column_list` pair covering `a:i64, b:string?`. That `named_column_list` pair itself contains two `named_column` inner pairs — one for `a:i64`, one for `b:string?` — each of which contains a `name` pair and a `type` pair. This inner-pair nesting captures the structure within a single line — the parent-child relationships between relations come from indentation, not the grammar.

`structural.rs` wraps each top-level `Pair` into a **`LineNode`**: either a **`RelationNode`** (a relation line) or an **`Addendum`** (a `+ Enh:` / `+ Opt:` / `+ Ext:` line). `RelationNode` holds the pair, its line number, `addenda` and `children` lists. **`TreeBuilder`** then places each `LineNode` into the tree by indentation depth.

**Phase 3 — Proto conversion**: once all lines are consumed, the completed `RelationNode` trees are walked depth-first, leaves first, then parents. This order is required because `RelationParsePair` (the trait each relation type implements) receives its children as already-converted `Rel` messages, and the field count flowing up from them, both of which are only available after the subtree below is resolved.

### Parser Summary

- `lib.rs` — entry point,  exposes the public API for parsing
- `expression_grammar.pest` — defines the grammar rules every file below depends on
- `parser/mod.rs` — re-exports types from the submodules
- `parser/chunks.rs` — provides the mechanism for grouping physical lines into chunks
- `parser/structural.rs` — hub; orchestrates all three phases: extensions, tree building, proto conversion
- `parser/extensions.rs` — parses simple extension declarations and addendum lines
- `parser/relations.rs` — converts each `RelationNode` into a `Rel` proto message
- `parser/expressions.rs` — called from relations to parse expression arguments
- `parser/types.rs` — called from expressions and relations to parse type annotations
- `parser/common.rs` — shared parsing traits and infrastructure used throughout

### Entry Point

The public entry points in `lib.rs` are `parse()` and `parse_with_registry()`. Unlike the textify side where `format()` and `format_with_options()` are thin wrappers that delegate to `format_with_registry`, here `parse()` does not delegate to `parse_with_registry()` — they are separate implementations.

The parser carries an `ExtensionRegistry` for the same reason as the textifier: advanced extension payloads (`+ Enh:`, `+ Opt:`, `+ Ext:`, and extension relations) are `google.protobuf.Any` blobs. `parse()` uses a default empty registry through `Parser::new()`, while `parse_with_registry()` supplies a caller-provided registry. The difference is in error handling: when textifying, an unregistered advanced extension can be represented as a soft error token output. When parsing, an unregistered advanced extension is a hard `ParseError::UnregisteredExtension`, because the parser cannot create a valid `Any` payload without the registered type. Plans with advanced extensions therefore need `parse_with_registry()`.

### `expression_grammar.pest` — PEG Grammar

The canonical grammar for the text format. Covers relation line syntax (one rule per relation type), argument lists, expression forms, etc... Section markers (`=== Plan`, `=== Extensions`) are not grammar rules — they are matched as string constants in `structural.rs`.

### `parser/chunks.rs` — Line Grouping

`ChunkCursor` provides the mechanism for grouping physical lines from the text into chunks. The policy of what to merge — recognizing continuation lines prefixed with `- ` and deciding when a chunk is complete — belongs to `structural.rs`, which drives the cursor.

### `parser/structural.rs` — Section Routing and Tree Construction

The main orchestrator. Matches `=== Extensions` and `=== Plan` as string constants to switch between sections, drives `ChunkCursor` for line grouping, routes extension declarations to `extensions.rs`, and builds the `RelationNode` tree via `TreeBuilder`.

### `parser/extensions.rs` — Extension and Addendum Parsing

Handles two distinct jobs: parsing URN declarations and anchor assignments in the Extensions section to produce the `SimpleExtensions` table, and parsing addendum lines (`+ Enh:`, `+ Opt:`, `+ Ext:`) attached to plan relations, decoding their names and arguments through the `ExtensionRegistry`.

### `parser/relations.rs` — Relation Construction

Converts each `RelationNode` into a `Rel` proto message. Implements the conversion for supported relation types: Read, Filter, Project etc... Also handles emit mapping — reconstructing the `RelCommon` output column remapping from the text representation.

Each relation type implements **`RelationParsePair`**, a third parsing trait alongside `ParsePair` and `ScopedParsePair`, which additionally accepts pre-converted child `Rel` messages and propagates output field counts up the tree.

`RelationParsingContext` is the context object used when a relation or addendum must turn parsed `ExtensionArgs` into a `google.protobuf.Any` payload. It carries the `ExtensionRegistry` plus source location so registry failures become contextual `ParseError`s.


### `parser/expressions.rs` — Expression Parsing

Called from `relations.rs` to parse supported expression arguments types into `proto::Expression` messages.

### `parser/types.rs` — Type Parsing

Called from `expressions.rs` and `relations.rs` to parse type annotations into `proto::Type` messages.

### `parser/common.rs` — Shared Infrastructure

Defines the parsing traits used throughout the parser — the parser-side equivalent of the `Textify` trait. There are three, each handling a different level of context dependency.

**`ParsePair`** converts a pest grammar match into a Rust type for constructs whose meaning is fully determined by syntax — literals, operators, nullability suffixes — with no anchor resolution needed.

**`ScopedParsePair`** does the same but takes a `&SimpleExtensions` context argument. It is used for constructs that require anchor lookups — function calls, type references, type variations — where the conversion can fail with a `MessageParseError` if the anchor is not in the table.

**`RuleIter`** wraps Pest's pair iterator with `try_pop` (consume the next pair only if it matches a specific rule, otherwise leave it) and `pop` (consume and assert).

---

## `src/extensions/` — Extension Support

Both simple and advanced extension code is found in this folder.

**Simple Extensions** are the Substrait standard mechanism for declaring custom functions, types, and type variations. In the text format these appear in the `=== Extensions` section as URN declarations and anchor assignments. `SimpleExtensions` is the in-memory lookup table built from those declarations — mapping integer anchors to qualified names and back.

**Advanced Extensions** are custom relation types, enhancements (`+ Enh:`), optimizations (`+ Opt:`), and extension table reads (`+ Ext:`, an addendum attached to a `ReadRel`) that are stored as a `google.protobuf.Any` blob in the proto. `substrait-explain` cannot parse or textify them from the plan alone. The caller supplies an `ExtensionRegistry` with registered Rust types that knows both the text representation and the protobuf `Any` representation.

**`AnyConvertible`** handles proto serialization. `to_any()` encodes the Rust type into a `google.protobuf.Any` blob; `from_any()` decodes it back to custom type. For prost-generated types (`prost::Message + prost::Name + Default`), this is provided automatically via a blanket impl. Custom types implement it manually.

**`Explainable`** handles text format conversion. `to_args()` converts the Rust type into an `ExtensionArgs` for the textifier to render; `from_args()` constructs the type from parsed `ExtensionArgs`.

A type registers as an extension by implementing both `AnyConvertible` and `Explainable`. **`Extension`** is an empty supertrait that groups them into a single bound used by the registry.

Together, the textifier path of a registered type is: `AnyRef` → `AnyConvertible::from_any` → Rust type → `Explainable::to_args` → `ExtensionArgs`. The parse path is the reverse: `ExtensionArgs` → `Explainable::from_args` → Rust type → `AnyConvertible::to_any` → proto `Any`

### File Map

- `extensions/simple.rs` — `SimpleExtensions`: anchor to name lookup table for functions, types, and type variations
- `extensions/registry.rs` — `ExtensionRegistry`: user-provided handler registry for advanced extension payloads
- `extensions/args.rs` — `ExtensionArgs`: the structured intermediate type that bridges text format and proto blobs
- `extensions/any.rs` — `Any` / `AnyRef`: owned and borrowed wrappers around `google.protobuf.Any`
- `extensions/examples.rs` — example of `Explainable` implementations used in documentation and tests

### `extensions/simple.rs` — Anchor Lookup

`SimpleExtensions` is the anchor lookup table built from the `=== Extensions` section. It maps integer anchors to URN strings a
nd qualified names for functions, types, and type variations.

### `extensions/registry.rs` — Advanced Extension Registry

`ExtensionRegistry` maps registered types to handlers that know how to convert between their `google.protobuf.Any` blob and their text representation.

### `extensions/args.rs` — Extension Arguments

`ExtensionArgs` is the structured intermediate that extension handlers read and write — a collection of positional values, named values, and output column declarations. `ExtensionValue` covers the scalar and expression types that can appear as argument values. Extension relations that declare an output schema use `ExtensionColumn` to describe it, which converts to and from the `NamedStruct` proto type.

Handlers read from `ExtensionArgs` via an `ArgsExtractor`, which tracks which arguments have been consumed. This enforces that no unexpected arguments are silently ignored — the extractor errors if unconsumed named arguments remain after parsing, catching mismatches between what the text format provides and what the handler expects.

### `extensions/any.rs` — Any Wrapper

`prost_types::Any` is always owned — there is no borrowed form. When the textifier encounters an extension blob inside a proto struct it only has a reference to it, not ownership. Passing that blob to `from_any` without a local borrowed type would require cloning it every time. `AnyRef<'a>` solves this without cloning. Created from a reference to a `prost_types::Any`. `Any` (owned) is used when ownership is required, such as the return type of `to_any`. Using these local types in the `AnyConvertible` API also means extension implementors do not need to depend on prost directly.