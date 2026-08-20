# Architecture

This document describes the structure and design of `substrait-explain` for contributors. It attempts to explains what each module does, how data flows through the system, and why the code is shaped the way it is. For the user-facing text format, see `GRAMMAR.md`. For design philosophy and compatibility expectations, see `DESIGN.md`. For contributor process, see `CONTRIBUTING.md`.

---

## Input and Output Types

`substrait-explain` is a bidirectional converter between two representations of a Substrait query plan:

- **Text format** — the human-readable format defined in `GRAMMAR.md`
- **Substrait protobuf** — the `proto::Plan` type from the `substrait` crate

The CLI(behind the `cli` feature flag) can accept text, JSON, protobuf, or YAML files. `cli.rs` routes each format and converts non-text input into `proto::Plan` at the boundary. JSON is handled in `json.rs`. Where we normalize Rust pbjson and Go protojson encodings to `proto::Plan`. Internally, `proto::Plan` is the handoff point between the two directions — there is no shared IR that both the parser and textifier use.

---

## Module Map

```
src/
├── lib.rs              # Public API — parse() and format*() entry points
├── main.rs             # CLI binary
├── cli.rs              # CLI argument handling and routing for text, JSON, YAML, and protobuf formats
├── json.rs             # JSON → proto::Plan (cli feature only), including pbjson and Go protojson Any handling
├── grammar.rs          # Re-exports GRAMMAR.md as rustdoc / doctests
├── fixtures.rs         # Shared test helpers (cfg(test) only)
├── types_tests.rs      # Type roundtrip tests (cfg(test) only)
├── parser/             # Text → proto::Plan
├── textify/            # proto::Plan → Text
└── extensions/         # Extension lookup, registry, argument types
```

---

## `src/json.rs` — JSON to Proto

Substrait plans can be encoded as JSON, but two incompatible JSON formats exist. Rust's pbjson library encodes `google.protobuf.Any` fields as `{"typeUrl": "...", "value": "<base64>"}`. Go's protojson library uses the protobuf standard encoding: `{"@type": "...", field1: val, ...}` where the concrete message's fields are inlined. A plan produced by a Go system will fail to parse with the Rust pbjson deserializer. `json.rs` handles both. It tries the pbjson serde path first; if that fails it falls back to `prost-reflect`, which implements the full protobuf JSON mapping spec and can handle the Go format as long as a `DescriptorPool` containing the schema for every referenced type URL is provided. Both paths produce the same `proto::Plan`. When outputting JSON, the CLI always produces the pbjson format (`{"typeUrl": "...", "value": "<base64>"}`) via `serde_json`. There is no option to emit Go-style protojson.

The Go-format path requires a `DescriptorPool` with the schema for any extension types stored as `google.protobuf.Any` blobs — the Substrait core types are already bundled, but custom extension schemas must be provided. Callers do this by adding their compiled proto descriptor blob to the `ExtensionRegistry` via `add_descriptor`. When the CLI receives JSON input, it builds the pool from those registered blobs before calling `parse_json`. `build.rs` demonstrates this pattern: it compiles a test proto at build time to produce the descriptor blob and generated Rust types used in tests.

---

## `src/textify/` — Proto to Text

Textifying means to generate substrait-explain text from a plan. A plan shouldn't fail to produce output for unsupported proto. The textifier writes what it can and pushes problems onto a shared ErrorQueue (textify/foundation.rs), which the caller can inspect after the fact (FormatError, PlanError).

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

The public entry points in `lib.rs` are `format()`, `format_with_options()`, and `format_with_registry()`. All three ultimately call `format_with_registry`, which is the real implementation. `format()` and `format_with_options()` are convenience wrappers that supply a default empty `ExtensionRegistry`. The registry is always required because advanced extensions: Enhancements(`+ Enh:`), Optimizations(`+ Opt:`), and extension relations(LeafRel, SingleRel, MultiRel), carry their payload as a `google.protobuf.Any` blob. The registry knows how to decode those blobs into readable text.

### `textify/plan.rs` — Top-Level Output

This file is responsible for the overall structure of the output text and writes the Extensions and Plan sections.

### `textify/foundation.rs` — Shared Infrastructure

This file defines the core shared infrastructure that every other textify file depends on:
The `Textify` trait is implemented by every type that can be rendered to text and describes how they should be written to text. 
The `Scope` trait is the context object carried through every `textify` call. Carrying output options, extension registry, error accumulator etc.. 

### `textify/rels.rs` — Relation Output

At the top level, a Substrait `Plan` contains `PlanRel` entries. Each `PlanRel` is either a `Root` or a regular `Rel`. The textifier handles `Root` separately in the `Textify for RelRoot` implementation. Regular protobuf relation types such as `Read`, `Filter`, and `Project` are converted into the `Relation` shape before rendering. Proto relations can be represented by their arguments, columns output mapping(Emit), addenda(advanced extensions), and children; which we store in the Relation internal type.

**`Value`** is the universal type for anything that can appear in a relation's argument list or output columns. 

**`Arguments`** separates positional from named arguments. Positional arguments are rendered in order; named arguments render as key=value pairs. Relations use named arguments when field labels are needed for clarity — e.g. fetch=10, offset=5 — and positional when order alone is unambiguous.

**`Emitted`** holds both the full direct-output column list and the `RelCommon` emit mapping. `Emitted` keeps that separation intact and applies the index remapping at render time.

**`ValueEnum`** handles proto enum fields, which arrive as raw `i32` values from prost.

### `textify/expressions.rs` — Expression Rendering

Called from `rels.rs` to render `Value::Expression` and `Value::AggregateFunction`. Handles Substrait expression types.

### `textify/types.rs` — Type and Name Rendering

Called from `expressions.rs` and `rels.rs` whenever a type annotation, column name, or extension name needs to be rendered. 

### `textify/extensions.rs` — Extension Argument Rendering

Called from `rels.rs` and `addenda.rs` to render advanced extension arguments. This file gives `ExtensionValue`, `ExtensionArgs`, `ExtensionColumn`, and `Expr` their `Textify` implementations.

The key intermediate type is **`ExtensionArgs`** (defined in `src/extensions/args.rs`). When the textifier encounters an advanced extension payload — a `google.protobuf.Any` blob — the `ExtensionRegistry` decodes it into `(name, ExtensionArgs)`: a text-format name and a generic argument bag holding positional `ExtensionValue`s, named `ExtensionValue`s, and `ExtensionColumn`s for output columns.

### `textify/addenda.rs` — Addendum Line Rendering

Called from `rels.rs` after the relation header, before child relations. Addendum lines (`+ Enh:`, `+ Opt:`, `+ Ext:`) attach metadata to a relation without changing its structural position in the plan tree.

---

## `src/parser/` — Text to Proto

The parser converts text to `proto::Plan` in three phases, all orchestrated by `structural.rs`.

**Phase 1 — Extensions**: `structural.rs` reads the `=== Extensions` section first, in full, building the `SimpleExtensions` anchor table. Every function, type, and variation anchor is resolved before any relation is parsed.

**Phase 2 — Tree building**: `structural.rs` reads the `=== Plan` section line by line. For each line, it calls `ChunkCursor` (from `chunks.rs`) to merge any continuation lines into a single complete chunk. Each chunk is then run through the PEG grammar (via [Pest](https://pest.rs)), producing a **`Pair<Rule>`**.

A **`Pair<Rule>`** is Pest's fundamental match type: it records which grammar rule matched, the text it covers, and a list of inner `Pair`s for sub-expressions within that match. For example, parsing `Read[my_table => a:i64, b:string?]` produces a `Pair` for the `read_relation` rule with two inner pairs: a `table_name` pair covering `my_table`, and a `named_column_list` pair covering `a:i64, b:string?`. That `named_column_list` pair itself contains two `named_column` inner pairs — one for `a:i64`, one for `b:string?` — each of which contains a `name` pair and a `type` pair. This inner-pair nesting captures the structure within a single line — the parent-child relationships between relations come from indentation, not the grammar.

`structural.rs` wraps each top-level `Pair` into a **`LineNode`**: either a **`RelationNode`** (a relation line) or an **`Addendum`** (a `+ Enh:` / `+ Opt:` / `+ Ext:` line). `RelationNode` holds the pair, its line number, `addenda` and `children` list. **`TreeBuilder`** then places each `LineNode` into the tree by indentation depth.

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
- `parser/errors.rs` — defines `ParseError`, `ParseContext`, and `ParseResult`: the public error types returned by `parse()` and `parse_with_registry()`

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

Each relation type implements **`RelationParsePair`**, a third parsing trait alongside `ParsePair` and `ScopedParsePair`(both described in Shared Infrastructure), which additionally accepts pre-converted child `Rel` messages and propagates output field counts up the tree.

`RelationParsingContext` is the context object used when a relation or addendum must turn parsed `ExtensionArgs` into a `google.protobuf.Any` payload. It carries the `ExtensionRegistry` plus source location so registry failures become contextual `ParseError`s.


### `parser/expressions.rs` — Expression Parsing

Called from `relations.rs` to parse supported expression arguments types into `proto::Expression` messages.

### `parser/types.rs` — Type Parsing

Called from `expressions.rs` and `relations.rs` to parse type annotations into `proto::Type` messages.

### `parser/common.rs` — Shared Infrastructure

Defines the parsing traits used throughout the parser — the parser-side equivalent of the `Textify` trait. There are three, each handling a different level of context dependency.

**`ParsePair`** converts a pest grammar match into a Rust type for constructs whose meaning is fully determined by syntax — literals, operators, nullability suffixes — with no anchor resolution needed.

**`ScopedParsePair`** does the same but takes a `&SimpleExtensions` context argument. It is used for constructs that require anchor lookups — function calls, type references, type variations — where the conversion can fail with a `MessageParseError` if the anchor is not in the table.

**`RuleIter`** helps parser code walk through Pest's nested parse results in the order the grammar defines them.

---

## `src/extensions/` — Extension Support

Both simple and advanced extension code is found in this folder.

**Simple Extensions** are the Substrait standard mechanism for declaring custom functions, types, and type variations. In the text format these appear in the `=== Extensions` section as URN declarations and anchor assignments. `SimpleExtensions` is the lookup table built from those declarations — mapping integer anchors to qualified names and back.

**Advanced Extensions** are custom relation types, enhancements (`+ Enh:`), optimizations (`+ Opt:`), and extension table reads (`+ Ext:`, an addendum attached to a `ReadRel`) that are stored as a `google.protobuf.Any` blob in the proto. `substrait-explain` cannot parse or textify them from the plan alone. The caller supplies an `ExtensionRegistry` with registered Rust types that knows both the text representation and the protobuf `Any` representation.

**`AnyConvertible`** handles proto serialization. `to_any()` encodes the Rust type into a `google.protobuf.Any` blob; `from_any()` decodes it back to custom type. For prost-generated types (`prost::Message + prost::Name + Default`), this is provided automatically via a blanket impl. Custom types implement it manually.

**`Explainable`** handles text format conversion. `to_args()` converts the Rust type into an `ExtensionArgs` for the textifier to render; `from_args()` constructs the type from parsed `ExtensionArgs`.

A type registers as an advanced extension by implementing both `AnyConvertible` and `Explainable`. **`Extension`** is an empty supertrait that groups them into a single bound used by the registry.

Together, the textifier path of a registered type is: `AnyRef` → `AnyConvertible::from_any` → Rust type → `Explainable::to_args` → `ExtensionArgs`. The parse path is the reverse: `ExtensionArgs` → `Explainable::from_args` → Rust type → `AnyConvertible::to_any` → proto `Any`

### File Map

- `extensions/simple.rs` — `SimpleExtensions`: anchor to name lookup table for functions, types, and type variations
- `extensions/registry.rs` — `ExtensionRegistry`: user-provided handler registry for advanced extension payloads
- `extensions/args.rs` — `ExtensionArgs`: the structured intermediate type that bridges text format and proto blobs
- `extensions/any.rs` — `Any` / `AnyRef`: owned and borrowed wrappers around `google.protobuf.Any`
- `extensions/examples.rs` — example of `Explainable` implementations used in documentation and tests

### `extensions/simple.rs` — Anchor Lookup

`SimpleExtensions` is the anchor lookup table built from the `=== Extensions` section. It maps integer anchors to URN strings and qualified names for functions, types, and type variations.

### `extensions/registry.rs` — Advanced Extension Registry

`ExtensionRegistry` maps registered types to handlers that know how to convert between their `google.protobuf.Any` blob and their text representation.

### `extensions/args.rs` — Extension Arguments

`ExtensionArgs` is the structured intermediate that extension handlers read and write — a collection of positional values, named values, and output column declarations. `ExtensionValue` covers the scalar and expression types that can appear as argument values. Extension relations that declare an output schema use `ExtensionColumn` to describe it, which converts to and from the `NamedStruct` proto type.

Handlers read from `ExtensionArgs` via an `ArgsExtractor`, which tracks which arguments have been consumed. This enforces that no unexpected arguments are silently ignored — the extractor errors if unconsumed named arguments remain after parsing, catching mismatches between what the text format provides and what the handler expects.

### `extensions/any.rs` — Any Wrapper

`any.rs` defines local owned and borrowed representations for protobuf `Any` payloads. `Any` is the crate-owned form used when code needs to build a new payload, such as the value returned by `AnyConvertible::to_any()`. `AnyRef<'a>` is a borrowed view over an existing payload and can be created from `prost_types::Any`, `pbjson_types::Any`, or the crate's own `Any`. The registry and textifier use these wrappers so extension code works with one stable `Any` API instead of depending on the concrete `Any` type used by a particular serialization crate. Prost-generated extension types can still use the blanket `AnyConvertible` implementation; custom types can implement `AnyConvertible` manually. `AnyRef<'a>` gives the API a way to pass existing `Any` payloads by reference instead of requiring ownership at every call site.

---

## Testing

The test suite is split between unit tests inside source files and integration tests under `tests/`.

Unit tests live alongside the code they test following standard Rust convention. `src/fixtures.rs` provides `TestContext` and other helpers shared across many test modules. `types_tests.rs` contains type round-trip tests that span both the parser and textifier and has no single source file it naturally belongs to. They both need access to `pub(crate)` types, so they cannot be in `tests/`, which is compiled as a separate crate and cannot see crate-internal items.

Integration tests under `tests/` test the public API as an external caller would — they can only access what `lib.rs` exports. The primary strategy is round-trip: parse a text plan to proto, textify it back to text, and assert the output matches. This catches most correctness bugs without requiring proto-level assertions.

- `plan_roundtrip.rs` — broad round-trip coverage across relation types and features
- `literal_roundtrip.rs` — round-trips for literal value parsing and formatting
- `multi_line_roundtrip.rs` — round-trips for multi-line `Read:Virtual` continuation syntax
- `extension_roundtrip.rs` — round-trips for custom extension handlers
- `adv_extension_roundtrip.rs` — round-trips for `+ Enh:` and `+ Opt:` advanced extension annotations
- `extension_table.rs` — round-trips for `Read:Extension` and `+ Ext:` extension table reads
- `json_parsing.rs` — exercises both JSON input formats (pbjson and Go protojson) against a real custom extension type

`tests/common/mod.rs` provides shared helpers used across the integration test files — primarily `roundtrip_plan`, which parses a text plan and formats it back, asserting the output matches.

## Examples and Reference Plans

`examples/` contains runnable Cargo examples (`cargo run --example <name>`) demonstrating the public API:
- `basic_usage.rs` covers parsing and formatting, 
- `advanced_usage.rs` covers output options
- `extensions.rs` shows the full custom extension handler pattern end-to-end — defining a type, implementing `Explainable`, registering it, and round-tripping through the registry.

`example-plans/` contains sample `.substrait` text files at different feature levels — basic relations, scalar functions, user-defined types. These are useful as manual test inputs and as reference when working on the parser or textifier.