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

## `src/textify/` — Proto to Text

### Textify Summary

1. lib.rs — entry point, calls into textify
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
Relations are made up of: arguments, columns output mapping(Emit), addenda(advanced extensions), and children.

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

The parser uses a PEG grammar (via [Pest](https://pest.rs)) to match text syntax; similar to regex. Each plan line is parsed individually by the grammar into a **`LineNode`**: either a **`RelationNode`** (a relation header line) or an **`Addendum`** (a `+ Enh:` / `+ Opt:` / `+ Ext:` line). `RelationNode` holds the raw grammar match for that line, its line number, and initially empty `addenda` and `children` lists.

**`TreeBuilder`** then assembles these flat `LineNode`s into a tree by using each line's indentation depth. A line at depth 0 becomes a new root. A line at depth N is attached as a child of the most recently seen node at depth N-1. Addendum lines are attached to the relation immediately above them at one less indent level, and must appear before any child relations. The result is a `RelationNode` tree that mirrors the visual indentation of the input text.

Once the tree is built, it is walked depth-first: each `RelationNode` is converted to a `Rel` proto by `relations.rs`, which calls into `expressions.rs` and `types.rs` for its arguments and type annotations. This two-phase approach — build the tree first, convert to proto second — is what allows the parser to construct properly nested `Rel` messages without backtracking.

### Parser Summary

1. `expression_grammar.pest` — PEG grammar rules; all text syntax is defined here
2. `lib.rs` — entry point, calls into parser
3. `parser/mod.rs` — re-exports types from the submodules
4. `parser/chunks.rs` — merges continuation lines into single logical chunks before grammar parsing
5. `parser/structural.rs` — builds a `RelationNode` tree from the chunked plan lines using indentation depth
6. `parser/extensions.rs` — parses simple extension declarations and addendum(advanced extension) lines
7. `parser/relations.rs` — converts each `RelationNode` into a `Rel` proto message
8. `parser/expressions.rs` — called from relations to parse expression arguments
9. `parser/types.rs` — called from expressions and relations to parse type annotations
10. `parser/common.rs` — shared parsing infrastructure and traits used throughout

### Entry Point

The public entry points in `lib.rs` are `parse()` and `parse_with_registry()`. Unlike the textify side where all format functions funnel into `format_with_registry`, here there are two independent paths. The parser processes input in two passes: the simple Extensions section first to build the anchor lookup tables, then the Plan section to convert relations. Every anchor referenced in a relation is already resolved by the time that relation is parsed.

The `ExtensionRegistry` is needed for the same reason as in the textifier: advanced extension payloads (`+ Enh:`, `+ Opt:`, extension relations) are `google.protobuf.Any` blobs, and the registry knows which registered type to encode them into. In the textifier, a missing registration produces a soft error token in the output and continues formatting. In the parser, a missing registration is a hard `ParseError::UnregisteredExtension` — there is no way to produce a valid `Any` blob without the registered type. This is why `parse()` does not supply a default empty registry with a silent fallback. Plans with advanced extensions must use `parse_with_registry()`.

### `parser/chunks.rs` — Line Grouping

Operates before any grammar parsing. The text format allows long argument lists to continue on the next line if that line is indented one level deeper and prefixed with `- `. `ChunkCursor` merges these continuation lines back into a single logical chunk so that the grammar parser always sees complete, self-contained expressions.

### `parser/structural.rs` — Section Routing and Tree Construction

The main orchestrator for parsing. It reads the input section by section and routes each line accordingly: extension declaration lines go to `extensions.rs`, plan lines are parsed into `LineNode`s and placed into the `RelationNode` tree by `TreeBuilder`. Once all lines are consumed, the completed `RelationNode` trees are walked depth-first to produce the final `proto::Plan`.

### `parser/extensions.rs` — Extension and Addendum Parsing

Handles two distinct jobs. Parses the Extensions section: URN declarations and function/type/variation anchor assignments, producing a `SimpleExtensions` anchor table that the rest of the parser uses to resolve function, type, and variation anchors to their declared names. Second, it parses addendum lines (`+ Enh:`, `+ Opt:`, `+ Ext:`) that appear attached to relations in the Plan section, decoding their names and arguments through the `ExtensionRegistry`.

### `parser/relations.rs` — Relation Construction

Converts each `RelationNode` into a `Rel` proto message. Implements the conversion for supported relation types: Read, Filter, Project etc... Also handles emit mapping — reconstructing the `RelCommon` output column remapping from the text representation.

### `parser/expressions.rs` — Expression Parsing

Called from `relations.rs` to parse expression arguments into `proto::Expression` messages. Covers expression types.

### `parser/types.rs` — Type Parsing

Called from `expressions.rs` and `relations.rs` to parse type annotations into `proto::Type` messages. Handles simple types, compound types, precision types, nullability suffixes, type variations, and user-defined types.

### `parser/common.rs` — Shared Infrastructure

Defines two traits that all relation, expression, and type parsers implement — the parser-side equivalent of the `Textify` trait.

**`ParsePair`** converts a pest grammar match into a Rust type for constructs whose meaning is fully determined by syntax — literals, operators, nullability suffixes — with no anchor resolution needed.

**`ScopedParsePair`** does the same but takes a `&SimpleExtensions` context argument. It is used for constructs that require anchor lookups — function calls, type references, type variations — where the conversion can fail with a `MessageParseError` if the anchor is not in the table.

### `expression_grammar.pest` — PEG Grammar

The canonical grammar for the text format. Defines rules for relation headers, argument lists, all expression forms, type syntax, and extension arguments. All text parsing ultimately flows through Pest rules defined here; the rest of the parser translates the resulting `Pair<Rule>` matches into proto types.
