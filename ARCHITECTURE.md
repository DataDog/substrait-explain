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

*More sections to follow: `src/parser/`, `src/extensions/`.*
