# AGENTS.md

This file gives AI agents the repository-specific context needed to work on
`substrait-explain`. Read `AGENTS.local.md` if it exists.

For contributor-facing policy, use `CONTRIBUTING.md` as the source of truth.

## Development Commands

- `just fmt` - Format code with custom import grouping
- `just check` - Run clippy checks
- `just fix` - Format code and auto-fix clippy issues where possible
- `just test` - Run tests, examples, and CLI validation
- `cargo check` - Check compilation without producing a final binary
- `cargo test --doc` - Test documentation examples; required for `GRAMMAR.md` changes
- `cargo test -- --nocapture` - Run tests with output visible
- `bash pre-commit.sh install` - Install local formatting/linting hooks

## Architecture

`substrait-explain` is a bidirectional parser/formatter for Substrait query
plans:

- Text format -> Substrait protobuf: `src/parser/`
- Substrait protobuf -> text format: `src/textify/`
- Extension function/type handling: `src/extensions/`

Key files:

- `src/parser/structural.rs` handles document structure and indentation.
- `src/parser/expression_grammar.pest` defines relation, expression, literal,
  type, extension, and addendum syntax.
- `src/grammar.rs` exposes `GRAMMAR.md` as crate documentation for doctests and
  generated docs.
- `src/textify/foundation.rs` provides formatting infrastructure and error
  accumulation.
- `src/textify/plan.rs`, `src/textify/rels.rs`, `src/textify/expressions.rs`,
  and `src/textify/types.rs` format plans, relations, expressions, and types.

The text format has optional `=== Extensions` and required `=== Plan` sections.
Relations generally follow `RelationName[arguments => output_columns]` with
2-space indentation showing the plan tree.

## Development Rules

- Prefer code that supports local reasoning: keep invariants close to the data
  they describe, avoid unnecessary cross-module helper plumbing, and use named
  types or trait implementations when signatures can carry intent.
- When implementing or documenting support for a Substrait feature, start from
  the upstream spec or protobuf definition. Link the relevant reference in the
  issue or PR, use upstream terminology where it exists, and describe
  `substrait-explain` text-format choices as representation choices rather than
  changes to Substrait itself.
- Parser code should fail with contextual errors for bad input. Pest pair
  parsers may unwrap only when the grammar enforces the invariant, and should
  assert the expected rule first.
- Textifier code should accumulate errors and keep producing best-effort output.
  Do not panic or silently emit invalid text for malformed, older, or partially
  populated protobuf input.
- Tests may unwrap known-good inputs. Prefer expected-value assertions when they
  are readable, but avoid huge hand-built protobuf expectations when focused
  property checks communicate the behavior better.

## Routing

- For project-level design philosophy like compatibility expectations, error handling
  posture, extension boundaries, or format change guidance, read `DESIGN.md`.
- For syntax or text-format changes, read `GRAMMAR.md` and update the docs,
  Pest grammar, parser, textifier, and roundtrip tests together.
- For contributor process, PR expectations, issue shape, or public guidance,
  read `CONTRIBUTING.md`.
- For local workflow or user-specific rules, read `AGENTS.local.md` if it
  exists.
- For release-process changes, read `RELEASING.md`.
- For review tasks, inspect the actual diff first and focus on regressions,
  missing tests, API boundaries, and grammar/parser/textifier alignment.

## Grammar and Text Format

- Treat `GRAMMAR.md` as the user-facing text-format contract.
- For syntax changes, update `GRAMMAR.md`, the Pest grammar, parser,
  textifier, and roundtrip tests together so accepted input and produced output
  stay aligned.
- `GRAMMAR.md` uses PEG-style documentation syntax. Do not copy Pest-specific
  syntax (e.g. `~` or `|`) into the user-facing grammar.
- Documentation examples must compile. Run `cargo test --doc` when changing
  `GRAMMAR.md`.
- Rustdoc hides lines that begin with `#`; use `##` for literal `#` characters
  in Substrait syntax examples.
- Prefer readable defaults in text output when they do not lose information.
  Keep common cases concise while preserving anchors, signatures, types, and
  extension references where needed for disambiguation.

## Project Notes

- Keep this file concise and agent-oriented. Contributor-facing policy belongs
  in `CONTRIBUTING.md`; user- or workspace-specific workflow belongs in
  `AGENTS.local.md`.
- Public API changes should be intentional. In PR descriptions, state whether
  the change preserves compatibility, intentionally breaks compatibility, or
  narrows an accidentally exposed internal surface.
