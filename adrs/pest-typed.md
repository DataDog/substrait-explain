# ADR: Migrate Parser Internals to `pest_typed`

- Status: Proposed
- Date: 2026-02-26
- Decision Owners: substrait-explain maintainers

## Context

`substrait-explain` already uses `pest` for grammar parsing (`src/parser/expression_grammar.pest`), but most parser code consumes untyped `Pair<Rule>` trees manually across:

- `src/parser/structural.rs`
- `src/parser/relations.rs`
- `src/parser/expressions.rs`
- `src/parser/types.rs`
- `src/parser/extensions.rs`

Current parsing has a few structural costs:

- High runtime shape-checking overhead (`assert_eq!(pair.as_rule(), ...)`, `unwrap_single_pair`, manual iterator choreography).
- Multiple panic/todo/unimplemented paths in parser internals, which are avoidable for parse-time validation.
- Grammar evolution requires synchronized updates across many hand-written tree walkers.
- Rule mismatch bugs are caught at runtime, not at compile time.

We evaluated `pest_typed` (`pest_typed`/`pest_typed_derive` v0.20.11, repo: https://github.com/TheVeryDarkness/pest-typed), which generates strongly typed rule nodes from the same `.pest` grammar.

Notable findings from upstream docs/repo:

- The main documented drawback is slower compile time than `pest`.
- `simulate_pair_api` is documented as "Currently ignored", so drop-in `Pair` API compatibility is not available.
- Project appears actively maintained (recent commits through 2025-12, latest crate publish 2025-09-25), but ecosystem size is still relatively small.

Local compile spike (throwaway mini crate) showed higher build cost for typed derive:

- `pest_derive` spike: ~3.9s `cargo check`
- `pest_typed_derive` spike: ~8.8s `cargo check`

## Decision

We will migrate parser internals from raw `pest::Pair<Rule>` traversal to `pest_typed` generated typed nodes.

Because this project is alpha, we explicitly allow breaking internal structures and parser internals during this migration. Compatibility-preserving migration layers are optional, not required.

### Scope

- In scope:
  - Parser internals (`src/parser/**`) that consume parse trees.
  - Internal parser traits/helpers that become unnecessary with typed nodes.
  - Test updates needed to preserve current parse/format roundtrip behavior.
- Out of scope:
  - Text format syntax changes.
  - Public API behavior changes (except improved error stability).

## Rationale

`pest_typed` improves maintainability and safety for this codebase's current parser style:

- Encodes grammar shape in types, reducing manual rule assertions and unwrap chains.
- Moves structural mismatch detection earlier (compile-time + typed construction boundaries).
- Makes grammar-driven refactors more local and less error-prone.
- Reduces accidental panics from invalid `Pair` assumptions.

## Migration Plan

1. Add typed parser generation side-by-side:
   - Introduce a new typed parser struct using `pest_typed_derive::TypedParser` and the same grammar file.
   - Keep or remove the existing parser code path based on implementation speed.
2. Migrate leaf domains first:
   - `types.rs`, `expressions.rs`, `extensions.rs`.
   - Replace `ParsePair`/`RuleIter` usage with typed-node accessors for these domains.
3. Migrate relation/structural parsing:
   - Convert `relations.rs` and `structural.rs` tree walkers to typed nodes.
4. Differential verification:
   - Keep roundtrip tests as primary guardrail.
   - Add focused regression tests for grammar areas being actively changed.
5. Remove legacy untyped traversal:
   - Delete obsolete helper abstractions once parity is established.

## Consequences

### Positive

- Less fragile parsing code with fewer runtime structural checks.
- Better refactorability when grammar changes.
- Fewer parser panic paths in non-test code.

### Negative / Tradeoffs

- Slower compile times for parser-related codegen.
- Larger generated type surface and potentially more complex IDE navigation.
- No incremental `Pair`-API shim available (`simulate_pair_api` currently ignored), so migration requires real code changes.

## Execution Notes (Alpha)

- Prefer delivery speed over compatibility scaffolding; avoid migration glue unless it materially reduces effort.
- Do not pin exact `pest_typed` patch versions by default; move quickly and only pin if upstream regressions force it.
- Keep parser changes in small PRs by subdomain (`types`, `expressions`, `relations`, `structural`) to reduce review risk.
- Use roundtrip and targeted parser tests as the safety net when making breaking internal changes.

## Alternatives Considered

1. Stay on raw `pest` + current helpers:
   - Lowest short-term cost, but keeps high manual traversal and runtime assertion burden.
2. Migrate to another parser stack (`lalrpop`, `nom`, etc.):
   - Higher rewrite cost, larger grammar/model churn, no clear short-term payoff versus typed-`pest`.
3. Use `pest` + AST helper crates (`pest_consume`, `pest-ast`):
   - Viable, but `pest_typed` gives the most direct typed-rule mapping with minimal grammar disruption.

## Acceptance Criteria

- All existing parser + roundtrip tests pass with typed parser internals.
- No new parser panics in non-test paths for invalid user input.
- Parser output remains byte-for-byte equivalent where existing tests assert canonical formatting.
- CI build time impact is measured and accepted by maintainers.

## Post-Implementation Compile-Time Comparison

Date measured: 2026-02-26

Method:

- Compared baseline revision (`routksqw 040734a0`) vs migrated revision in current workspace.
- Used identical command for timed trials: `/usr/bin/time -p cargo check --all-features`.
- To avoid full dependency rebuild noise, prebuilt dependencies once per revision (`cargo check --all-features`), then for each trial ran `cargo clean -p substrait-explain` before timing.
- Ran 3 trials per revision and compared medians of `real`.

Results (`real`, seconds):

- Baseline: `0.98`, `0.98`, `1.06` -> median `0.98s`
- Migrated: `2.17`, `2.07`, `2.20` -> median `2.17s`
- Delta: `+1.19s` (`+121.4%`)

Interpretation:

- The migration increased this crate's check-time under `--all-features` in this environment.
- This result is informational (non-blocking), consistent with expected `pest_typed` compile-time tradeoff.

## References

- Current parser grammar: `src/parser/expression_grammar.pest`
- Current parser internals: `src/parser/*.rs`
- `pest_typed` crate: https://docs.rs/pest_typed
- `pest_typed_derive` crate: https://docs.rs/pest_typed_derive
- Upstream repo: https://github.com/TheVeryDarkness/pest-typed
- Generator usage notes (`simulate_pair_api` status): https://docs.rs/crate/pest_typed_generator/latest/source/Usage.md
