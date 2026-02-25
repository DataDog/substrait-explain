# ADR: Replace Pest with LALRPOP for Line-Level Parsing

**Status:** Proposed
**Date:** 2026-02-25

## Context

The parser for substrait-explain's text format is currently built with [Pest](https://pest.rs/), a PEG parser generator. Pest produces a generic parse tree (`Pairs`) that must be walked manually to build Substrait protobuf structs. This works but has several pain points:

- **Manual tree walking** — ~5,000 lines of `RuleIter`, `ParsePair`, and `ScopedParsePair` code to consume Pest pairs and build protobuf structs directly.
- **`RuleIter::done()` ceremony** — The `Drop`-based consumption enforcement requires calling `done()` before any early return, forcing an awkward "extract all data first, validate after" pattern across the codebase.
- **No intermediate AST** — Pest pairs are converted directly to protobuf, tightly coupling the parser to the Substrait proto schema and mixing syntax parsing with semantic validation (e.g., extension lookups happen during parsing).
- **Silent ambiguity** — PEG's ordered choice silently resolves grammar ambiguities. Ordering bugs produce wrong parses rather than errors.
- **Per-relation grammar rules** — Each relation type has its own Pest rule despite the text format being designed around a uniform `Name[args => columns]` structure.

## Decision

Replace the Pest expression parser with [LALRPOP](https://github.com/lalrpop/lalrpop), introducing an intermediate AST and a lowering pass from AST to Substrait protobuf.

### Scope: Strategy A — Replace Only the Line Parser

The structural parser (`structural.rs`) — which handles section headers (`=== Extensions`, `=== Plan`), indentation-based tree building, and line dispatching — remains unchanged. Only the per-line content parsing (expressions, types, relation arguments) moves to LALRPOP.

This is the minimal migration. The structural parser is clean and well-suited to its job; the pain points are entirely in the Pest layer.

### Lexer: LALRPOP Built-in

Use LALRPOP's built-in regex-based lexer. The token set is small (~25 tokens: `$`, `#`, `@`, `&`, `=>`, delimiters, identifiers, numbers, strings, keywords). The built-in lexer handles this comfortably, and automatic whitespace skipping is appropriate since each line is parsed independently.

If we later need context-sensitive lexing (e.g., for more complex string escapes), we can upgrade to [Logos](https://github.com/maciejhirsz/logos) without changing the grammar.

### Grammar: Abstract/Generic Relations

Instead of per-relation grammar rules, define a single generic relation rule:

```lalrpop
Relation: ast::Relation = {
    <name:RelationName> "[" <args:ArgList> "=>" <cols:Comma<Arg>> "]"
        => ast::Relation { name, args, outputs: cols },
    <name:RelationName> "[" <args:ArgList> "]"
        => ast::Relation { name, args, outputs: vec![] },
    <name:RelationName> "[" "]"
        => ast::Relation { name, args: vec![], outputs: vec![] },
};

// Positional arguments followed by optional named arguments.
// Named arguments always come last — they cannot be intermingled with positional args.
ArgList: ast::ArgList = {
    <pos:Comma<Arg>> "," <named:Comma<NamedArg>>
        => ast::ArgList { positional: pos, named },
    <pos:Comma<Arg>>
        => ast::ArgList { positional: pos, named: vec![] },
    <named:Comma<NamedArg>>
        => ast::ArgList { positional: vec![], named },
};

// Positional argument — expressions, enums, named columns, tuples, wildcard.
// Tuples also contain only positional arguments (no named args inside tuples).
Arg: ast::Arg = {
    <e:Expression>              => ast::Arg::Expr(e),
    <e:Enum>                    => ast::Arg::Enum(e),
    <n:Name> ":" <t:Type>       => ast::Arg::NamedColumn(n, t),
    "(" <args:Comma<Arg>> ")"   => ast::Arg::Tuple(args),
    "_"                         => ast::Arg::Wildcard,
};

NamedArg: ast::NamedArg = {
    <n:Name> "=" <a:Arg>        => ast::NamedArg { name: n, value: a },
};
```

Relation-specific validation (e.g., "Filter expects an expression before `=>` and references after") moves to the lowering pass. This reflects the text format's stated design principle: *"All relations follow the same structure: `Name[arguments => columns]`"*.

**Argument ordering:** The grammar enforces that named arguments (`name=value`) always follow positional arguments — they cannot be intermingled. Tuples can only contain positional arguments. This is a structural constraint that belongs in the grammar, not the lowering pass, since it applies uniformly to all relations.

**Benefits:**
- Smaller grammar with fewer LR conflict risks.
- Adding new relation types (Window, Set, HashJoin) requires only lowering code, no grammar recompilation.
- Cleaner separation: grammar owns syntax, lowering owns semantics.

**Cost:**
- Parse errors are less relation-specific. Mitigated by the lowering pass, which has full context (including line reference) to produce clear semantic errors like "Filter's output must be field references, not named columns."

### Intermediate AST

Introduce a lightweight AST as the grammar's output, rather than building protobuf directly:

```rust
// src/ast.rs (sketch)

struct Document {
    extensions: Extensions,
    relations: Vec<RelationTree>,
}

struct RelationTree {
    relation: Relation,
    children: Vec<RelationTree>,
    line: LineInfo,  // for error reporting
}

struct Relation {
    name: RelationName,
    args: ArgList,
    outputs: Vec<Arg>,                    // positional only — no named args after =>
}

enum RelationName {
    Standard(String),                     // "Read", "Filter", ...
    Extension(ExtensionType, String),     // "ExtensionLeaf:ParquetScan"
}

/// Input arguments: positional args followed by optional named args.
/// Named args always come last; they cannot be intermingled.
struct ArgList {
    positional: Vec<Arg>,
    named: Vec<NamedArg>,
}

/// A positional argument. Used both in input args and output columns.
/// Tuples also contain only positional args.
enum Arg {
    Expr(Expr),
    Enum(String),                         // &Inner, &AscNullsFirst
    NamedColumn(String, Type),            // name:type
    Tuple(Vec<Arg>),                      // (ref, enum)
    Wildcard,                             // _
}

/// A named argument (name=value). Only valid in input position.
struct NamedArg {
    name: String,
    value: Arg,
}

enum Expr {
    FieldRef(u32),                        // $0, $1
    Literal(LiteralValue, Option<Type>),  // 42, 'hello':string
    FunctionCall {
        name: String,
        anchor: Option<u32>,
        urn_anchor: Option<u32>,
        args: Vec<Expr>,
        output_type: Option<Type>,
    },
    IfThen {
        clauses: Vec<(Expr, Expr)>,
        else_expr: Box<Expr>,
    },
}

enum LiteralValue {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(String),
}
```

### Lowering Pass: AST to Protobuf

A new `lower` module converts the AST to `substrait::proto::Plan`:

1. **Extension resolution** — resolve `#anchor` and `@urn` references against `SimpleExtensions`. This moves out of the parser entirely.
2. **Relation dispatch** — match on relation name, validate argument shapes, build the appropriate `Rel` variant.
3. **Type construction** — convert AST types to protobuf type structs.
4. **Error reporting** — errors reference the source line via `LineInfo`, providing context like "line 5: Filter expects a boolean expression before `=>`, got named column `foo:i64`".

This cleanly separates the three concerns currently mixed in the parser: syntax (grammar), name resolution (extensions), and semantic validation (relation-specific rules).

### Parse Algorithm: LR(1) with LALR(1) Fallback

Start with LR(1) (LALRPOP's default), which accepts a broader grammar class. If build times become a concern, switch to LALR(1) via `grammar["LALR(1)"];` in the grammar file, which generates smaller code at the cost of rejecting some valid LR(1) grammars.

## Consequences

### What We Gain

- **~3,000 lines of pair-walking code removed** — `RuleIter`, `ParsePair`, `ScopedParsePair`, and most of `common.rs`, `expressions.rs`, `relations.rs`, `types.rs` are replaced by inline grammar actions and a lowering pass.
- **Compile-time grammar validation** — Ambiguous grammars produce shift/reduce conflict errors instead of silent misbehavior.
- **Clean architecture** — Three distinct phases (parse → AST → protobuf) with clear boundaries.
- **Extension resolution decoupled from parsing** — Errors are categorized as syntactic vs. semantic, and extension changes don't affect the grammar.
- **Easier to add relations** — New relation types are code-only changes in the lowering pass.
- **`get_input_field_count()` bug fixable** — The incomplete field-count helper (currently only handles Read/Filter/Project) can be properly implemented in the lowering pass where all relation types are handled uniformly.

### What We Lose / Risk

- **Build time increase** — LALRPOP uses `build.rs` code generation. For our grammar size (~50–100 rules), this should add ~10–20s to builds, not minutes. Worth measuring early.
- **Opaque generated code** — LALRPOP's generated parser is not meant to be read. Grammar debugging relies on conflict diagnostics rather than code inspection.
- **Migration effort** — Touches every file in `src/parser/`. Estimated at 2–3 focused sessions. The existing roundtrip tests provide strong regression coverage.
- **New dependency** — `lalrpop` (build-dep) + `lalrpop-util` (runtime dep). Both are well-maintained (~2.8M downloads/month, latest release Feb 2026).
- **LR grammar engineering** — Some PEG patterns (ordered choice for disambiguation) need rethinking as explicit precedence. The abstract relation grammar minimizes this risk.

### What Stays the Same

- **Structural parser** (`structural.rs`) — Line-by-line state machine, indentation tree building, section header handling. Unchanged.
- **Textifier** (`src/textify/`) — Completely unaffected; it reads protobuf, not the parser's internals.
- **Public API** — `parse()` still returns `Result<Plan, ParseError>`.
- **Roundtrip tests** — Continue to validate parse→format→parse. They don't depend on parser internals.
- **Extension system** — `SimpleExtensions` and `ExtensionRegistry` remain; they're just consumed by the lowering pass instead of during parsing.

## Migration Plan

### Phase 1: AST Types and Grammar

1. Add `lalrpop` to `[build-dependencies]` and `lalrpop-util` to `[dependencies]`.
2. Create `build.rs` with `lalrpop::process_root().unwrap()`.
3. Define AST types in `src/ast.rs`.
4. Write the LALRPOP grammar (`src/parser/grammar.lalrpop`) covering expressions, types, arguments, and the generic relation rule.
5. Unit-test the grammar in isolation against known-good inputs.

### Phase 2: Lowering Pass

1. Implement `src/lower.rs` — AST → `substrait::proto` conversion with extension resolution.
2. Implement per-relation validation and argument destructuring.
3. Wire up error reporting with line references.

### Phase 3: Integration and Cutover

1. Modify `structural.rs` to call the LALRPOP parser instead of Pest for line content.
2. Route LALRPOP AST through the lowering pass to produce protobuf.
3. Run the full roundtrip test suite — this is the primary validation.
4. Remove Pest: delete `expression_grammar.pest`, `common.rs` (RuleIter, ParsePair), and the per-relation/per-expression pair-walking code.

### Phase 4: Cleanup

1. Remove the `pest` and `pest_derive` dependencies.
2. Simplify error types (parse errors vs. lowering errors are now distinct).
3. Update GRAMMAR.md examples if any syntax details changed.

## Alternatives Considered

### Keep Pest, add an AST layer

We could introduce an intermediate AST while keeping Pest. This would fix the "no AST" and "extension resolution during parsing" problems but would *not* fix RuleIter ergonomics, silent ambiguity, or per-relation grammar coupling. The pair-walking code would still exist, just targeting AST types instead of protobuf.

### Hand-written recursive descent parser

Maximum control and debuggability. No build-time code generation. Used by Ruff (which migrated *from* LALRPOP). However, for our grammar size, the engineering effort is higher than LALRPOP and we'd lose compile-time ambiguity checking. Better suited for larger, more complex grammars where error recovery and performance are critical.

### Parser combinators (nom, winnow, chumsky)

No grammar file, pure Rust. Good composability. But for a grammar of this size with clear structure, a grammar file is more readable and maintainable than combinator chains. Chumsky has excellent error recovery but is less mature than LALRPOP.

### Stay with Pest as-is

The parser works. The pain points are real but manageable. This is a valid choice if the migration effort is better spent on feature work (new relation types, better error messages, etc.). The risk is that each new relation adds more pair-walking boilerplate, compounding the maintenance cost.
