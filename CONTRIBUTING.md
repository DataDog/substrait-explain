# Contributing to Substrait-Explain

Welcome! We are glad you are interested in contributing to Substrait-Explain. This guide will help you understand the requirements and guidelines to improve your contributor experience.

## Contributing to Code

### New Features

If you want to contribute a new feature, we recommend opening an issue first to discuss your proposal. This helps ensure that:

- The feature aligns with the project's goals
- You don't duplicate work that's already in progress
- We can provide guidance on the best approach

Open a new issue with a clear title describing your feature request. In the description, explain:

- Why this feature is needed
- How it would be useful
- Your proposed implementation approach

The maintainers will review your proposal and provide feedback. Once approved, you can proceed with implementation.

### Bug Fixes

If you have identified an issue that is already labeled as `type/bug` that hasn't been assigned to anyone, feel free to claim it and ask a maintainer to add you as assignee.

For non-trivial bug fixes, please open an issue first to discuss the problem and potential solutions with the maintainers.

### Setting up your development environment

1. **Install Rust**: Install the latest stable version of Rust from [rustup.rs](https://rustup.rs/)
2. **Clone the repository**:
   ```bash
   git clone https://github.com/DataDog/substrait-explain.git
   cd substrait-explain
   ```
3. **Verify setup**: Run `cargo test` to ensure everything works
4. **Install pre-commit hooks**: Run `bash pre-commit.sh install` to automatically format and lint your code
   - This will run `cargo fmt` and `cargo clippy -- -D warnings` automatically on each commit
5. **Useful development commands**:
   ```bash
   just fmt                      # Format code
   just check                    # Run linting and checks
   just fix                      # Format code, auto-fix clippy errors
   just test                     # Run all tests and examples
   cargo test                    # Run Rust tests
   cargo test -- --nocapture     # Run tests with output
   cargo run --example basic_usage
   cargo doc --open              # View documentation
   ```

### Code Style

- Follow Rust conventions and idioms
- Use `cargo fmt` for formatting (enforced by pre-commit hooks)
- Run `cargo clippy` to catch common issues (enforced by pre-commit hooks)
- Add documentation for public APIs, and review them with `cargo doc --open`
- Include tests for new functionality
- **Import organization**: Prefer module-level imports over function-level `use` statements or long inline type references (e.g., `use substrait::proto::RelType;` at module level rather than `substrait::proto::RelType` inline)
- **Structure for local reasoning**: Prefer code that keeps invariants close to the data they describe. Use named intermediate types, trait implementations, or methods when they make parser/textifier behavior easier to understand from signatures and local context. Avoid helper functions whose signatures expose unrelated state or spread one concept across several modules.
- **Pattern matching depth**: Avoid deeply nested pattern matching (if-let chains, match arms). Prefer sequential operations with let-else patterns or early returns for clarity

  ```rust
  // Good: Sequential with let-else
  let Some(root) = plan.relations.first_mut() else {
      return Err("No relations");
  };
  let RelType::Filter(filter) = &root.rel_type else {
      return Err("Expected Filter");
  };

  // Bad: Deeply nested
  if let Some(root) = plan.relations.first_mut() {
      if let RelType::Filter(filter) = &root.rel_type {
          // ... more nesting
      }
  }
  ```

- **Unused parameters**: When refactoring changes behavior, remove parameters that are no longer used rather than prefixing with underscore

### Project-Specific Guidelines

See the overall [Design Philosophy documentation](DESIGN.md) for the project-level design principles and [`GRAMMAR.md`](GRAMMAR.md) for the user-facing syntax contract.

#### Substrait-Specific Development

This project implements Substrait protobuf plan parsing and formatting. See the [official Substrait documentation](https://substrait.io/) for the complete specification, including output mapping rules per relation type.

When implementing or documenting support for a Substrait feature, start from the upstream spec or protobuf definition. Link the relevant reference in the issue or PR, use upstream terminology where it exists, and describe `substrait-explain` text-format choices as representation choices rather than changes to Substrait itself.

##### Project-specific text format patterns

- **Enum textification**: All Substrait enums use `&` prefix in text format (e.g., `&Inner`, `&AscNullsFirst`)
- **Unknown enum handling**: For invalid/unknown enum values, use `Value::Missing(PlanError)` and fallback to reasonable default enum value (usually `UNSPECIFIED`) for processing
- **Readable defaults**: Prefer concise, human-readable text output when it does not lose information. Syntax should make the common case clear while still preserving enough detail to disambiguate Substrait/Protobuf specifics like anchors, signatures, types, and extension references when needed.

#### Error Handling Patterns

This project uses different error handling patterns depending on the context:

##### Pest Parser Context

Unwraps are safe where the grammar enforces the invariants. Always assert that the right rule was used to ensure the same invariants:

```rust
// Pest parse functions should assert the rule matches
fn parse_pair(pair: pest::iterators::Pair<Rule>) -> Self {
    assert_eq!(pair.as_rule(), Self::rule(), "Expected rule {:?}", Self::rule());
    // Now safe to unwrap based on grammar guarantees
    let inner = pair.into_inner().next().unwrap();
    // ... rest of parsing logic
}
```

_Note: Ideally, `pest_derive` would remove the need for unwraps by providing structural types that encode the invariants, ensuring at compile-time the grammar and code remain in sync; however, that functionality does not yet fully exist._

##### Other Parsing Context (e.g., lookups)

These can reasonably fail. For the parser, we error on bad input and attempt to provide good error messages:

```rust
// Handle parsing failures gracefully
let value = input.parse::<i32>().map_err(|_| "Invalid integer")?;
let first = chars.next().ok_or("Expected non-empty input")?;
```

_Note: When parsing with pest, use `pest::Span` and `pest::Error` types to provide clearer error messages about where in the input parsing failed. This helps users understand exactly what went wrong and where._

##### Formatting Context

When going from a Plan to text, our goal is to be lenient: track errors and return them, but also always provide "best-effort" output. Failures should be noted and pushed to an ErrorAccumulator, but processing should continue, with a `!{message}` block in the output:

```rust
// Use Scope::expect for graceful error handling in textification
write!(w, "{}", ctx.expect(relation.input.as_ref()));

// Avoid protobuf field unwraps - they can legitimately be None
let input = relation.input.as_ref().unwrap(); // RISKY

// Better: Use Scope::expect or Scope::failure
write!(w, "{}", ctx.expect(relation.input.as_ref()));
```

**Error accumulation patterns:**

- Use `ctx.expect(option)` to handle missing values with error accumulation
- Use `ctx.failure(error)` to record errors while continuing processing
- Return `PlanError` directly only for unrecoverable parsing failures

##### Test Context

Unwraps and asserts are fine in tests with known-good inputs:

```rust
// Acceptable in test code
let plan = Parser::parse(plan_text).unwrap();
assert!(result.is_ok());
```

##### General Best Practices

- Only use `unwrap`, `expect`, or equivalents when you have validated that the operation cannot fail (e.g., after assertions, in test code, or when grammar guarantees apply)
- Use `unwrap_or` for defaults over `unwrap` when a fallback value is appropriate
- Use `expect` with descriptive messages when unwrap is necessary but could fail
- Return `Result` types for operations that can legitimately fail
- Collect and continue for formatting errors, fail-fast for parsing errors

#### RuleIter Consumption Patterns

`RuleIter` enforces complete consumption via its `Drop` implementation. This can cause assertion failures if validation functions return early before calling `iter.done()`.

**Current workaround:** Extract all data first, then validate:

```rust
// Extract data without validation
let (limit_pair, offset_pair) = /* extract pairs */;
iter.done(); // Call before any early returns

// Then validate using functional patterns
let count_mode = limit_pair.map(|pair| CountMode::parse_pair(extensions, pair)).transpose()?;
```

_Note: The RuleIter interface could be improved to handle this pattern more naturally._

#### Documentation Formatting

##### Rustdoc Markdown Formatting

Since markdown files are included in Rustdoc, use `##` for actual `#` characters in code examples (especially important for Substrait extension syntax). Lines starting with `# ` are hidden in documentation but included for compilation. Run `cargo test --doc` to verify examples compile correctly.

```rust
// Correct: ## becomes # in Substrait syntax
## 10 @  1: add

// Incorrect: # becomes a markdown header
# 10 @  1: add
```

##### Grammar Documentation Best Practices

When working with [`GRAMMAR.md`](GRAMMAR.md):

- Use PEG format with `rule_name := definition` syntax
- Run `cargo test --doc` to verify all code examples compile
- Ensure all referenced identifiers are properly defined somewhere in the grammar
- Treat `GRAMMAR.md` as the user-facing text-format contract. For syntax changes, update the docs, Pest grammar, parser, textifier, and roundtrip tests together so accepted input and produced output stay aligned.
- Describe the user-facing grammar clearly rather than copying Pest syntax directly. Pest-specific details such as `~` for concatenation and `|` for choice belong in `src/parser/expression_grammar.pest`, not in the documentation grammar. The syntax described by `GRAMMAR.md` and the Pest grammar should match; however, naming and/or structuring (e.g. inlining rules) may differ so that `GRAMMAR.md` prioritizes user-facing documentation whereas the Pest grammar must also lead to code generation.

#### Testing Guidelines

**Roundtrip testing**: The most effective validation is parse→format→parse comparison. Parse text to protobuf, format back to text, and verify the output matches the input. See `tests/plan_roundtrip.rs` for examples.

When changing protobuf-to-text behavior, include tests for raw, malformed, older, or partially populated protobuf inputs when relevant. Textification should surface missing or unsupported fields through accumulated errors and `!{...}` output rather than panicking or silently producing text the parser cannot read.

**Assert on expected values, not just properties**: When testing structured output, prefer comparing against concrete expected values rather than only checking lengths or other surface-level properties. Use judgment for deeply nested structures where building a full expected value would be unwieldy — property-based checks are fine there. For example:

```rust
// Good: catches ordering, content, and structural bugs
assert_eq!(result, vec![0, 1, 2]);

// Weaker: only catches missing or extra entries
assert_eq!(result.len(), 3);
```

**TODO**: Add comprehensive edge case testing for invalid enum values, unknown relation types, malformed input, and boundary conditions to ensure graceful error handling.

### Pull Request Process

1. Fork the repository
2. Create a feature branch (`git checkout -b feature-name`)
3. Make your changes
4. Add tests for new functionality
5. Commit your changes with a clear message
   - Pre-commit hooks run formatting and clippy; run `just test` locally or rely on CI for tests and examples
6. Push to the branch (`git push origin feature-name`)
7. Open a Pull Request

**PR Guidelines:**

- Keep changes focused and atomic
- Make the PR title and description match the actual scope. If one diff implements multiple concepts, split it or call out the combined scope explicitly so reviewers can reason about it.
- Use [Conventional Commits](https://www.conventionalcommits.org/) syntax for PR titles, which are automatically compiled into our Changelog.
- Include tests for new functionality.
- Update documentation if needed.
- For public API changes, state whether the change preserves compatibility, intentionally breaks compatibility, or narrows an accidentally exposed internal surface.
- Ensure all CI checks pass
  - You may wish to leave it in 'Draft' form until you validate this
- Provide a clear description of what the PR does
- Link to any related issues

### Releases

Releases are automated via [release-plz](https://release-plz.dev/). See
[RELEASING.md](RELEASING.md) for details. Contributors don't need to worry about
version bumping or changelog entries — just use
[conventional commit](https://www.conventionalcommits.org/) messages and the
release automation handles the rest.

## Contributing to Issues

### Feature Requests

Feature Requests should:

- Clearly state the missing feature at a user level.
  - If the feature is support for a particular Substrait feature, describe the Substrait feature in terms of Substrait itself (<https://substrait.io/>), not in terms of language-specific libraries.
- Give a reproducible example wherever possible / reasonable. This should include both the input and commands to reproduce, and the output / errors it produced.
  - For example, a Substrait feature would ideally include a JSON form of a plan with the feature in it, and show the output when running substrait-explain (e.g. the output of `substrait-explain convert ...`).
- (Optional) Possible solutions, with a recommendation.
  - If desired, suggest what some possible options are, and what your preference might be and why.
  - In particular, focus on the user-level API / syntax changes.

Example issue: <https://github.com/DataDog/substrait-explain/issues/160>

### Contributing to reporting bugs

If you think you have found a bug in Substrait-Explain, feel free to report it. When creating issues, please include:

- A clear description of the problem
- Steps to reproduce the issue
- Expected vs actual behavior
- For Substrait plan parsing issues, include the plan text or protobuf
- Your Rust version (`rustc --version`)
- Any relevant error messages or stack traces

### Contributing to triaging issues

Triaging issues is a great way to contribute to an open source project. Some actions you can perform on open issues that will help address them sooner:

- **Trying to reproduce the issue**: If you can reproduce the issue following the steps the reporter provided, add a comment specifying that you could reproduce the issue.
- **Finding duplicates**: If there is a bug, there might be a chance that it was already reported in a different issue. If you find an already reported issue that is the same one as the one you are triaging, add a comment with "Duplicate of" followed by the issue number of the original one.
- **Asking the reporter for more information**: Sometimes the reporter of an issue doesn't include enough information to work on the fix, i.e. lack of steps to reproduce, not specifying the affected version, etc. If you find a bug that doesn't have enough information, add a comment tagging the reporter asking for the missing information.

## Questions or Need Help?

If you have questions or need help getting started, feel free to:

- Open an issue for discussion
- Ask questions in the issue tracker
- Check existing issues and pull requests for examples
- Review the [README.md](https://github.com/DataDog/substrait-explain/blob/main/README.md) for usage examples and documentation
