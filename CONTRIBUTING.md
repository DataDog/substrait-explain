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

### Testing your changes

This project uses standard Rust testing practices:

```bash
# Run all tests
cargo test

# Run tests with output
cargo test -- --nocapture

# Run specific test file
cargo test --test types

# Run examples
cargo run --example basic_usage
cargo run --example advanced_usage --features serde

# View documentation
cargo doc --open
```

### Code Style

- Follow Rust conventions and idioms
- Use `cargo fmt` for formatting (enforced by pre-commit hooks)
- Run `cargo clippy` to catch common issues (enforced by pre-commit hooks)
- Add documentation for public APIs, and review them with `cargo doc --open`
- Include tests for new functionality

### Project-Specific Guidelines

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

_Note: Ideally, `pest_derive` would remove the need for unwraps by providing structural types that encode the invariants, ensuring at compile-time the grammar and code remain in sync._

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
// Use the Scope::expect pattern for graceful error handling
write!(w, "{}", ctx.expect(relation.input.as_ref()));

// Avoid protobuf field unwraps - they can legitimately be None
// RISKY:
let input = relation.input.as_ref().unwrap();
let schema = read.base_schema.as_ref().unwrap();

// Better: Use proper Option handling
write!(w, "{}", ctx.expect(relation.input.as_ref()));
```

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

#### Documentation Formatting

##### Rustdoc Markdown Formatting

Since the markdown files ([`API.md`](API.md), [`GRAMMAR.md`](GRAMMAR.md)) are included in Rustdoc, ensure `#` characters in code examples are escaped as `##` for proper interpretation. Examples in these files are automatically tested through Rustdoc compilation - any code in fenced blocks will be compiled and tested, ensuring documentation accuracy:

```rust
// Correct for Rustdoc (in markdown files)
# let plan_text = r#" // Single # gets stripped, line is included for test but excluded for doc output, so only the plan gets shown
=== Extensions
Functions:
  ## 10 @  1: add  // Note: ## becomes # in doc output
# "#;

// Incorrect for Rustdoc
let plan_text = r#"
=== Extensions
Functions:
  # 10 @  1: add  // This will be interpreted as a markdown header
"#;
```

This is especially important for Substrait extension syntax which uses `#` for anchors. In Rustdoc processing of code examples in backticks:

- Lines starting with `# ` have the `# ` stripped and the code is hidden in the output documentation
- `##` gets converted to `#` before being compiled as Rust code

Always verify that examples compile and run correctly.

##### Grammar Documentation Best Practices

When working with [`GRAMMAR.md`](GRAMMAR.md):

- **PEG Format**: All grammar rules should be defined in PEG (Parsing Expression Grammar) format using the `rule_name := definition` syntax
- **Consistency**: Use consistent markdown heading levels - convert bolded single-line items like `**Syntax**:` to proper markdown headings like `#### Syntax`
- **Organization**: Keep related concepts together - all common, basic expressions (enums, literals, identifiers, etc.) belong in Basic Terminals, while relation-specific rules belong with their respective relations
- **Cross-references**: Use markdown links to reference other sections rather than duplicating content
- **Testing**: Run `cargo test --doc` to verify all code examples in the grammar documentation compile and work correctly
- **Completeness**: Ensure all referenced identifiers are properly defined somewhere in the grammar

### Pull Request Process

1. Fork the repository
2. Create a feature branch (`git checkout -b feature-name`)
3. Make your changes
4. Add tests for new functionality
5. Commit your changes with a clear message
   - Pre-commit hooks will automatically run tests, clippy, and formatting
6. Push to the branch (`git push origin feature-name`)
7. Open a Pull Request

**PR Guidelines:**

- Keep changes focused and atomic
- Include tests for new functionality
- Update documentation if needed
- Ensure all CI checks pass
  - You may wish to leave it in 'Draft' form until you validate this
- Provide a clear description of what the PR does
- Link to any related issues

## Contributing to Issues

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
