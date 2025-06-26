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
