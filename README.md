# Substrait-Explain

## Setup

This project uses a fairly standard rust setup; test with `cargo test`, build with `cargo build`, standard rust integration.

### Linting: Clippy

This project uses [clippy](https://doc.rust-lang.org/clippy/) for linting. This is enforced in CI, and integrations are available in most IDEs.

### Pre-Commit Hooks

Run `bash pre-commit.sh install` to install the pre-commit Git hook, which will run `cargo fmt` and `cargo clippy -- -D warnings` whenever `git commit` is called, validating that formatting and linting pass locally.

## Formatting

Formatting will happen automatically via the git hook and in CI, using `cargo fmt`. This uses standard `cargo fmt` features.

### Imports

To format imports a bit more thoroughly, there are two unstable `rustfmt` [features](https://rust-lang.github.io/rustfmt/?version=v1.8.0) that will do this. If you have `rustup` installed and a nightly release, you can format with these:

```sh
cargo +nightly fmt -- --unstable-features --config imports_granularity=Module,group_imports=StdExternalCrate
```

- [`imports_granularity`](https://rust-lang.github.io/rustfmt/?version=v1.8.0&search=#imports_granularity) standardizes when use statements are merged or split on the same line with `{}` brackets. `Module` puts everything from the same module on one line, different modules on different lines.
- [`group_imports`](https://rust-lang.github.io/rustfmt/?version=v1.8.0&search=#group_imports) sets how imports are grouped; `StdExternalCrate` puts standard library in one group, external dependencies in another, and ones from this crate in a third.

As these require a nightly release, they aren't currently part of the git hooks or CI.
