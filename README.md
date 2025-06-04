# Substrait-Explain

## Formatting

Formatting will happen automatically via the git hook and in CI, using `cargo fmt`.

To reorder imports, I suggest:

```sh
cargo +nightly fmt -- --unstable-features --config imports_granularity=Module,group_imports=StdExternalCrate
```

This uses two unstable features to improve how imports are handled; it looks nicer that way.
