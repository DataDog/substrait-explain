fmt:
    cargo +nightly fmt -- --unstable-features --config imports_granularity=Module,group_imports=StdExternalCrate

fix:
    cargo clippy --fix --allow-dirty --allow-staged

test:
    cargo test

check:
    cargo clippy