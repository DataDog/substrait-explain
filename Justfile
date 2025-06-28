# Run clippy and fmt to check for linting errors and fix them.
fix: fmt
    cargo clippy --all-features --fix --allow-dirty --allow-staged

# Run rustfmt to format the code. We use the --config flag to get a couple
# of extra features around imports handling. These features are unstable, so
# you can't put them in the rustfmt.toml without using nightly (something
# that doesn't seem like a great idea), but... it turns out that you can use
# the --config flag to get the same effect:
# https://github.com/rust-lang/rustfmt/issues/5511
#
# Format the code.
fmt:
    cargo fmt -- --config imports_granularity=Module,group_imports=StdExternalCrate

# Run the tests.
test:
    cargo test

# Run clippy to check for linting errors.
check:
    cargo clippy

# Generate the LICENSE-3rdparty.csv file from the Cargo.lock file, for tracking
# licenses of third-parties.
licenses:
    bash generate_licenses.sh
