# Format the code.
fmt:
    cargo +nightly fmt -- --unstable-features --config imports_granularity=Module,group_imports=StdExternalCrate

# Run clippy to check for linting errors and fix them.
fix:
    cargo clippy --fix --allow-dirty --allow-staged

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