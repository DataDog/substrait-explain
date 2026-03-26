# Default target is to run all checks and tests, including the examples.
default: check test examples

# Run clippy and fmt to check for linting errors and fix them.
fix: fmt
    cargo clippy --examples --tests --all-features --fix --allow-dirty --allow-staged

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
    cargo test --all-features
    cargo run --features cli,protoc -- validate -i example-plans/basic.substrait
    cargo run --features cli,protoc -- validate -i example-plans/simple.substrait

# Run clippy to check for linting errors.
check:
    cargo clippy --examples --tests --all-features

examples:
    cargo run --example basic_usage
    cargo run --example advanced_usage --features serde
    cargo run --example extensions --features cli

# Generate the LICENSE-3rdparty.csv file from the Cargo.lock file, for tracking
# licenses of third-parties.
licenses:
    bash generate_licenses.sh

# Run the CLI with the given arguments. Usage: just run [args...]
# Example: just run convert --help
# Example: just run convert --from text --to json < input.txt
run *args:
    cargo run --features cli -- {{args}}

# Tag the current version and create a GitHub Release. Run after CI publishes
# to crates.io (step 5 in RELEASING.md). Requires maintainer permissions.
release-tag:
    #!/usr/bin/env bash
    set -euo pipefail
    version=$(cargo metadata --no-deps --format-version=1 | jq -r '.packages[0].version')
    tag="v${version}"

    # Guard: don't re-tag
    if git rev-parse "${tag}" >/dev/null 2>&1; then
        echo "Error: tag ${tag} already exists" >&2
        exit 1
    fi

    # Guard: version should be published on crates.io
    if ! curl -sf "https://crates.io/api/v1/crates/substrait-explain/${version}" >/dev/null; then
        echo "Error: ${version} not found on crates.io — wait for CI to publish first" >&2
        exit 1
    fi

    # Find the commit that set this version in Cargo.toml, so we tag the
    # right commit even if main has moved forward since the release.
    commit=$(git log --format="%H" -1 -S "version = \"${version}\"" -- Cargo.toml)
    if [ -z "$commit" ]; then
        echo "Error: could not find commit that set version ${version}" >&2
        exit 1
    fi

    # Extract this version's changelog section (between its ## header and the next)
    notes=$(awk "/^## \[${version}\]/{found=1; next} /^## \[/{if(found) exit} found{print}" CHANGELOG.md)
    if [ -z "$notes" ]; then
        notes="See CHANGELOG.md"
    fi

    echo "Tagging ${tag} at ${commit:0:12}..."
    git tag "${tag}" "${commit}"
    git push origin "${tag}"
    gh release create "${tag}" --title "${tag}" --notes "${notes}"

# Create a release PR for current main. Will bump the version number and create
# a changelog, based on conventional commits + semantic versioning.
release-pr:
    # Must be on main to release
    git checkout main
    git pull origin main
    release-plz release-pr --git-token $(gh auth token)