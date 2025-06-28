# License Generation

This project uses a shell script to automatically generate a `LICENSE-3rdparty.csv` file from your Rust project's `Cargo.lock` dependencies, as required by DataDog's open source compliance requirements.

## Quick Start

```bash
./generate_licenses.sh
```

This will:

1. Check for required dependencies
2. Generate `LICENSE-3rdparty.csv` from your `Cargo.lock`

## Requirements

- Rust toolchain (for `cargo`)
- [`cargo-license`](https://github.com/onur/cargo-license) (the script will install it if missing)
- `Cargo.lock` file (run `cargo build` first if it doesn't exist)

## Output Format

The generated `LICENSE-3rdparty.csv` file contains:

- **Component**: Package name and version (e.g., "chrono 0.4.41")
- **Origin**: Source of the package (e.g., "crates.io")
- **License**: SPDX license identifier (e.g., "MIT", "Apache-2.0", or dual/multi-license expressions)
- **Copyright**: Copyright information for the package
