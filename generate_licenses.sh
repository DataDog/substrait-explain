#!/bin/bash

# Script to generate LICENSE-3rdparty.csv from Cargo.lock using cargo-license
# This script provides an easy way to generate the required license file

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if cargo-license is installed
if ! command -v cargo-license &> /dev/null; then
    print_error "cargo-license is not installed. Installing it now..."
    cargo install cargo-license
fi

print_status "Generating LICENSE-3rdparty.csv from Cargo.lock using cargo-license..."

# Create the output CSV file
OUTPUT_FILE="LICENSE-3rdparty.csv"

# Write CSV header
echo "Component,Origin,License,Copyright" > "$OUTPUT_FILE"

# Process each line and convert to required format
print_status "Converting to required CSV format..."

# Get license information and process each dependency (skip header line)
# Use awk to parse TSV and format as CSV
# cargo-license TSV fields: $1=name, $2=version, $3=authors, $4=repository, $5=license, $6=license_file, $7=description
cargo license --tsv | tail -n +2 | awk -F'\t' '
    $1 != "substrait-explain" {
        authors = $3
        gsub(/^[ \t]+|[ \t]+$/, "", authors) # Trim leading and trailing whitespace
        if (authors == "") {
            authors = "the " $1 " Authors"
        }
        print $1 "," $4 "," $5 ",\"" authors "\""
    }' >> "$OUTPUT_FILE"

# Count the number of dependencies (excluding header)
DEPENDENCY_COUNT=$(($(wc -l < "$OUTPUT_FILE") - 1))

print_status "Successfully generated $OUTPUT_FILE"
print_status "Generated CSV contains $DEPENDENCY_COUNT dependencies"

echo ""
echo "Generated $OUTPUT_FILE contains:"
echo "Component,Origin,License,Copyright"
echo "----------------------------------------"
head -5 "$OUTPUT_FILE"
echo "..."
echo "Total lines: $(wc -l < "$OUTPUT_FILE")"

print_status "License generation complete!"