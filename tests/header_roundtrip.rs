//! Tests for the `=== Header` section, which carries plan-level metadata such
//! as `Plan.version`.

mod common;

use common::roundtrip_plan;
use substrait_explain::{Parser, format};

/// A header with a full version (numbers + producer + git_hash) nested under a
/// compact `Version:` line round-trips exactly.
#[test]
fn test_full_version_roundtrip() {
    let input = r#"
=== Header
Version: 0.64.0
  producer: "datafusion-substrait 46.0.0"
  git_hash: f455ecbe8b581c9b93abd6fbf4a360ec54b196d9

=== Plan
Root[col]
  Read[table => col:i32]
"#
    .trim_start();
    roundtrip_plan(input);
}

/// A header with only version numbers round-trips with no nested fields.
#[test]
fn test_version_only_roundtrip() {
    let input = r#"
=== Header
Version: 1.2.3

=== Plan
Root[col]
  Read[table => col:i32]
"#
    .trim_start();
    roundtrip_plan(input);
}

/// A header that carries only metadata (all version numbers zero) is written
/// without a `Version:` line, at the top level.
#[test]
fn test_metadata_only_roundtrip() {
    let input = r#"
=== Header
producer: "my-producer"

=== Plan
Root[col]
  Read[table => col:i32]
"#
    .trim_start();
    roundtrip_plan(input);
}

/// A header combined with an extensions section round-trips, preserving section
/// order: Header, Extensions, Plan.
#[test]
fn test_header_with_extensions_roundtrip() {
    let input = r#"
=== Header
Version: 0.64.0

=== Extensions
URNs:
  @  1: /urn/arithmetic
Functions:
  # 10 @  1: add

=== Plan
Project[$0, add($0, $0):i32]
  Read[table => col:i32]
"#
    .trim_start();
    roundtrip_plan(input);
}

/// Plans without a header continue to parse and format unchanged.
#[test]
fn test_no_header_roundtrip() {
    let input = r#"
=== Plan
Root[col]
  Read[table => col:i32]
"#
    .trim_start();
    roundtrip_plan(input);
}

/// The version fields are populated on the parsed `Plan`.
#[test]
fn test_version_populated_on_plan() {
    let input = r#"
=== Header
Version: 0.64.0
  producer: "datafusion-substrait 46.0.0"
  git_hash: abc123

=== Plan
Root[col]
  Read[table => col:i32]
"#;
    let plan = Parser::parse(input).expect("parse failed");
    let version = plan.version.expect("expected a version");
    assert_eq!(version.major_number, 0);
    assert_eq!(version.minor_number, 64);
    assert_eq!(version.patch_number, 0);
    assert_eq!(version.producer, "datafusion-substrait 46.0.0");
    assert_eq!(version.git_hash, "abc123");
}

/// A plan with no header has no version.
#[test]
fn test_no_header_has_no_version() {
    let input = "=== Plan\nRoot[col]\n  Read[table => col:i32]";
    let plan = Parser::parse(input).expect("parse failed");
    assert!(plan.version.is_none());
}

/// An all-empty version (manually constructed) produces no header section.
#[test]
fn test_empty_version_omits_header() {
    let mut plan = Parser::parse("=== Plan\nRoot[col]\n  Read[table => col:i32]").unwrap();
    plan.version = Some(substrait::proto::Version::default());

    let (text, errors) = format(&plan);
    assert!(errors.is_empty(), "unexpected errors: {errors:?}");
    assert!(
        !text.contains("=== Header"),
        "empty version should not emit a header:\n{text}"
    );
}
