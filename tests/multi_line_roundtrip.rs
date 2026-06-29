//! Round-trip tests for multi-line relation syntax.
//!
//! A `Read:Virtual` may be written across several lines using `- ` continuation
//! markers. Layout is handled entirely in the chunk layer (grouping physical
//! lines) and the grammar (silent continuation rules), so a multi-line relation
//! parses to the same plan as its inline form — and currently formats back to
//! the inline canonical, since multi-line output is not yet implemented.

mod common;

use common::assert_roundtrip_canonical;
use substrait_explain::Parser;

#[test]
fn test_virtual_read_multiline_single_row() {
    let inline = r#"
=== Plan
Root[id, name]
  Read:Virtual[(1, 'alice') => id:i64, name:string]"#;

    let multiline = r#"
=== Plan
Root[id, name]
  Read:Virtual[
    - (1, 'alice')
    - => id:i64, name:string]"#;

    assert_roundtrip_canonical(inline.trim(), multiline.trim());
}

#[test]
fn test_virtual_read_multiline_multi_row() {
    // Rows carry trailing commas; the last row before `=>` does not.
    let inline = r#"
=== Plan
Root[id, name]
  Read:Virtual[(1, 'alice'), (2, 'bob'), (3, 'carol') => id:i64, name:string]"#;

    let multiline = r#"
=== Plan
Root[id, name]
  Read:Virtual[
    - (1, 'alice'),
    - (2, 'bob'),
    - (3, 'carol')
    - => id:i64, name:string]"#;

    assert_roundtrip_canonical(inline.trim(), multiline.trim());
}

#[test]
fn test_virtual_read_multiline_typed_null() {
    let inline = r#"
=== Plan
Root[id, name]
  Read:Virtual[(1, null:string?), (2, 'bob') => id:i64, name:string?]"#;

    let multiline = r#"
=== Plan
Root[id, name]
  Read:Virtual[
    - (1, null:string?),
    - (2, 'bob')
    - => id:i64, name:string?]"#;

    assert_roundtrip_canonical(inline.trim(), multiline.trim());
}

#[test]
fn test_virtual_read_multiline_nested_in_plan() {
    // A multi-line virtual read nested under another relation, indented deeper.
    let inline = r#"
=== Extensions
URNs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_comparison.yaml
Functions:
  # 10 @  1: gt

=== Plan
Root[name]
  Filter[gt($0, 1):boolean => $0, $1]
    Read:Virtual[(1, 'alice'), (2, 'bob') => id:i64, name:string]"#;

    let multiline = r#"
=== Extensions
URNs:
  @  1: https://github.com/substrait-io/substrait/blob/main/extensions/functions_comparison.yaml
Functions:
  # 10 @  1: gt

=== Plan
Root[name]
  Filter[gt($0, 1):boolean => $0, $1]
    Read:Virtual[
      - (1, 'alice'),
      - (2, 'bob')
      - => id:i64, name:string]"#;

    assert_roundtrip_canonical(inline.trim(), multiline.trim());
}

#[test]
fn test_virtual_read_multiline_tolerates_trailing_whitespace() {
    // Trailing spaces after `[`, after a row comma, and after a row must not
    // break parsing — the old line-based parser trimmed each physical line, so
    // files with harmless trailing whitespace must keep working. `concat!`
    // keeps the (otherwise invisible) trailing spaces explicit.
    let inline = "=== Plan\n\
                  Root[id, name]\n  \
                    Read:Virtual[(1, 'alice'), (2, 'bob') => id:i64, name:string]";
    let multiline = concat!(
        "=== Plan\n",
        "Root[id, name]\n",
        "  Read:Virtual[  \n",     // trailing spaces after `[`
        "    - (1, 'alice'),  \n", // trailing spaces after a row comma
        "    - (2, 'bob')  \n",    // trailing spaces after a row
        "    - => id:i64, name:string]",
    );

    assert_roundtrip_canonical(inline, multiline);
}

#[test]
fn test_virtual_read_multiline_rejects_child() {
    // A child relation (not a `- ` line) under a multi-line virtual read is not
    // merged into the chunk; it becomes a child, and Read nodes have no children.
    let plan = r#"
=== Plan
Root[id]
  Read:Virtual[
    - (1)
    - => id:i64]
    Read[t => x:i64]"#;

    assert!(
        Parser::parse(plan.trim()).is_err(),
        "child relation under multi-line Read:Virtual should be rejected"
    );
}

#[test]
fn test_virtual_read_multiline_rejects_stray_continuation() {
    // A `- ` continuation line after the relation closes is merged into the
    // chunk but left unconsumed by the grammar; it must error, not be dropped.
    let plan = r#"
=== Plan
Root[id]
  Read:Virtual[
    - (1)
    - => id:i64]
    - (99)"#;

    assert!(
        Parser::parse(plan.trim()).is_err(),
        "stray continuation line after Read:Virtual should be rejected"
    );
}

#[test]
fn test_virtual_read_multiline_rejects_malformed_row() {
    // A continuation line that is not a valid virtual_row fails to parse.
    let plan = r#"
=== Plan
Root[id]
  Read:Virtual[
    - not_a_row
    - => id:i64]"#;

    assert!(
        Parser::parse(plan.trim()).is_err(),
        "malformed continuation row should fail to parse"
    );
}
