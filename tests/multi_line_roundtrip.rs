//! Round-trip tests for multi-line relation syntax.
//!
//! A `Read:Virtual` may be written across several lines using `- ` continuation
//! markers. Parsing is handled entirely in the chunk layer (grouping physical
//! lines) and the grammar (silent continuation rules), so a multi-line relation
//! parses to the same plan as its inline form. Formatting emits the multi-line
//! form once a virtual table reaches
//! [`OutputOptions::virtual_table_multiline_threshold`] rows (default 3), and
//! the inline form below that.

mod common;

use common::assert_roundtrip_canonical;
use substrait_explain::{OutputOptions, Parser, format_with_options};

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
    // With three rows (the default threshold) the multi-line form is canonical:
    // rows carry trailing commas, and the last row before `=>` does not.
    let multiline = r#"
=== Plan
Root[id, name]
  Read:Virtual[
    - (1, 'alice'),
    - (2, 'bob'),
    - (3, 'carol')
    - => id:i64, name:string]"#;

    let inline = r#"
=== Plan
Root[id, name]
  Read:Virtual[(1, 'alice'), (2, 'bob'), (3, 'carol') => id:i64, name:string]"#;

    assert_roundtrip_canonical(multiline.trim(), inline.trim());
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

#[test]
fn test_virtual_read_two_rows_stays_inline() {
    // Two rows is below the default threshold (3), so the canonical output is
    // inline even though the multi-line form parses to the same plan.
    let inline = r#"
=== Plan
Root[id, name]
  Read:Virtual[(1, 'alice'), (2, 'bob') => id:i64, name:string]"#;

    let multiline = r#"
=== Plan
Root[id, name]
  Read:Virtual[
    - (1, 'alice'),
    - (2, 'bob')
    - => id:i64, name:string]"#;

    assert_roundtrip_canonical(inline.trim(), multiline.trim());
}

#[test]
fn test_virtual_read_threshold_is_configurable() {
    // Raising the threshold keeps an otherwise multi-line table (3 rows) inline.
    let inline = r#"
=== Plan
Root[id]
  Read:Virtual[(1), (2), (3) => id:i64]"#;

    let plan = Parser::parse(inline.trim()).expect("parse failed");
    let options = OutputOptions {
        virtual_table_multiline_threshold: 100,
        ..OutputOptions::default()
    };
    let (text, errors) = format_with_options(&plan, &options);

    assert!(errors.is_empty(), "unexpected formatting errors: {errors:?}");
    assert_eq!(text.trim(), inline.trim());
}

#[test]
fn test_virtual_read_empty_stays_inline_even_at_zero_threshold() {
    // An empty virtual table is written `_`; there is no `- _` row form, so it
    // must stay inline even when the threshold (0) would otherwise force rows —
    // otherwise the formatted text would not parse back.
    let inline = r#"
=== Plan
Root[id, name]
  Read:Virtual[_ => id:i64, name:string]"#;

    let plan = Parser::parse(inline.trim()).expect("parse failed");
    let options = OutputOptions {
        virtual_table_multiline_threshold: 0,
        ..OutputOptions::default()
    };
    let (text, errors) = format_with_options(&plan, &options);

    assert!(errors.is_empty(), "unexpected formatting errors: {errors:?}");
    assert_eq!(text.trim(), inline.trim());

    // The output must also parse back cleanly.
    Parser::parse(text.trim()).expect("formatted empty virtual table should re-parse");
}
