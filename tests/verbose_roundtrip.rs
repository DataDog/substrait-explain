//! Round-trip tests for compact and verbose relation output styles.

mod common;

use common::assert_roundtrip_verbose;

#[test]
fn test_filter_empty_output_styles() {
    let compact = r#"=== Plan
Root[_]
  Filter[$2 => _]
    Read[table => a:i32, b:string, c:boolean]"#;
    let verbose = r#"=== Plan
Root[_]
  Filter[$2 |> _]
    Read[table => a:i32, b:string, c:boolean]"#;

    assert_roundtrip_verbose(compact, verbose);
}

#[test]
fn test_sort_partial_reordered_output_styles() {
    let compact = r#"=== Plan
Root[c, a]
  Sort[($1, &DescNullsLast), ($0, &AscNullsFirst) => $2, $0]
    Read[table => a:i32, b:string, c:boolean]"#;
    let verbose = r#"=== Plan
Root[c, a]
  Sort[($1, &DescNullsLast), ($0, &AscNullsFirst) |> $2, $0]
    Read[table => a:i32, b:string, c:boolean]"#;

    assert_roundtrip_verbose(compact, verbose);
}

#[test]
fn test_fetch_single_high_index_output_styles() {
    let compact = r#"=== Plan
Root[d]
  Fetch[limit=10, offset=2 => $3]
    Read[table => a:i32, b:string, c:boolean, d:i64]"#;
    let verbose = r#"=== Plan
Root[d]
  Fetch[limit=10:i64, offset=2:i64 |> $3]
    Read[table => a:i32, b:string, c:boolean, d:i64]"#;

    assert_roundtrip_verbose(compact, verbose);
}

#[test]
fn test_set_three_input_output_styles() {
    let compact = r#"=== Plan
Root[c, a, b]
  Set[&UnionAll => $2, $0, $1]
    Read[left => a:i32, b:string, c:boolean]
    Read[middle => a:i32, b:string, c:boolean]
    Read[right => a:i32, b:string, c:boolean]"#;
    let verbose = r#"=== Plan
Root[c, a, b]
  Set[&UnionAll |> $2, $0, $1]
    Read[left => a:i32, b:string, c:boolean]
    Read[middle => a:i32, b:string, c:boolean]
    Read[right => a:i32, b:string, c:boolean]"#;

    assert_roundtrip_verbose(compact, verbose);
}

#[test]
fn test_cross_uneven_input_output_styles() {
    let compact = r#"=== Plan
Root[right_c, left, right_b]
  Cross[_ => $3, $0, $2]
    Read[left => left:i32]
    Read[right => right_a:string, right_b:i64, right_c:boolean]"#;
    let verbose = r#"=== Plan
Root[right_c, left, right_b]
  Cross[_ |> $3, $0, $2]
    Read[left => left:i32]
    Read[right => right_a:string, right_b:i64, right_c:boolean]"#;

    assert_roundtrip_verbose(compact, verbose);
}
