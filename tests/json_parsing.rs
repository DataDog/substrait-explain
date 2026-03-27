//! Integration tests for JSON plan parsing.

//! The extension type under test is `example.ParquetScanConfig`, defined in
//! `tests/json_parsing/parquet_scan.proto`. Its descriptor is built
//! programmatically in [`parquet_scan_descriptor_bytes`] so there is no
//! protoc build dependency.

use prost::{Message, Name};
use prost_types::{DescriptorProto, FieldDescriptorProto, FileDescriptorProto, FileDescriptorSet};
use substrait_explain::cli::{Cli, Commands, Format};
use substrait_explain::extensions::{
    Explainable, ExtensionArgs, ExtensionColumn, ExtensionError, ExtensionRegistry,
    ExtensionRelationType, ExtensionValue,
};
use substrait_explain::json::{build_descriptor_pool, parse_json};
use substrait_explain::parser::Parser;
use substrait_explain::{OutputOptions, format_with_registry};

#[derive(Clone, PartialEq, Message)]
struct ParquetScanConfig {
    #[prost(string, tag = "1")]
    path: String,
    #[prost(int64, tag = "2")]
    batch_size: i64,
}

impl Name for ParquetScanConfig {
    const NAME: &'static str = "ParquetScanConfig";
    const PACKAGE: &'static str = "example";
    fn full_name() -> String {
        "example.ParquetScanConfig".to_string()
    }
    fn type_url() -> String {
        "type.googleapis.com/example.ParquetScanConfig".to_string()
    }
}

impl Explainable for ParquetScanConfig {
    fn name() -> &'static str {
        "ParquetScan"
    }

    fn from_args(args: &ExtensionArgs) -> Result<Self, ExtensionError> {
        let mut x = args.extractor();
        let path: &str = x.expect_named_arg("path")?;
        let batch_size: i64 = x.get_named_or("batch_size", 1024)?;
        x.check_exhausted()?;
        Ok(Self {
            path: path.to_string(),
            batch_size,
        })
    }

    fn to_args(&self) -> Result<ExtensionArgs, ExtensionError> {
        let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf);
        args.named.insert(
            "path".to_string(),
            ExtensionValue::String(self.path.clone()),
        );
        args.named.insert(
            "batch_size".to_string(),
            ExtensionValue::Integer(self.batch_size),
        );
        args.output_columns.push(ExtensionColumn::Named {
            name: "customer_id".to_string(),
            type_spec: "i64".to_string(),
        });
        args.output_columns.push(ExtensionColumn::Named {
            name: "amount".to_string(),
            type_spec: "fp64".to_string(),
        });
        Ok(args)
    }
}

fn build_registry() -> ExtensionRegistry {
    let mut r = ExtensionRegistry::new();
    r.register_relation::<ParquetScanConfig>().unwrap();
    r.add_descriptor(parquet_scan_descriptor_bytes());
    r
}

fn parquet_scan_descriptor_bytes() -> Vec<u8> {
    use prost_types::field_descriptor_proto::{Label, Type};

    let fds = FileDescriptorSet {
        file: vec![FileDescriptorProto {
            name: Some("parquet_scan.proto".to_string()),
            package: Some("example".to_string()),
            syntax: Some("proto3".to_string()),
            message_type: vec![DescriptorProto {
                name: Some("ParquetScanConfig".to_string()),
                field: vec![
                    FieldDescriptorProto {
                        name: Some("path".to_string()),
                        number: Some(1),
                        label: Some(Label::Optional as i32),
                        r#type: Some(Type::String as i32),
                        json_name: Some("path".to_string()),
                        ..Default::default()
                    },
                    FieldDescriptorProto {
                        name: Some("batch_size".to_string()),
                        number: Some(2),
                        label: Some(Label::Optional as i32),
                        r#type: Some(Type::Int64 as i32),
                        json_name: Some("batchSize".to_string()),
                        ..Default::default()
                    },
                ],
                ..Default::default()
            }],
            ..Default::default()
        }],
    };
    fds.encode_to_vec()
}

fn format_plan(plan: &substrait::proto::Plan) -> String {
    let registry = build_registry();
    let (text, errors) = format_with_registry(plan, &OutputOptions::default(), &registry);
    assert!(
        errors.is_empty(),
        "unexpected formatting errors: {errors:?}"
    );
    text
}

const PLAN_TEXT: &str = include_str!("json_parsing/plan.substrait");
const PLAN_PBJSON: &str = include_str!("json_parsing/plan_pbjson.json"); // rust json
const PLAN_PROTOJSON: &str = include_str!("json_parsing/plan_protojson.json"); // go json

#[test]
fn test_text_path() {
    let registry = build_registry();
    let plan = Parser::new()
        .with_extension_registry(registry)
        .parse_plan(PLAN_TEXT)
        .expect("failed to parse text plan");

    let serialized = serde_json::to_string_pretty(&plan).expect("failed to serialize");
    assert_eq!(
        serialized.trim(),
        PLAN_PBJSON.trim(),
        "plan_pbjson.json is out of sync — re-generate from the text fixture"
    );
}

/// Verify the go protojson fixture is a valid encoding of the same plan as the
/// text fixture.
#[test]
fn test_protojson_fixture_matches_text_plan() {
    let ext_descriptor = parquet_scan_descriptor_bytes();
    let pool =
        build_descriptor_pool(&[&ext_descriptor]).expect("failed to build pool with extension");
    let plan_from_protojson =
        parse_json(PLAN_PROTOJSON, &pool).expect("failed to parse protojson fixture");

    let plan_from_text = Parser::new()
        .with_extension_registry(build_registry())
        .parse_plan(PLAN_TEXT)
        .expect("failed to parse text fixture");

    assert_eq!(
        format_plan(&plan_from_text),
        format_plan(&plan_from_protojson),
        "plan_protojson.json is out of sync — re-generate from the text fixture"
    );
}

/// `parse_json` correctly decodes protojson (Go @type encoding) when the
/// contained type's descriptor is in the pool.
#[test]
fn test_gojson_parsing() {
    let registry = build_registry();
    let plan_from_text = Parser::new()
        .with_extension_registry(registry)
        .parse_plan(PLAN_TEXT)
        .expect("failed to parse text plan");

    let ext_descriptor = parquet_scan_descriptor_bytes();
    let pool =
        build_descriptor_pool(&[&ext_descriptor]).expect("failed to build pool with extension");
    let plan_from_protojson = parse_json(PLAN_PROTOJSON, &pool).expect("failed to parse protojson");

    assert_eq!(
        format_plan(&plan_from_text),
        format_plan(&plan_from_protojson),
        "protojson parse produced a different plan than text parse"
    );
}

/// `parse_json` correctly decodes pbjson (Rust typeUrl/value encoding) via the
/// serde_json path — no extension descriptor needed for this format.
#[test]
fn test_parse_rustjson() {
    let pool = build_descriptor_pool(&[]).expect("failed to build core pool");
    let plan = parse_json(PLAN_PBJSON, &pool).expect("failed to parse pbjson");
    assert_eq!(
        format_plan(&plan),
        format_plan(
            &Parser::new()
                .with_extension_registry(build_registry())
                .parse_plan(PLAN_TEXT)
                .unwrap()
        )
    );
}

/// `parse_json` fails when the Any type URL is not in the descriptor pool.
/// prost-reflect cannot decode the inlined @type fields without the schema.
#[test]
fn test_failed_parse_without_schema_on_gojson() {
    let pool = build_descriptor_pool(&[]).expect("failed to build core pool");
    let result = parse_json(PLAN_PROTOJSON, &pool);
    assert!(
        result.is_err(),
        "parse_json should fail when extension schema is absent from pool"
    );
}

// ---------------------------------------------------------------------------
// CLI Format::Json — runtime pbjson → protojson fallback
// ---------------------------------------------------------------------------

fn make_cli(from: Format) -> Cli {
    Cli {
        command: Commands::Convert {
            input: "-".to_string(),
            output: "-".to_string(),
            from: Some(from),
            to: Some(Format::Text),
            show_literal_types: false,
            show_expression_types: false,
            verbose: false,
        },
    }
}

#[test]
fn test_cli_parses_rustjson() {
    let cli = make_cli(Format::Json);
    let registry = build_registry();
    let mut output = Vec::new();

    cli.run_with_io(std::io::Cursor::new(PLAN_PBJSON), &mut output, &registry)
        .expect("CLI failed to parse pbjson");

    let result = String::from_utf8(output).unwrap();
    assert!(
        result.contains("ParquetScan"),
        "expected ParquetScan in output"
    );
    assert!(result.contains("data/sales.parquet"));
}

#[test]
fn test_cli_parses_gojson() {
    let cli = make_cli(Format::Json);
    let registry = build_registry();
    let mut output = Vec::new();

    cli.run_with_io(std::io::Cursor::new(PLAN_PROTOJSON), &mut output, &registry)
        .expect("CLI failed to parse go protojson");

    let result = String::from_utf8(output).unwrap();
    assert!(
        result.contains("ParquetScan"),
        "expected ParquetScan in output"
    );
    assert!(result.contains("data/sales.parquet"));
}

#[test]
fn test_cli_parses_standard_plan_json() {
    // A plan with no google.protobuf.Any fields — pbjson and protojson are
    // identical for standard proto3 messages, so either path should work.
    let standard_json = r#"{
      "relations": [{
        "root": {
          "input": {
            "read": {
              "baseSchema": {
                "names": ["a", "b"],
                "struct": {
                  "types": [
                    {"i64": {"nullability": "NULLABILITY_REQUIRED"}},
                    {"string": {"nullability": "NULLABILITY_NULLABLE"}}
                  ]
                }
              },
              "namedTable": {"names": ["data"]}
            }
          },
          "names": ["a", "b"]
        }
      }]
    }"#;

    let cli = make_cli(Format::Json);
    let mut output = Vec::new();

    cli.run_with_io(
        std::io::Cursor::new(standard_json),
        &mut output,
        &ExtensionRegistry::default(),
    )
    .expect("CLI failed to parse standard plan JSON");

    let result = String::from_utf8(output).unwrap();
    assert!(result.contains("Read[data => a:i64, b:string?]"));
}
