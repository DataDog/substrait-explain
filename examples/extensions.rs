//! Example demonstrating typed extension handlers using protobuf messages.
//!
//! This example shows how to:
//! 1. Define a custom protobuf message for extension configuration
//! 2. Implement `Explainable` for Substrait text round-tripping
//! 3. Parse Substrait text into the protobuf plan with a registry
//! 4. Recover the typed payload and format the plan back to text
//!
//! `AnyConvertible` is implemented automatically for any type that implements
//! `prost::Message` and `prost::Name`. If you are not using prost—or you need a
//! bespoke payload format—you can provide a manual `AnyConvertible`
//! implementation following the same pattern shown for `Explainable`.

use prost::{Message, Name};
use substrait::proto::{plan_rel, rel};
use substrait_explain::extensions::any::AnyRef;
use substrait_explain::extensions::{
    AnyConvertible, ExplainContext, Explainable, ExtensionArgs, ExtensionColumn, ExtensionError,
    ExtensionRegistry, ExtensionRelationType, ExtensionValue,
};
use substrait_explain::fixtures::parse_type;
use substrait_explain::parser::Parser;
use substrait_explain::{OutputOptions, format_with_registry};

/// Custom protobuf message for our ParquetScan extension configuration.
/// In a real implementation, this would be generated from a .proto file.
#[derive(Clone, PartialEq, Message)]
pub struct ParquetScanConfig {
    #[prost(string, tag = "1")]
    pub path: String,
    #[prost(int64, tag = "2")]
    pub batch_size: i64,
    #[prost(bool, tag = "3")]
    pub use_dictionary: bool,
    #[prost(string, repeated, tag = "4")]
    pub selected_columns: Vec<String>,
}

// Manually implement Name trait for our custom message
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

// Implement Explainable for our custom message
impl Explainable for ParquetScanConfig {
    fn name() -> &'static str {
        "TypedParquetScan"
    }

    fn from_args(args: &ExtensionArgs, _ctx: &ExplainContext) -> Result<Self, ExtensionError> {
        let mut extractor = args.extractor();
        // path is required
        let path: &str = extractor.expect_named_arg("path")?;
        // batch_size and use_dictionary are optional, with default values
        let batch_size: i64 = extractor.get_named_or("batch_size", 1024)?;
        let use_dictionary: bool = extractor.get_named_or("use_dictionary", true)?;

        // Validate there are no other named arguments
        extractor.check_exhausted()?;

        let selected_columns = args
            .output_columns
            .iter()
            .map(|column| match column {
                ExtensionColumn::Named { name, .. } => Ok(name.clone()),
                _ => Err(ExtensionError::InvalidArgument(format!(
                    "expected named columns only: {column:?}"
                ))),
            })
            .collect::<Result<Vec<String>, ExtensionError>>()?;

        Ok(ParquetScanConfig {
            path: path.to_string(),
            // batch_size and use_dictionary are optional, with default values
            batch_size,
            use_dictionary,
            selected_columns,
        })
    }

    fn to_args(&self, _ctx: &ExplainContext) -> Result<ExtensionArgs, ExtensionError> {
        let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf);

        // Add named arguments from the message
        args.named.insert(
            "path".to_string(),
            ExtensionValue::String(self.path.clone()),
        );
        args.named.insert(
            "batch_size".to_string(),
            ExtensionValue::Integer(self.batch_size),
        );
        args.named.insert(
            "use_dictionary".to_string(),
            ExtensionValue::Boolean(self.use_dictionary),
        );

        // Add output columns from selected columns
        for column_name in &self.selected_columns {
            args.output_columns.push(ExtensionColumn::Named {
                name: column_name.clone(),
                ty: parse_type("string?"),
            });
        }

        Ok(args)
    }
}

fn main() -> anyhow::Result<()> {
    let mut registry = ExtensionRegistry::new();
    registry.register_relation::<ParquetScanConfig>()?;

    let plan_text = r#"=== Plan
Root[customer_id, amount]
  ExtensionLeaf:TypedParquetScan[path='data/sales.parquet', batch_size=2048, use_dictionary=true => customer_id:i64, amount:fp64]"#;

    println!("Parsing plan text:\n---\n{plan_text}\n---");
    let plan = Parser::new()
        .with_extension_registry(registry.clone())
        .parse_plan(plan_text)?;
    println!("Parsed plan with {} relation(s).", plan.relations.len());

    let detail = extension_detail(&plan)?;
    let config = <ParquetScanConfig as AnyConvertible>::from_any(detail)?;
    println!("Decoded: {config:?}");

    let (roundtrip, errors) = format_with_registry(&plan, &OutputOptions::default(), &registry);
    if !errors.is_empty() {
        return Err(anyhow::anyhow!("formatting produced errors: {errors:?}"));
    }
    println!("Round-tripped text:\n{roundtrip}");

    let reparsed_plan = Parser::new()
        .with_extension_registry(registry)
        .parse_plan(&roundtrip)?;
    let reparsed_detail = extension_detail(&reparsed_plan)?;
    let reparsed_config = <ParquetScanConfig as AnyConvertible>::from_any(reparsed_detail)?;
    if reparsed_config != config {
        return Err(anyhow::anyhow!(
            "reparsed config does not match initial payload: {reparsed_config:?}"
        ));
    }

    if Parser::parse(plan_text).is_ok() {
        return Err(anyhow::anyhow!(
            "parsing without registry unexpectedly succeeded"
        ));
    }
    // else: Parsing without the registry fails, as expected.

    Ok(())
}

/// Extract the extension detail from the plan, for validation. Assumes Root -> ExtensionLeaf plan.
fn extension_detail<'a>(plan: &'a substrait::proto::Plan) -> Result<AnyRef<'a>, anyhow::Error> {
    assert!(plan.relations.len() == 1);
    let root = match plan.relations.first().unwrap() {
        substrait::proto::PlanRel {
            rel_type: Some(plan_rel::RelType::Root(root)),
        } => root,
        rel => return Err(anyhow::anyhow!("expected Root relation, got {rel:?}")),
    };

    let rel = root.input.as_ref().unwrap();

    match root.input.as_ref() {
        Some(substrait::proto::Rel {
            rel_type: Some(rel::RelType::ExtensionLeaf(leaf)),
        }) => leaf
            .detail
            .as_ref()
            .map(AnyRef::from)
            .ok_or_else(|| anyhow::anyhow!("expected Extension detail in parsed plan")),
        _ => Err(anyhow::anyhow!(
            "expected ExtensionLeaf relation, got {rel:?}"
        )),
    }
}
