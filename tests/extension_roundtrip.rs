//! Integration test for custom extension handlers with roundtrip parsing and formatting

use prost::{Message, Name};
use substrait::proto;
use substrait::proto::expression::RexType;
use substrait::proto::expression::literal::LiteralType;
use substrait::proto::r#type::Nullability;
use substrait_explain::extensions::examples::PartitionHint;
use substrait_explain::extensions::{
    EnumValue, ExplainContext, Explainable, Expr, ExtensionArgs, ExtensionColumn, ExtensionError,
    ExtensionRegistry, ExtensionRelationType, ExtensionValue, SimpleExtensions, TupleValue,
};
use substrait_explain::fixtures::parse_type;
use substrait_explain::format_with_registry;
use substrait_explain::parser::Parser;

/// A custom extension configuration for a hypothetical "UserTable" data source.
/// This differs from the file-based scan in the example by using logical table properties.
#[derive(Clone, PartialEq, Message)]
pub struct UserTableConfig {
    #[prost(string, tag = "1")]
    pub table_name: String,
    #[prost(int64, tag = "2")]
    pub version: i64,
    #[prost(bool, tag = "3")]
    pub is_temporary: bool,
    #[prost(string, repeated, tag = "4")]
    pub tracked_columns: Vec<String>,
}

// Implement Name trait for protobuf type URL
impl Name for UserTableConfig {
    const NAME: &'static str = "UserTableConfig";
    const PACKAGE: &'static str = "test";

    fn full_name() -> String {
        "test.UserTableConfig".to_string()
    }

    fn type_url() -> String {
        "type.googleapis.com/test.UserTableConfig".to_string()
    }
}

// Implement Explainable for text format conversion
impl Explainable for UserTableConfig {
    fn name() -> &'static str {
        "UserTable"
    }

    fn from_args(args: &ExtensionArgs, _ctx: &ExplainContext) -> Result<Self, ExtensionError> {
        let mut extractor = args.extractor();
        let table_name: &str = extractor.expect_named_arg("name")?;
        let version: i64 = extractor.get_named_or("version", 1)?;
        let is_temporary: bool = extractor.get_named_or("temp", false)?;

        extractor.check_exhausted()?;

        // Extract columns from output columns to populate tracked_columns
        let mut tracked_columns = Vec::new();
        for col in &args.output_columns {
            match col {
                ExtensionColumn::Named { name, .. } => {
                    tracked_columns.push(name.clone());
                }
                _ => {
                    return Err(ExtensionError::InvalidArgument(
                        "Expected named columns only".to_string(),
                    ));
                }
            }
        }

        Ok(UserTableConfig {
            table_name: table_name.to_string(),
            version,
            is_temporary,
            tracked_columns,
        })
    }

    fn to_args(&self, _ctx: &ExplainContext) -> Result<ExtensionArgs, ExtensionError> {
        let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf);

        // Add named arguments
        args.named.insert(
            "name".to_string(),
            ExtensionValue::String(self.table_name.clone()),
        );
        args.named
            .insert("version".to_string(), ExtensionValue::Integer(self.version));
        args.named.insert(
            "temp".to_string(),
            ExtensionValue::Boolean(self.is_temporary),
        );

        // Add output columns
        for column in &self.tracked_columns {
            args.output_columns.push(ExtensionColumn::Named {
                name: column.clone(),
                r#type: parse_type("string"),
            });
        }

        Ok(args)
    }
}

#[test]
fn test_extension_leaf_roundtrip() {
    let mut registry = ExtensionRegistry::new();
    registry.register_relation::<UserTableConfig>().unwrap();

    // Test plan with UserTable extension
    let plan_text = r#"
=== Plan
Root[result]
  ExtensionLeaf:UserTable[name='customers', version=2, temp=true => id:string, region:string]
"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("Failed to parse plan");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "Unexpected errors: {errors:?}");

    assert_eq!(formatted.trim(), plan_text.trim());
}

#[test]
fn test_extension_without_registry() {
    // Test that extensions work in different modes when registry is not available
    let plan_text = r#"
=== Plan
Root[result]
  ExtensionLeaf:UnknownExtension[arg1='value1', arg2=42 => col1:i32, col2:string]
"#;

    // Parse without registry - should fail by default
    let parser = Parser::default();
    let result = parser.parse_plan(plan_text);
    assert!(result.is_err());
}

#[test]
fn test_multiple_extensions_in_plan() {
    let mut registry = ExtensionRegistry::new();
    registry.register_relation::<UserTableConfig>().unwrap();

    // Also register a second type for variety
    #[derive(Clone, PartialEq, Message)]
    pub struct FilterConfig {
        #[prost(string, tag = "1")]
        pub expression: String,
    }

    impl Name for FilterConfig {
        const NAME: &'static str = "FilterConfig";
        const PACKAGE: &'static str = "test";

        fn full_name() -> String {
            "test.FilterConfig".to_string()
        }

        fn type_url() -> String {
            "type.googleapis.com/test.FilterConfig".to_string()
        }
    }

    impl Explainable for FilterConfig {
        fn name() -> &'static str {
            "TestFilter"
        }

        fn from_args(args: &ExtensionArgs, _ctx: &ExplainContext) -> Result<Self, ExtensionError> {
            let mut extractor = args.extractor();
            let expression: String = extractor.expect_named_arg::<&str>("expr")?.to_string();
            extractor.check_exhausted()?;

            Ok(FilterConfig { expression })
        }

        fn to_args(&self, _ctx: &ExplainContext) -> Result<ExtensionArgs, ExtensionError> {
            let mut args = ExtensionArgs::new(ExtensionRelationType::Single);
            args.named.insert(
                "expr".to_string(),
                ExtensionValue::String(self.expression.clone()),
            );
            Ok(args)
        }
    }

    registry.register_relation::<FilterConfig>().unwrap();

    // Plan with multiple extension types
    let plan_text = r#"
=== Plan
Root[result]
  ExtensionSingle:TestFilter[expr='status = "active"' => $0, $1]
    ExtensionLeaf:UserTable[name='users_prod', version=1, temp=false => id:i64, status:string]
"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("Failed to parse plan");

    assert_eq!(plan.relations.len(), 1);
}

/// Test-only protobuf payload used to verify literal argument round-tripping for
/// extensions. Holds a representative mixture of scalar literal types that the
/// text format should preserve without truncation or allocation leaks.
#[derive(Clone, PartialEq, Message)]
pub struct LiteralConfig {
    #[prost(string, tag = "1")]
    pub path: String,
    #[prost(int64, tag = "2")]
    pub big: i64,
    #[prost(double, tag = "3")]
    pub ratio: f64,
    #[prost(bool, tag = "4")]
    pub enabled: bool,
}

impl Name for LiteralConfig {
    const NAME: &'static str = "LiteralConfig";
    const PACKAGE: &'static str = "test";

    fn full_name() -> String {
        "test.LiteralConfig".to_string()
    }

    fn type_url() -> String {
        "type.googleapis.com/test.LiteralConfig".to_string()
    }
}

impl Explainable for LiteralConfig {
    fn name() -> &'static str {
        "LiteralTest"
    }

    fn from_args(args: &ExtensionArgs, _ctx: &ExplainContext) -> Result<Self, ExtensionError> {
        let mut extractor = args.extractor();
        let path: String = extractor.expect_named_arg::<&str>("path")?.to_string();
        let big: i64 = extractor.expect_named_arg("big")?;

        // Manually handle ratio to support both Integer and Float types
        let ratio = match extractor.get_named_arg("ratio") {
            Some(ExtensionValue::Float(f)) => *f,
            Some(ExtensionValue::Integer(i)) => *i as f64,
            Some(v) => {
                return Err(ExtensionError::InvalidArgument(format!(
                    "ratio must be a float, got {v}"
                )));
            }
            None => {
                return Err(ExtensionError::MissingArgument {
                    name: "ratio".to_string(),
                });
            }
        };

        let enabled: bool = extractor.expect_named_arg("enabled")?;

        extractor.check_exhausted()?;

        Ok(LiteralConfig {
            path,
            big,
            ratio,
            enabled,
        })
    }

    fn to_args(&self, _ctx: &ExplainContext) -> Result<ExtensionArgs, ExtensionError> {
        let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf);
        args.named.insert(
            "path".to_string(),
            ExtensionValue::String(self.path.clone()),
        );
        args.named
            .insert("big".to_string(), ExtensionValue::Integer(self.big));
        args.named
            .insert("ratio".to_string(), ExtensionValue::Float(self.ratio));
        args.named
            .insert("enabled".to_string(), ExtensionValue::Boolean(self.enabled));
        args.output_columns.push(ExtensionColumn::Named {
            name: "value".to_string(),
            r#type: parse_type("string"),
        });
        Ok(args)
    }
}

#[test]
fn test_extension_literal_roundtrip() {
    let mut registry = ExtensionRegistry::new();
    registry.register_relation::<LiteralConfig>().unwrap();

    let plan_text = r#"
=== Plan
Root[result]
  ExtensionLeaf:LiteralTest[path='data/source', big=1099511627776, ratio=3.25, enabled=false => value:string]
"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("Failed to parse plan");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "Unexpected errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

#[test]
fn test_extension_unknown_arguments() {
    let mut registry = ExtensionRegistry::new();
    registry.register_relation::<UserTableConfig>().unwrap();

    // Test plan with unknown argument 'invalid_arg'
    let plan_text = r#"
=== Plan
Root[result]
  ExtensionLeaf:UserTable[name='customers', version=2, invalid_arg=true => id:string]
"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let result = parser.parse_plan(plan_text);

    // Should fail during parsing when it tries to convert args
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        err.to_string()
            .contains("Unknown named arguments: invalid_arg")
    );
}

/// Extension with no arguments — tests empty args (`_`) roundtrip.
#[derive(Clone, PartialEq, Message)]
pub struct EmptySource {
    #[prost(string, tag = "1")]
    pub marker: String,
}

impl Name for EmptySource {
    const NAME: &'static str = "EmptySource";
    const PACKAGE: &'static str = "test";
    fn full_name() -> String {
        "test.EmptySource".to_string()
    }
    fn type_url() -> String {
        "type.googleapis.com/test.EmptySource".to_string()
    }
}

impl Explainable for EmptySource {
    fn name() -> &'static str {
        "EmptySource"
    }

    fn from_args(args: &ExtensionArgs, _ctx: &ExplainContext) -> Result<Self, ExtensionError> {
        let mut extractor = args.extractor();
        extractor.check_exhausted()?;
        Ok(EmptySource {
            marker: "empty".to_string(),
        })
    }

    fn to_args(&self, _ctx: &ExplainContext) -> Result<ExtensionArgs, ExtensionError> {
        Ok(ExtensionArgs::new(ExtensionRelationType::Leaf))
    }
}

#[test]
fn test_extension_empty_args_roundtrip() {
    let mut registry = ExtensionRegistry::new();
    registry.register_relation::<EmptySource>().unwrap();

    // No args, no output columns — textifies as [_]
    let plan_text = r#"
=== Plan
Root[result]
  ExtensionLeaf:EmptySource[_ => ]
"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser
        .parse_plan(plan_text)
        .expect("Failed to parse empty args extension");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "Unexpected errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

#[test]
fn test_extension_string_escaping_roundtrip() {
    let mut registry = ExtensionRegistry::new();
    registry.register_relation::<UserTableConfig>().unwrap();

    // String with escaped single quote and backslash
    let plan_text = r#"
=== Plan
Root[result]
  ExtensionLeaf:UserTable[name='it\'s a \"test\\path', version=1, temp=false => col:string]
"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser
        .parse_plan(plan_text)
        .expect("Failed to parse escaped string extension");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "Unexpected errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

// ---------------------------------------------------------------------------
// ExtensionSingle and ExtensionMulti fixtures
// ---------------------------------------------------------------------------

/// A minimal ExtensionSingle: no arguments, passes through its single input
/// column unchanged.  The `_ => $0` syntax uses the empty-args placeholder
/// and a reference-style output column.
#[derive(Clone, PartialEq, Message)]
pub struct PassThroughWrapper {}

impl Name for PassThroughWrapper {
    const NAME: &'static str = "PassThroughWrapper";
    const PACKAGE: &'static str = "test";
    fn full_name() -> String {
        "test.PassThroughWrapper".to_string()
    }
    fn type_url() -> String {
        "type.googleapis.com/test.PassThroughWrapper".to_string()
    }
}

impl Explainable for PassThroughWrapper {
    fn name() -> &'static str {
        "PassThrough"
    }

    fn from_args(args: &ExtensionArgs, _ctx: &ExplainContext) -> Result<Self, ExtensionError> {
        let mut extractor = args.extractor();
        extractor.check_exhausted()?;
        Ok(PassThroughWrapper {})
    }

    fn to_args(&self, _ctx: &ExplainContext) -> Result<ExtensionArgs, ExtensionError> {
        let mut args = ExtensionArgs::new(ExtensionRelationType::Single);
        args.output_columns.push(ExtensionColumn::Reference(0));
        Ok(args)
    }
}

/// A minimal ExtensionMulti: no arguments, takes two children and exposes one
/// reference-style output column from each (`_ => $0, $1`).
#[derive(Clone, PartialEq, Message)]
pub struct BinaryMerge {}

impl Name for BinaryMerge {
    const NAME: &'static str = "BinaryMerge";
    const PACKAGE: &'static str = "test";
    fn full_name() -> String {
        "test.BinaryMerge".to_string()
    }
    fn type_url() -> String {
        "type.googleapis.com/test.BinaryMerge".to_string()
    }
}

impl Explainable for BinaryMerge {
    fn name() -> &'static str {
        "BinaryMerge"
    }

    fn from_args(args: &ExtensionArgs, _ctx: &ExplainContext) -> Result<Self, ExtensionError> {
        let mut extractor = args.extractor();
        extractor.check_exhausted()?;
        Ok(BinaryMerge {})
    }

    fn to_args(&self, _ctx: &ExplainContext) -> Result<ExtensionArgs, ExtensionError> {
        let mut args = ExtensionArgs::new(ExtensionRelationType::Multi);
        args.output_columns.push(ExtensionColumn::Reference(0));
        args.output_columns.push(ExtensionColumn::Reference(1));
        Ok(args)
    }
}

// ---------------------------------------------------------------------------
// ExtensionSingle round-trip over a standard Read
// ---------------------------------------------------------------------------

#[test]
fn test_extension_single_over_read_roundtrip() {
    let mut registry = ExtensionRegistry::new();
    registry.register_relation::<PassThroughWrapper>().unwrap();

    let plan_text = r#"=== Plan
Root[col]
  ExtensionSingle:PassThrough[_ => $0]
    Read[my.table => col:i64]"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

// ---------------------------------------------------------------------------
// ExtensionMulti round-trip
// ---------------------------------------------------------------------------

#[test]
fn test_extension_multi_roundtrip() {
    let mut registry = ExtensionRegistry::new();
    registry.register_relation::<BinaryMerge>().unwrap();

    let plan_text = r#"=== Plan
Root[a, b]
  ExtensionMulti:BinaryMerge[_ => $0, $1]
    Read[left => a:i64]
    Read[right => b:i64]"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

// ---------------------------------------------------------------------------
// Enhancement on a standard relation nested under an ExtensionSingle
// ---------------------------------------------------------------------------

#[test]
fn test_enhancement_on_read_under_extension_single() {
    let mut registry = ExtensionRegistry::new();
    registry.register_relation::<PassThroughWrapper>().unwrap();
    registry
        .register_enhancement::<PartitionHint>()
        .expect("register_enhancement");

    let plan_text = r#"=== Plan
Root[col]
  ExtensionSingle:PassThrough[_ => $0]
    Read[my.table => col:i64]
      + Enh:PartitionHint[&HASH]"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

// ---------------------------------------------------------------------------
// Lenient textification for an ExtensionLeaf with an unknown type URL
// Symmetric to the enhancement leniency tests: if the type URL stored in the
// proto is not registered at textify time, the node renders with a failure
// token rather than panicking or omitting the line entirely.
// ---------------------------------------------------------------------------

#[test]
fn test_extension_leaf_unknown_type_url_textify_is_lenient() {
    let mut parse_registry = ExtensionRegistry::new();
    parse_registry
        .register_relation::<UserTableConfig>()
        .unwrap();

    let plan_text = r#"=== Plan
Root[result]
  ExtensionLeaf:UserTable[name='customers', version=2, temp=true => id:string, region:string]"#;

    let parser = Parser::new().with_extension_registry(parse_registry);
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    // Textify with an empty registry — the extension type URL is no longer known.
    let empty_registry = ExtensionRegistry::new();
    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &empty_registry);

    assert!(
        formatted.contains("Root["),
        "expected Root in output, got:\n{formatted}"
    );
    // The ExtensionLeaf line should contain a failure token.
    assert!(
        formatted.contains("ExtensionLeaf[!{"),
        "expected failure token in ExtensionLeaf output, got:\n{formatted}"
    );
    assert!(
        !errors.is_empty(),
        "expected at least one format error for unknown extension type URL"
    );
}
/// An enhancement that carries a single positional tuple of sort-direction enums.
#[derive(Clone, PartialEq, Message)]
pub struct TupleSortHint {
    /// Sort direction identifiers, e.g. ["ASC", "DESC"].
    #[prost(string, repeated, tag = "1")]
    pub directions: Vec<String>,
}

impl Name for TupleSortHint {
    const NAME: &'static str = "TupleSortHint";
    const PACKAGE: &'static str = "test";

    fn full_name() -> String {
        "test.TupleSortHint".to_string()
    }

    fn type_url() -> String {
        "type.googleapis.com/test.TupleSortHint".to_string()
    }
}

impl Explainable for TupleSortHint {
    fn name() -> &'static str {
        "TupleSortHint"
    }

    fn from_args(args: &ExtensionArgs, _ctx: &ExplainContext) -> Result<Self, ExtensionError> {
        if args.positional.len() != 1 {
            return Err(ExtensionError::InvalidArgument(format!(
                "expected 1 positional tuple arg, got {}",
                args.positional.len()
            )));
        }
        let tv = <&TupleValue>::try_from(&args.positional[0])?;
        let directions = tv
            .iter()
            .map(|v| {
                let EnumValue(s) = EnumValue::try_from(v)?;
                Ok(s)
            })
            .collect::<Result<Vec<_>, ExtensionError>>()?;
        let mut extractor = args.extractor();
        extractor.check_exhausted()?;
        Ok(TupleSortHint { directions })
    }

    fn to_args(&self, _ctx: &ExplainContext) -> Result<ExtensionArgs, ExtensionError> {
        let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf);
        let tv: TupleValue = self
            .directions
            .iter()
            .map(|d| ExtensionValue::Enum(d.clone()))
            .collect();
        args.positional.push(ExtensionValue::Tuple(tv));
        Ok(args)
    }
}

#[test]
fn test_tuple_sort_hint_roundtrip() {
    let mut registry = ExtensionRegistry::new();
    registry
        .register_enhancement::<TupleSortHint>()
        .expect("register_enhancement");

    let plan_text = r#"=== Plan
Root[result]
  Read[my.table => col:i64]
    + Enh:TupleSortHint[(&ASC, &DESC)]"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

#[test]
fn test_tuple_sort_hint_empty_tuple_roundtrip() {
    let mut registry = ExtensionRegistry::new();
    registry
        .register_enhancement::<TupleSortHint>()
        .expect("register_enhancement");

    let plan_text = r#"=== Plan
Root[result]
  Read[my.table => col:i64]
    + Enh:TupleSortHint[()]"#;

    let parser = Parser::new().with_extension_registry(registry.clone());
    let plan = parser.parse_plan(plan_text).expect("parse failed");

    let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
    assert!(errors.is_empty(), "unexpected format errors: {errors:?}");
    assert_eq!(formatted.trim(), plan_text.trim());
}

#[test]
fn test_tuple_sort_hint_from_args_rejects_non_tuple() {
    let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf);
    args.positional
        .push(ExtensionValue::Enum("ASC".to_string()));
    let ext = SimpleExtensions::default();
    let result = TupleSortHint::from_args(&args, &ExplainContext::new(&ext));
    assert!(
        result.is_err(),
        "expected error when positional arg is not a tuple"
    );
}

// ---------------------------------------------------------------------------
// Tests for ExplainContext helpers
// ---------------------------------------------------------------------------

#[test]
fn test_try_from_expr() {
    // Construct a simple literal expression
    let expr = proto::Expression {
        rex_type: Some(RexType::Literal(proto::expression::Literal {
            nullable: false,
            type_variation_reference: 0,
            literal_type: Some(LiteralType::I64(42)),
        })),
    };
    let val = ExtensionValue::Expression(Expr(Box::new(expr.clone())));
    let extracted: Expr = Expr::try_from(&val).unwrap();
    assert_eq!(*extracted.0, expr);

    let wrong = ExtensionValue::Integer(42);
    assert!(Expr::try_from(&wrong).is_err());
}

#[test]
fn test_explain_context_schema_columns_roundtrip() {
    let ext = SimpleExtensions::default();
    let ctx = ExplainContext::new(&ext);

    let columns = vec![
        ExtensionColumn::Named {
            name: "id".to_string(),
            r#type: parse_type("i64"),
        },
        ExtensionColumn::Named {
            name: "name".to_string(),
            r#type: parse_type("string?"),
        },
    ];

    let schema = ctx.schema(&columns).unwrap();
    assert_eq!(schema.names, vec!["id", "name"]);

    let roundtripped = ctx.columns(&schema).unwrap();
    assert_eq!(roundtripped.len(), 2);
    match &roundtripped[0] {
        ExtensionColumn::Named { name, r#type: ty } => {
            assert_eq!(name, "id");
            assert_eq!(*ty, parse_type("i64"));
        }
        other => panic!("Expected Named, got {other:?}"),
    }
}

#[test]
fn test_explain_context_type_string_roundtrip() {
    let ext = SimpleExtensions::default();
    let ctx = ExplainContext::new(&ext);

    let ty = ctx.string_to_type("timestamp_tz?").unwrap();
    assert_eq!(ctx.type_to_string(&ty).unwrap(), "timestamp_tz?");
}

#[test]
fn test_explain_context_schema_rejects_references() {
    let ext = SimpleExtensions::default();
    let ctx = ExplainContext::new(&ext);

    let columns = vec![ExtensionColumn::Reference(0)];
    assert!(ctx.schema(&columns).is_err());
}

#[test]
fn test_explain_context_columns_rejects_mismatch() {
    let ext = SimpleExtensions::default();
    let ctx = ExplainContext::new(&ext);

    // 2 names but 1 type
    let schema = proto::NamedStruct {
        names: vec!["a".to_string(), "b".to_string()],
        r#struct: Some(proto::r#type::Struct {
            types: vec![parse_type("i64")],
            type_variation_reference: 0,
            nullability: Nullability::Required as i32,
        }),
    };
    assert!(ctx.columns(&schema).is_err());
}
