//! [`PartitionHint`] demonstrates how to implement a custom enhancement:
//! positional enum arguments combined with an optional named integer argument.
//!
//! # Text Format
//!
//! ```text
//! Read[data => col:i64]
//!   + Enh:PartitionHint[&HASH, count=8]
//! ```
//!
//! Each positional argument is a [`PartitionStrategy`] variant rendered with
//! the `&` enum prefix.  The optional named argument `count` gives the target
//! number of partitions (`0` / absent means "let the executor decide").
//!
//! This hidden module is crate-owned example support for doctests and
//! integration tests, not a stable extension API.

use crate::extensions::args::{EnumValue, ExtensionArgs, ExtensionValue};
use crate::extensions::registry::{Explainable, ExtensionError, ExtensionRegistry};

// ---------------------------------------------------------------------------
// PartitionStrategy enum
// ---------------------------------------------------------------------------

/// Partitioning strategy for a [`PartitionHint`] enhancement.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, prost::Enumeration)]
#[repr(i32)]
pub enum PartitionStrategy {
    /// No strategy specified.
    Unspecified = 0,
    /// Distribute rows by hashing one or more key columns.
    Hash = 1,
    /// Sort-based range partitioning.
    Range = 2,
    /// Broadcast the entire relation to every partition.
    Broadcast = 3,
}

impl PartitionStrategy {
    /// The identifier used in the text format (without the leading `&`).
    pub fn as_str_name(self) -> &'static str {
        match self {
            PartitionStrategy::Unspecified => "UNSPECIFIED",
            PartitionStrategy::Hash => "HASH",
            PartitionStrategy::Range => "RANGE",
            PartitionStrategy::Broadcast => "BROADCAST",
        }
    }

    /// Parse from the text-format identifier (without the leading `&`).
    pub fn from_str_name(s: &str) -> Option<Self> {
        match s {
            "UNSPECIFIED" => Some(PartitionStrategy::Unspecified),
            "HASH" => Some(PartitionStrategy::Hash),
            "RANGE" => Some(PartitionStrategy::Range),
            "BROADCAST" => Some(PartitionStrategy::Broadcast),
            _ => None,
        }
    }
}

// ---------------------------------------------------------------------------
// PartitionHint
// ---------------------------------------------------------------------------

/// Enhancement that hints the executor how to partition a relation's output.
///
/// Attach this to any standard relation via `register_enhancement` to convey
/// partitioning decisions made during planning.
///
/// # Text Format
///
/// ```rust
/// # use substrait_explain::extensions::examples;
/// # use substrait_explain::format_with_registry;
/// # use substrait_explain::Parser;
/// #
/// # let registry = examples::registry();
/// # let parser = Parser::new().with_extension_registry(registry.clone());
/// #
/// # let plan_text = r#"
/// === Plan
/// Root[result]
///   Read[data => col:i64]
///     + Enh:PartitionHint[&HASH, count=8]
/// # "#;
/// #
/// # let plan = parser.parse_plan(plan_text).unwrap();
/// # let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
/// # assert!(errors.is_empty());
/// # assert_eq!(formatted.trim(), plan_text.trim());
/// ```
#[derive(Clone, PartialEq, prost::Message)]
pub struct PartitionHint {
    /// The strategies to apply, in order of preference.  Each value is the
    /// integer representation of [`PartitionStrategy`].
    #[prost(enumeration = "PartitionStrategy", repeated, tag = "1")]
    pub strategies: Vec<i32>,
    /// Target number of partitions.  `0` means "let the executor decide".
    #[prost(int64, tag = "2")]
    pub count: i64,
}

impl prost::Name for PartitionHint {
    const PACKAGE: &'static str = "example";
    const NAME: &'static str = "PartitionHint";

    fn full_name() -> String {
        "example.PartitionHint".to_owned()
    }

    fn type_url() -> String {
        "type.googleapis.com/example.PartitionHint".to_owned()
    }
}

impl Explainable for PartitionHint {
    fn name() -> &'static str {
        "PartitionHint"
    }

    fn from_args(args: &ExtensionArgs) -> Result<Self, ExtensionError> {
        // Positional arguments are PartitionStrategy enum values.
        let strategies: Result<Vec<i32>, ExtensionError> = args
            .positional
            .iter()
            .map(|val| {
                let EnumValue(ident) = EnumValue::try_from(val)?;
                PartitionStrategy::from_str_name(&ident)
                    .map(|s| s as i32)
                    .ok_or_else(|| {
                        ExtensionError::InvalidArgument(format!(
                            "Unknown PartitionStrategy variant '&{ident}'; \
                             expected one of &UNSPECIFIED, &HASH, &RANGE, &BROADCAST"
                        ))
                    })
            })
            .collect();

        let mut extractor = args.extractor();
        let count: i64 = extractor.get_named_or("count", 0)?;
        extractor.check_exhausted()?;

        Ok(PartitionHint {
            strategies: strategies?,
            count,
        })
    }

    fn to_args(&self) -> Result<ExtensionArgs, ExtensionError> {
        let mut args = ExtensionArgs::default();
        for &raw in &self.strategies {
            let s = PartitionStrategy::try_from(raw).unwrap_or(PartitionStrategy::Unspecified);
            args.positional
                .push(ExtensionValue::Enum(s.as_str_name().to_owned()));
        }
        if self.count != 0 {
            args.insert("count", self.count);
        }
        Ok(args)
    }
}

// ---------------------------------------------------------------------------
// PlanHint
// ---------------------------------------------------------------------------

/// Optimization hint that carries a planner directive as a string.
///
/// Attach this to any standard relation via `register_optimization` to convey
/// planner choices without changing relation semantics.
///
/// # Text Format
///
/// ```rust
/// # use substrait_explain::extensions::examples;
/// # use substrait_explain::format_with_registry;
/// # use substrait_explain::Parser;
/// #
/// # let registry = examples::registry();
/// # let parser = Parser::new().with_extension_registry(registry.clone());
/// #
/// # let plan_text = r#"
/// === Plan
/// Root[result]
///   Read[data => col:i64]
///     + Opt:PlanHint[hint='use_index']
/// # "#;
/// #
/// # let plan = parser.parse_plan(plan_text).unwrap();
/// # let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
/// # assert!(errors.is_empty());
/// # assert_eq!(formatted.trim(), plan_text.trim());
/// ```
#[derive(Clone, PartialEq, prost::Message)]
pub struct PlanHint {
    /// Planner directive. The text format stores this as `hint='...'`.
    #[prost(string, tag = "1")]
    pub hint: String,
}

impl prost::Name for PlanHint {
    const PACKAGE: &'static str = "example";
    const NAME: &'static str = "PlanHint";

    fn full_name() -> String {
        "example.PlanHint".to_owned()
    }

    fn type_url() -> String {
        "type.googleapis.com/example.PlanHint".to_owned()
    }
}

impl Explainable for PlanHint {
    fn name() -> &'static str {
        "PlanHint"
    }

    fn from_args(args: &ExtensionArgs) -> Result<Self, ExtensionError> {
        if !args.positional.is_empty() {
            return Err(ExtensionError::InvalidArgument(
                "PlanHint does not accept positional arguments".to_owned(),
            ));
        }

        let mut extractor = args.extractor();
        let hint: String = extractor.expect_named_arg::<&str>("hint")?.to_owned();
        extractor.check_exhausted()?;
        Ok(PlanHint { hint })
    }

    fn to_args(&self) -> Result<ExtensionArgs, ExtensionError> {
        let mut args = ExtensionArgs::default();
        args.named
            .insert("hint".to_owned(), ExtensionValue::String(self.hint.clone()));
        Ok(args)
    }
}

// ---------------------------------------------------------------------------
// BlobStoreRead
// ---------------------------------------------------------------------------

/// ExtensionTable detail for a simple blob-store backed read.
///
/// Attach this to `Read:Extension[...]` via `register_extension_table` to
/// describe a custom table source whose output schema is carried by the
/// surrounding `ReadRel.base_schema`.
///
/// # Text Format
///
/// ```rust
/// # use substrait_explain::extensions::examples;
/// # use substrait_explain::format_with_registry;
/// # use substrait_explain::Parser;
/// #
/// # let registry = examples::registry();
/// # let parser = Parser::new().with_extension_registry(registry.clone());
/// #
/// # let plan_text = r#"
/// === Plan
/// Root[id, payload]
///   Read:Extension[id:i64, payload:string]
///     + Ext:BlobStoreRead['path/to/file', limit=100]
/// # "#;
/// #
/// # let plan = parser.parse_plan(plan_text).unwrap();
/// # let (formatted, errors) = format_with_registry(&plan, &Default::default(), &registry);
/// # assert!(errors.is_empty());
/// # assert_eq!(formatted.trim(), plan_text.trim());
/// ```
#[derive(Clone, PartialEq, prost::Message)]
pub struct BlobStoreRead {
    /// Blob path or URI to read.
    #[prost(string, tag = "1")]
    pub path: String,
    /// Optional row limit. `0` means no limit.
    #[prost(int64, tag = "2")]
    pub limit: i64,
    /// Whether archived blobs should be included.
    #[prost(bool, tag = "3")]
    pub include_archived: bool,
}

impl prost::Name for BlobStoreRead {
    const PACKAGE: &'static str = "example";
    const NAME: &'static str = "BlobStoreRead";

    fn full_name() -> String {
        "example.BlobStoreRead".to_owned()
    }

    fn type_url() -> String {
        "type.googleapis.com/example.BlobStoreRead".to_owned()
    }
}

impl Explainable for BlobStoreRead {
    fn name() -> &'static str {
        "BlobStoreRead"
    }

    fn from_args(args: &ExtensionArgs) -> Result<Self, ExtensionError> {
        if args.positional.len() != 1 {
            return Err(ExtensionError::InvalidArgument(format!(
                "BlobStoreRead expects exactly 1 positional path argument, got {}",
                args.positional.len()
            )));
        }
        if !args.output_columns.is_empty() {
            return Err(ExtensionError::InvalidArgument(
                "BlobStoreRead output columns belong in Read:Extension[...]".to_owned(),
            ));
        }

        let path = <&str>::try_from(&args.positional[0])?.to_owned();
        let mut extractor = args.extractor();
        let limit: i64 = extractor.get_named_or("limit", 0)?;
        let include_archived: bool = extractor.get_named_or("include_archived", false)?;
        extractor.check_exhausted()?;

        Ok(Self {
            path,
            limit,
            include_archived,
        })
    }

    fn to_args(&self) -> Result<ExtensionArgs, ExtensionError> {
        let mut args = ExtensionArgs::default();
        args.positional
            .push(ExtensionValue::String(self.path.clone()));
        if self.limit != 0 {
            args.named
                .insert("limit".to_owned(), ExtensionValue::Integer(self.limit));
        }
        if self.include_archived {
            args.named
                .insert("include_archived".to_owned(), ExtensionValue::Boolean(true));
        }
        Ok(args)
    }
}

/// Create an [`ExtensionRegistry`] preloaded with the example extension types.
pub fn registry() -> ExtensionRegistry {
    let mut registry = ExtensionRegistry::new();
    registry
        .register_enhancement::<PartitionHint>()
        .expect("register PartitionHint example enhancement");
    registry
        .register_optimization::<PlanHint>()
        .expect("register PlanHint example optimization");
    registry
        .register_extension_table::<BlobStoreRead>()
        .expect("register BlobStoreRead example extension table");
    registry
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::extensions::AnyConvertible;

    fn make_hint(strategies: Vec<PartitionStrategy>, count: i64) -> PartitionHint {
        PartitionHint {
            strategies: strategies.into_iter().map(|s| s as i32).collect(),
            count,
        }
    }

    #[test]
    fn round_trip_via_any() {
        let original = make_hint(vec![PartitionStrategy::Hash, PartitionStrategy::Range], 4);
        let any = original.to_any().expect("encode");
        let decoded = PartitionHint::from_any(any.as_ref()).expect("decode");
        assert_eq!(original, decoded);
    }

    #[test]
    fn to_args_produces_enum_and_named() {
        let hint = make_hint(vec![PartitionStrategy::Hash], 8);
        let args = hint.to_args().unwrap();
        assert_eq!(args.positional.len(), 1);
        assert!(matches!(&args.positional[0], ExtensionValue::Enum(s) if s == "HASH"));
        let count = args.named.get("count").expect("count arg");
        assert_eq!(i64::try_from(count).unwrap(), 8);
    }

    #[test]
    fn to_args_omits_zero_count() {
        let hint = make_hint(vec![PartitionStrategy::Broadcast], 0);
        let args = hint.to_args().unwrap();
        assert!(args.named.is_empty(), "count=0 should be omitted");
    }

    #[test]
    fn from_args_round_trip() {
        let original = make_hint(vec![PartitionStrategy::Hash, PartitionStrategy::Range], 16);
        let args = original.to_args().unwrap();
        let decoded = PartitionHint::from_args(&args).unwrap();
        assert_eq!(original, decoded);
    }

    #[test]
    fn from_args_rejects_unknown_strategy() {
        let mut args = ExtensionArgs::default();
        args.positional
            .push(ExtensionValue::Enum("BOGUS".to_owned()));
        assert!(PartitionHint::from_args(&args).is_err());
    }

    #[test]
    fn from_args_rejects_non_enum_positional() {
        // An integer positional arg where an enum is expected should fail.
        let mut args = ExtensionArgs::default();
        args.push(1_i64);
        let result = PartitionHint::from_args(&args);
        assert!(
            result.is_err(),
            "expected error for non-enum positional arg, got {result:?}"
        );
    }

    #[test]
    fn from_args_rejects_extra_named_args() {
        // check_exhausted should reject unknown named args.
        let mut args = ExtensionArgs::default();
        args.insert("unknown_key", 99_i64);
        let result = PartitionHint::from_args(&args);
        assert!(
            result.is_err(),
            "expected error for unknown named arg, got {result:?}"
        );
    }

    #[test]
    fn from_args_empty_strategies_roundtrip() {
        let original = make_hint(vec![], 0);
        let args = original.to_args().unwrap();
        let decoded = PartitionHint::from_args(&args).unwrap();
        assert_eq!(original, decoded);
        assert!(decoded.strategies.is_empty());
        assert_eq!(decoded.count, 0);
    }

    #[test]
    fn registry_roundtrip() {
        let registry = registry();

        let original = make_hint(vec![PartitionStrategy::Hash], 4);
        let any = original.to_any().unwrap();

        let (name, args) = registry
            .decode_enhancement(any.as_ref())
            .expect("decode_enhancement");
        assert_eq!(name, "PartitionHint");
        assert_eq!(args.positional.len(), 1);

        let any2 = registry
            .parse_enhancement("PartitionHint", &args)
            .expect("parse_enhancement");
        let decoded = PartitionHint::from_any(any2.as_ref()).unwrap();
        assert_eq!(original, decoded);
    }

    #[test]
    fn plan_hint_args_round_trip() {
        let original = PlanHint {
            hint: "use_index".to_owned(),
        };
        let args = original.to_args().unwrap();
        let decoded = PlanHint::from_args(&args).unwrap();
        assert_eq!(original, decoded);
    }

    #[test]
    fn plan_hint_registry_roundtrip() {
        let registry = registry();

        let original = PlanHint {
            hint: "parallel".to_owned(),
        };
        let any = original.to_any().unwrap();

        let (name, args) = registry
            .decode_optimization(any.as_ref())
            .expect("decode_optimization");
        assert_eq!(name, "PlanHint");

        let any2 = registry
            .parse_optimization("PlanHint", &args)
            .expect("parse_optimization");
        let decoded = PlanHint::from_any(any2.as_ref()).unwrap();
        assert_eq!(original, decoded);
    }

    #[test]
    fn blob_store_read_args_round_trip() {
        let original = BlobStoreRead {
            path: "path/to/file".to_owned(),
            limit: 100,
            include_archived: true,
        };

        let args = original.to_args().unwrap();
        let decoded = BlobStoreRead::from_args(&args).unwrap();

        assert_eq!(original, decoded);
    }

    #[test]
    fn blob_store_read_registry_roundtrip() {
        let registry = registry();

        let original = BlobStoreRead {
            path: "path/to/file".to_owned(),
            limit: 100,
            include_archived: true,
        };
        let any = original.to_any().unwrap();

        let (name, args) = registry
            .decode_extension_table(any.as_ref())
            .expect("decode_extension_table");
        assert_eq!(name, "BlobStoreRead");

        let any2 = registry
            .parse_extension_table("BlobStoreRead", &args)
            .expect("parse_extension_table");
        let decoded = BlobStoreRead::from_any(any2.as_ref()).unwrap();
        assert_eq!(original, decoded);
    }
}
