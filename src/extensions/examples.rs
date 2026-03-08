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

use crate::extensions::args::{EnumValue, ExtensionArgs, ExtensionRelationType, ExtensionValue};
use crate::extensions::registry::{Explainable, ExtensionError};

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
/// # use substrait_explain::extensions::{ExtensionRegistry, examples::PartitionHint};
/// # use substrait_explain::format_with_registry;
/// # use substrait_explain::parser::Parser;
/// #
/// # let mut registry = ExtensionRegistry::new();
/// # registry.register_enhancement::<PartitionHint>().unwrap();
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
        let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf);
        for &raw in &self.strategies {
            let s = PartitionStrategy::try_from(raw).unwrap_or(PartitionStrategy::Unspecified);
            args.positional
                .push(ExtensionValue::Enum(s.as_str_name().to_owned()));
        }
        if self.count != 0 {
            args.named
                .insert("count".to_owned(), ExtensionValue::Integer(self.count));
        }
        Ok(args)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::extensions::{AnyConvertible, ExtensionRegistry};

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
        assert!(matches!(
            args.named.get("count"),
            Some(ExtensionValue::Integer(8))
        ));
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
        let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf);
        args.positional
            .push(ExtensionValue::Enum("BOGUS".to_owned()));
        assert!(PartitionHint::from_args(&args).is_err());
    }

    #[test]
    fn registry_roundtrip() {
        let mut registry = ExtensionRegistry::new();
        registry.register_enhancement::<PartitionHint>().unwrap();

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
}
