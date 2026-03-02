//! Concrete enhancement types for Substrait advanced extensions.
//!
//! This module provides [`ReadRelEnhancement`] — the enhancement type that
//! corresponds to `ddsqlizer.ReadRelEnhancement` — along with the
//! [`Namespace`] enum it carries.
//!
//! # Text Format
//!
//! ```text
//! Read[my.table => col:i64]
//!   + Enh:ReadRelEnhancement[&CORE, &CUSTOM]
//! ```
//!
//! Each positional argument is a [`Namespace`] variant rendered with the `&`
//! enum prefix.  The order of arguments matches the order of namespaces stored
//! in the proto message.

use crate::extensions::args::{EnumValue, ExtensionArgs, ExtensionRelationType, ExtensionValue};
use crate::extensions::registry::{Explainable, ExtensionError};

// ---------------------------------------------------------------------------
// Namespace enum
// ---------------------------------------------------------------------------

/// Namespace for a [`ReadRelEnhancement`].
///
/// Mirrors the `Namespace` enum defined in `ddsqlizer.proto`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, prost::Enumeration)]
#[repr(i32)]
pub enum Namespace {
    Unknown = 0,
    Core = 1,
    Custom = 2,
    Tag = 3,
}

impl Namespace {
    /// The identifier used in the text format (without the leading `&`).
    pub fn as_str_name(self) -> &'static str {
        match self {
            Namespace::Unknown => "UNKNOWN",
            Namespace::Core => "CORE",
            Namespace::Custom => "CUSTOM",
            Namespace::Tag => "TAG",
        }
    }

    /// Parse from the text-format identifier (without the leading `&`).
    pub fn from_str_name(s: &str) -> Option<Self> {
        match s {
            "UNKNOWN" => Some(Namespace::Unknown),
            "CORE" => Some(Namespace::Core),
            "CUSTOM" => Some(Namespace::Custom),
            "TAG" => Some(Namespace::Tag),
            _ => None,
        }
    }
}

// ---------------------------------------------------------------------------
// ReadRelEnhancement
// ---------------------------------------------------------------------------

/// Enhancement attached to a `ReadRel` via `AdvancedExtension.enhancement`.
///
/// Corresponds to `ddsqlizer.ReadRelEnhancement`.  Only the `namespaces`
/// field (tag 100) is represented in the text format; the inner `oneof
/// enhancement` variants are handled separately by the downstream crate.
#[derive(Clone, PartialEq, prost::Message)]
pub struct ReadRelEnhancement {
    /// The list of namespaces that apply to this read.  Each value is the
    /// integer representation of the [`Namespace`] enum (tag 100 in the
    /// proto).
    #[prost(enumeration = "Namespace", repeated, tag = "100")]
    pub namespaces: Vec<i32>,
}

impl prost::Name for ReadRelEnhancement {
    const PACKAGE: &'static str = "ddsqlizer";
    const NAME: &'static str = "ReadRelEnhancement";

    fn full_name() -> String {
        "ddsqlizer.ReadRelEnhancement".to_owned()
    }

    fn type_url() -> String {
        "type.googleapis.com/ddsqlizer.ReadRelEnhancement".to_owned()
    }
}

// `AnyConvertible` is covered by the blanket impl for `prost::Message + prost::Name + Default`.

impl Explainable for ReadRelEnhancement {
    fn name() -> &'static str {
        "ReadRelEnhancement"
    }

    fn from_args(args: &ExtensionArgs) -> Result<Self, ExtensionError> {
        let namespaces: Result<Vec<i32>, ExtensionError> = args
            .positional
            .iter()
            .map(|val| {
                let EnumValue(ident) = EnumValue::try_from(val)?;
                Namespace::from_str_name(&ident)
                    .map(|ns| ns as i32)
                    .ok_or_else(|| {
                        ExtensionError::InvalidArgument(format!(
                            "Unknown Namespace variant '&{ident}'; \
                         expected one of &UNKNOWN, &CORE, &CUSTOM, &TAG"
                        ))
                    })
            })
            .collect();

        if !args.named.is_empty() {
            return Err(ExtensionError::InvalidArgument(
                "ReadRelEnhancement does not accept named arguments".to_owned(),
            ));
        }

        Ok(ReadRelEnhancement {
            namespaces: namespaces?,
        })
    }

    fn to_args(&self) -> Result<ExtensionArgs, ExtensionError> {
        let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf);
        for &raw in &self.namespaces {
            let ns = Namespace::try_from(raw).unwrap_or(Namespace::Unknown);
            args.positional
                .push(ExtensionValue::Enum(ns.as_str_name().to_owned()));
        }
        Ok(args)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::extensions::{AnyConvertible, ExtensionRegistry};

    fn make_enhancement(namespaces: Vec<Namespace>) -> ReadRelEnhancement {
        ReadRelEnhancement {
            namespaces: namespaces.into_iter().map(|ns| ns as i32).collect(),
        }
    }

    #[test]
    fn round_trip_via_any() {
        let original = make_enhancement(vec![Namespace::Core, Namespace::Custom]);
        let any = original.to_any().expect("encode");
        let decoded = ReadRelEnhancement::from_any(any.as_ref()).expect("decode");
        assert_eq!(original, decoded);
    }

    #[test]
    fn to_args_produces_enum_values() {
        let enh = make_enhancement(vec![Namespace::Core, Namespace::Tag]);
        let args = enh.to_args().unwrap();
        assert_eq!(args.positional.len(), 2);
        assert!(matches!(&args.positional[0], ExtensionValue::Enum(s) if s == "CORE"));
        assert!(matches!(&args.positional[1], ExtensionValue::Enum(s) if s == "TAG"));
    }

    #[test]
    fn from_args_parses_enum_values() {
        let enh = make_enhancement(vec![Namespace::Core, Namespace::Custom]);
        let args = enh.to_args().unwrap();
        let decoded = ReadRelEnhancement::from_args(&args).unwrap();
        assert_eq!(enh, decoded);
    }

    #[test]
    fn from_args_rejects_unknown_variant() {
        let mut args = ExtensionArgs::new(ExtensionRelationType::Leaf);
        args.positional
            .push(ExtensionValue::Enum("BOGUS".to_owned()));
        assert!(ReadRelEnhancement::from_args(&args).is_err());
    }

    #[test]
    fn registry_roundtrip() {
        let mut registry = ExtensionRegistry::new();
        registry
            .register_enhancement::<ReadRelEnhancement>()
            .unwrap();

        let original = make_enhancement(vec![Namespace::Core, Namespace::Custom]);
        let any = original.to_any().unwrap();

        // decode_enhancement should recover name + args
        let (name, args) = registry
            .decode_enhancement(any.as_ref())
            .expect("decode_enhancement");
        assert_eq!(name, "ReadRelEnhancement");
        assert_eq!(args.positional.len(), 2);

        // parse_enhancement should rebuild the Any
        let any2 = registry
            .parse_enhancement("ReadRelEnhancement", &args)
            .expect("parse_enhancement");
        let decoded = ReadRelEnhancement::from_any(any2.as_ref()).unwrap();
        assert_eq!(original, decoded);
    }
}
