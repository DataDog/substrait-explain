//! Shared conversion and lowering infrastructure for parser internals.
//!
//! This module provides:
//! - [`ParseCtx`]: shared context passed to lowerers
//! - [`Lower`] / [`LowerWith`]: common lowering traits for typed nodes
//! - typed wrapper values like [`Name`], [`FieldIndex`], [`UrnAnchor`], and
//!   [`Anchor`]
//! - helper utilities for consistent list parsing patterns

use std::convert::TryFrom;

use pest_typed::Spanned;
use substrait::proto::expression::field_reference::ReferenceType;
use substrait::proto::expression::{FieldReference, ReferenceSegment, reference_segment};

use crate::extensions::SimpleExtensions;
use crate::parser::common::{MessageParseError, Rule, rules, unescape_string};

#[derive(Debug, Clone)]
pub(crate) struct ParseCtx<'a> {
    /// Extension declarations available while lowering typed parse nodes.
    pub extensions: &'a SimpleExtensions,
}

/// Lower a parsed typed node into a semantic/protobuf value.
pub(crate) trait Lower {
    type Output;

    fn lower(&self, cx: &ParseCtx<'_>) -> Result<Self::Output, MessageParseError>;
}

/// Lower a parsed typed node with additional caller-provided input.
pub(crate) trait LowerWith<I> {
    type Output;

    fn lower_with(&self, cx: &ParseCtx<'_>, input: I) -> Result<Self::Output, MessageParseError>;
}

/// Collect values exposed by `(first, rest)` grammar accessors into one `Vec`.
pub(crate) fn collect_first_rest<T, I>(first: T, rest: I) -> Vec<T>
where
    I: IntoIterator<Item = T>,
{
    let mut values = vec![first];
    values.extend(rest);
    values
}

/// Parsed identifier or quoted name with escaping already resolved.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name(pub String);

impl From<&rules::name<'_>> for Name {
    fn from(node: &rules::name<'_>) -> Self {
        if let Some(identifier) = node.identifier() {
            return Self(identifier.span.as_str().to_string());
        }

        let quoted = node
            .quoted_name()
            .expect("name must be identifier or quoted_name");
        Self(unescape_string(quoted.span.as_str(), '"'))
    }
}

/// Zero-based field index from `$<n>` references.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FieldIndex(pub i32);

impl TryFrom<&rules::reference<'_>> for FieldIndex {
    type Error = MessageParseError;

    fn try_from(node: &rules::reference<'_>) -> Result<Self, Self::Error> {
        let value = node
            .integer()
            .span
            .as_str()
            .parse::<i32>()
            .map_err(|error| {
                MessageParseError::invalid(
                    "FieldIndex",
                    node.span(),
                    format!("Invalid reference index: {error}"),
                )
            })?;
        Ok(Self(value))
    }
}

impl From<FieldIndex> for FieldReference {
    fn from(value: FieldIndex) -> Self {
        FieldReference {
            reference_type: Some(ReferenceType::DirectReference(ReferenceSegment {
                reference_type: Some(reference_segment::ReferenceType::StructField(Box::new(
                    reference_segment::StructField {
                        field: value.0,
                        child: None,
                    },
                ))),
            })),
            root_type: None,
        }
    }
}

impl TryFrom<&rules::reference<'_>> for FieldReference {
    type Error = MessageParseError;

    fn try_from(node: &rules::reference<'_>) -> Result<Self, Self::Error> {
        Ok(FieldIndex::try_from(node)?.into())
    }
}

/// Parsed URN anchor (`@ <n>`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UrnAnchor(pub u32);

/// Semantic context for `# <n>` anchors.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AnchorKind {
    Function,
    Type,
    TypeVariation,
    Extension,
}

impl AnchorKind {
    fn parse_message(self) -> &'static str {
        match self {
            AnchorKind::Function => "FunctionAnchor",
            AnchorKind::Type => "TypeAnchor",
            AnchorKind::TypeVariation => "TypeVariationAnchor",
            AnchorKind::Extension => "ExtensionAnchor",
        }
    }
}

/// Parsed `# <n>` anchor tagged with semantic kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Anchor {
    value: u32,
    kind: AnchorKind,
}

impl Anchor {
    pub fn parse(node: &rules::anchor<'_>, kind: AnchorKind) -> Result<Self, MessageParseError> {
        let integer = node.integer();
        Ok(Self {
            value: integer.span.as_str().parse::<u32>().map_err(|error| {
                MessageParseError::invalid(
                    kind.parse_message(),
                    integer.span(),
                    format!(
                        "Invalid {:?} value '{}': {error}",
                        Rule::anchor,
                        integer.span.as_str()
                    ),
                )
            })?,
            kind,
        })
    }

    pub fn kind(self) -> AnchorKind {
        self.kind
    }
}

impl TryFrom<&rules::urn_anchor<'_>> for UrnAnchor {
    type Error = MessageParseError;

    fn try_from(value: &rules::urn_anchor<'_>) -> Result<Self, Self::Error> {
        let integer = value.integer();
        Ok(Self(integer.span.as_str().parse::<u32>().map_err(
            |error| {
                MessageParseError::invalid(
                    "UrnAnchor",
                    integer.span(),
                    format!(
                        "Invalid {:?} value '{}': {error}",
                        Rule::urn_anchor,
                        integer.span.as_str()
                    ),
                )
            },
        )?))
    }
}

impl From<UrnAnchor> for u32 {
    fn from(value: UrnAnchor) -> Self {
        value.0
    }
}

impl From<Anchor> for u32 {
    fn from(value: Anchor) -> Self {
        value.value
    }
}

/// Output mapping index used for relation `emit` handling.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RelationOutputIndex(pub i32);

impl From<FieldIndex> for RelationOutputIndex {
    fn from(value: FieldIndex) -> Self {
        Self(value.0)
    }
}

impl From<RelationOutputIndex> for i32 {
    fn from(value: RelationOutputIndex) -> Self {
        value.0
    }
}

/// Parsed table path from dotted table names (`a.b.c`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TablePath(pub Vec<Name>);

impl From<&rules::table_name<'_>> for TablePath {
    fn from(node: &rules::table_name<'_>) -> Self {
        let (first, rest) = node.name();
        let mut names = Vec::with_capacity(rest.len() + 1);
        names.push(Name::from(first));
        names.extend(rest.into_iter().map(Name::from));
        Self(names)
    }
}

#[cfg(test)]
mod tests {
    use substrait::proto::expression::field_reference::ReferenceType;
    use substrait::proto::expression::reference_segment::ReferenceType as SegmentReferenceType;

    use super::*;
    use crate::parser::common::parse_typed;

    #[test]
    fn field_index_from_reference_and_into_field_reference() {
        let reference = parse_typed::<rules::reference<'_>>("$12", "reference").unwrap();
        let index = FieldIndex::try_from(&reference).unwrap();
        assert_eq!(index, FieldIndex(12));

        let field_ref: FieldReference = index.into();
        let Some(ReferenceType::DirectReference(segment)) = field_ref.reference_type else {
            panic!("expected direct reference");
        };
        let Some(SegmentReferenceType::StructField(field)) = segment.reference_type else {
            panic!("expected struct field segment");
        };
        assert_eq!(field.field, 12);
    }

    #[test]
    fn field_index_out_of_range_returns_parse_error() {
        let reference =
            parse_typed::<rules::reference<'_>>("$999999999999999999999", "reference").unwrap();
        let err = FieldIndex::try_from(&reference).unwrap_err();
        assert!(err.to_string().contains("Invalid reference index"));
    }

    #[test]
    fn anchor_wrappers_parse_and_roundtrip() {
        let anchor = parse_typed::<rules::anchor<'_>>("# 42", "anchor").unwrap();
        for kind in [
            AnchorKind::Function,
            AnchorKind::Type,
            AnchorKind::TypeVariation,
            AnchorKind::Extension,
        ] {
            let parsed = Anchor::parse(&anchor, kind).unwrap();
            assert_eq!(parsed.kind(), kind);
            assert_eq!(u32::from(parsed), 42);
        }
    }

    #[test]
    fn urn_anchor_wrapper_parse() {
        let urn_anchor = parse_typed::<rules::urn_anchor<'_>>("@ 7", "urn_anchor").unwrap();
        assert_eq!(u32::from(UrnAnchor::try_from(&urn_anchor).unwrap()), 7);
    }

    #[test]
    fn table_path_preserves_identifier_and_quoted_names() {
        let table_name =
            parse_typed::<rules::table_name<'_>>("db.\"two words\"", "table_name").unwrap();
        let table = TablePath::from(&table_name);
        assert_eq!(
            table.0.iter().map(|n| n.0.as_str()).collect::<Vec<_>>(),
            vec!["db", "two words"]
        );
    }
}
