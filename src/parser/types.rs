//! Type fragment parsing and lowering.
//!
//! This module handles built-in and user-defined type parsing, including
//! extension anchor validation against [`SimpleExtensions`]. It also provides
//! the `ParseFragment` implementation for `substrait::proto::Type`.

use pest_typed::Spanned;
use substrait::proto::r#type::{Kind, Nullability, Parameter};
use substrait::proto::{self, Type};

use super::common::{MessageParseError, parse_typed, rules};
use super::convert::{Anchor, AnchorKind, Lower, Name, ParseCtx, UrnAnchor};
use crate::extensions::SimpleExtensions;
use crate::extensions::simple::ExtensionKind;
use crate::parser::ParseFragment;

/// Resolve an optional explicit anchor against extension metadata.
///
/// If `anchor` is present, it must match the provided name. If absent, the
/// anchor is looked up by name.
pub(crate) fn get_and_validate_anchor(
    extensions: &SimpleExtensions,
    kind: ExtensionKind,
    anchor: Option<u32>,
    name: &str,
    span: pest_typed::Span<'_>,
) -> Result<u32, MessageParseError> {
    match anchor {
        Some(a) => match extensions.is_name_unique(kind, a, name) {
            Ok(_) => Ok(a),
            Err(e) => Err(MessageParseError::lookup(
                kind.name(),
                e,
                span,
                "Error matching name to anchor",
            )),
        },
        None => match extensions.find_by_name(kind, name) {
            Ok(a) => Ok(a),
            Err(e) => Err(MessageParseError::lookup(
                kind.name(),
                e,
                span,
                "Error finding extension for name",
            )),
        },
    }
}

fn parse_nullability(node: &rules::nullability<'_>) -> Nullability {
    match node.span.as_str() {
        "?" => Nullability::Nullable,
        "" => Nullability::Required,
        "⁉" => Nullability::Unspecified,
        other => unreachable!("grammar guarantees nullability token, got: {other}"),
    }
}

impl Lower for rules::parameter<'_> {
    type Output = Parameter;

    fn lower(&self, cx: &ParseCtx<'_>) -> Result<Self::Output, MessageParseError> {
        if let Some(data_type) = self.r#type() {
            return Ok(Parameter {
                parameter: Some(proto::r#type::parameter::Parameter::DataType(
                    data_type.lower(cx)?,
                )),
            });
        }
        if let Some(integer) = self.integer() {
            return Err(MessageParseError::invalid(
                "parameter",
                integer.span(),
                format!(
                    "Integer parameters are not currently supported: {}",
                    integer.span.as_str()
                ),
            ));
        }
        if let Some(name) = self.name() {
            return Err(MessageParseError::invalid(
                "parameter",
                name.span(),
                format!(
                    "Named parameters are not currently supported: {}",
                    name.span.as_str()
                ),
            ));
        }

        // Grammar guarantees parameter is one of: type, integer, or name.
        unreachable!("parameter must be type, integer, or name")
    }
}

impl Lower for rules::simple_type<'_> {
    type Output = Type;

    fn lower(&self, _cx: &ParseCtx<'_>) -> Result<Self::Output, MessageParseError> {
        let name = self.simple_type_name().span.as_str();
        let nullability = parse_nullability(self.nullability());

        let kind = match name {
            "boolean" => Kind::Bool(proto::r#type::Boolean {
                nullability: nullability as i32,
                type_variation_reference: 0,
            }),
            "i64" => Kind::I64(proto::r#type::I64 {
                nullability: nullability as i32,
                type_variation_reference: 0,
            }),
            "i32" => Kind::I32(proto::r#type::I32 {
                nullability: nullability as i32,
                type_variation_reference: 0,
            }),
            "i16" => Kind::I16(proto::r#type::I16 {
                nullability: nullability as i32,
                type_variation_reference: 0,
            }),
            "i8" => Kind::I8(proto::r#type::I8 {
                nullability: nullability as i32,
                type_variation_reference: 0,
            }),
            "fp32" => Kind::Fp32(proto::r#type::Fp32 {
                nullability: nullability as i32,
                type_variation_reference: 0,
            }),
            "fp64" => Kind::Fp64(proto::r#type::Fp64 {
                nullability: nullability as i32,
                type_variation_reference: 0,
            }),
            "string" => Kind::String(proto::r#type::String {
                nullability: nullability as i32,
                type_variation_reference: 0,
            }),
            "binary" => Kind::Binary(proto::r#type::Binary {
                nullability: nullability as i32,
                type_variation_reference: 0,
            }),
            #[allow(deprecated)]
            "timestamp" => Kind::Timestamp(proto::r#type::Timestamp {
                nullability: nullability as i32,
                type_variation_reference: 0,
            }),
            #[allow(deprecated)]
            "timestamp_tz" => Kind::TimestampTz(proto::r#type::TimestampTz {
                nullability: nullability as i32,
                type_variation_reference: 0,
            }),
            "date" => Kind::Date(proto::r#type::Date {
                nullability: nullability as i32,
                type_variation_reference: 0,
            }),
            "time" => Kind::Time(proto::r#type::Time {
                nullability: nullability as i32,
                type_variation_reference: 0,
            }),
            "interval_year" => Kind::IntervalYear(proto::r#type::IntervalYear {
                nullability: nullability as i32,
                type_variation_reference: 0,
            }),
            "uuid" => Kind::Uuid(proto::r#type::Uuid {
                nullability: nullability as i32,
                type_variation_reference: 0,
            }),
            _ => {
                return Err(MessageParseError::invalid(
                    "Type",
                    self.simple_type_name().span(),
                    format!("Type '{name}' is not currently supported"),
                ));
            }
        };

        Ok(Type { kind: Some(kind) })
    }
}

impl Lower for rules::list_type<'_> {
    type Output = Type;

    fn lower(&self, cx: &ParseCtx<'_>) -> Result<Self::Output, MessageParseError> {
        let nullability = parse_nullability(self.nullability());
        let inner = self.r#type().lower(cx)?;

        Ok(Type {
            kind: Some(Kind::List(Box::new(proto::r#type::List {
                nullability: nullability as i32,
                r#type: Some(Box::new(inner)),
                type_variation_reference: 0,
            }))),
        })
    }
}

impl Lower for rules::compound_type<'_> {
    type Output = Type;

    fn lower(&self, cx: &ParseCtx<'_>) -> Result<Self::Output, MessageParseError> {
        if let Some(list_type) = self.list_type() {
            return list_type.lower(cx);
        }
        if self.map_type().is_some() {
            return Err(MessageParseError::invalid(
                "Type",
                self.span(),
                "Map types are not currently supported",
            ));
        }
        if self.struct_type().is_some() {
            return Err(MessageParseError::invalid(
                "Type",
                self.span(),
                "Struct types are not currently supported",
            ));
        }

        // Grammar guarantees compound_type is one of: list_type, map_type, struct_type.
        unreachable!("compound_type must be list_type, map_type, or struct_type")
    }
}

impl Lower for rules::parameters<'_> {
    type Output = Vec<Parameter>;

    fn lower(&self, cx: &ParseCtx<'_>) -> Result<Self::Output, MessageParseError> {
        let mut params = Vec::new();
        if let Some((first, rest)) = self.parameter() {
            params.push(first.lower(cx)?);
            for parameter in rest {
                params.push(parameter.lower(cx)?);
            }
        }
        Ok(params)
    }
}

impl Lower for rules::user_defined_type<'_> {
    type Output = Type;

    fn lower(&self, cx: &ParseCtx<'_>) -> Result<Self::Output, MessageParseError> {
        let span = self.span();
        let name = Name::from(self.name()).0;
        let anchor = self
            .anchor()
            .map(|a| Anchor::parse(a, AnchorKind::Type))
            .transpose()?;

        // TODO: Handle urn_anchor; validate that it matches the anchor.
        let _urn_anchor = self.urn_anchor().map(UrnAnchor::try_from).transpose()?;

        let nullability = parse_nullability(self.nullability());
        let parameters = match self.parameters() {
            Some(parameters) => parameters.lower(cx)?,
            None => Vec::new(),
        };

        let anchor = get_and_validate_anchor(
            cx.extensions,
            ExtensionKind::Type,
            anchor.map(u32::from),
            &name,
            span,
        )?;

        Ok(Type {
            kind: Some(Kind::UserDefined(proto::r#type::UserDefined {
                type_reference: anchor,
                nullability: nullability as i32,
                type_parameters: parameters,
                type_variation_reference: 0,
            })),
        })
    }
}

impl Lower for rules::r#type<'_> {
    type Output = Type;

    fn lower(&self, cx: &ParseCtx<'_>) -> Result<Self::Output, MessageParseError> {
        if let Some(simple_type) = self.simple_type() {
            return simple_type.lower(cx);
        }
        if let Some(compound_type) = self.compound_type() {
            return compound_type.lower(cx);
        }
        if let Some(user_defined) = self.user_defined_type() {
            return user_defined.lower(cx);
        }

        // Grammar guarantees type is one of: simple_type, compound_type, user_defined_type.
        unreachable!("type must be simple_type, compound_type, or user_defined_type")
    }
}

impl ParseFragment for Type {
    fn parse_fragment(
        extensions: &SimpleExtensions,
        input: &str,
    ) -> Result<Self, MessageParseError> {
        let cx = ParseCtx { extensions };
        let typed = parse_typed::<rules::r#type<'_>>(input, "Type")?;
        typed.lower(&cx)
    }
}

#[cfg(test)]
mod tests {
    use substrait::proto::r#type::{I64, Kind, Nullability};

    use super::*;

    #[test]
    fn test_parse_simple_type() {
        let extensions = SimpleExtensions::default();
        let cx = ParseCtx {
            extensions: &extensions,
        };
        let typed = parse_typed::<rules::simple_type<'_>>("i64", "simple_type").unwrap();
        let t = typed.lower(&cx).unwrap();
        assert_eq!(
            t,
            Type {
                kind: Some(Kind::I64(I64 {
                    nullability: Nullability::Required as i32,
                    type_variation_reference: 0,
                })),
            }
        );

        let typed = parse_typed::<rules::simple_type<'_>>("string?", "simple_type").unwrap();
        let t = typed.lower(&cx).unwrap();
        assert_eq!(
            t,
            Type {
                kind: Some(Kind::String(proto::r#type::String {
                    nullability: Nullability::Nullable as i32,
                    type_variation_reference: 0,
                })),
            }
        );
    }

    #[test]
    fn test_parse_type() {
        let extensions = SimpleExtensions::default();
        let t = Type::parse_fragment(&extensions, "i64").unwrap();
        assert_eq!(
            t,
            Type {
                kind: Some(Kind::I64(I64 {
                    nullability: Nullability::Required as i32,
                    type_variation_reference: 0,
                }))
            }
        );
    }

    #[test]
    fn test_parse_list_type() {
        let extensions = SimpleExtensions::default();
        let cx = ParseCtx {
            extensions: &extensions,
        };
        let typed = parse_typed::<rules::list_type<'_>>("list<i64>", "list_type").unwrap();
        let t = typed.lower(&cx).unwrap();
        assert_eq!(
            t,
            Type {
                kind: Some(Kind::List(Box::new(proto::r#type::List {
                    nullability: Nullability::Required as i32,
                    r#type: Some(Box::new(Type {
                        kind: Some(Kind::I64(I64 {
                            nullability: Nullability::Required as i32,
                            type_variation_reference: 0,
                        }))
                    })),
                    type_variation_reference: 0,
                })))
            }
        );
    }

    #[test]
    fn test_parse_parameters() {
        let extensions = SimpleExtensions::default();
        let cx = ParseCtx {
            extensions: &extensions,
        };
        let typed = parse_typed::<rules::parameters<'_>>("<i64?,string>", "parameters").unwrap();
        let t = typed.lower(&cx).unwrap();
        assert_eq!(
            t,
            vec![
                Parameter {
                    parameter: Some(proto::r#type::parameter::Parameter::DataType(Type {
                        kind: Some(Kind::I64(proto::r#type::I64 {
                            nullability: Nullability::Nullable as i32,
                            type_variation_reference: 0,
                        })),
                    })),
                },
                Parameter {
                    parameter: Some(proto::r#type::parameter::Parameter::DataType(Type {
                        kind: Some(Kind::String(proto::r#type::String {
                            nullability: Nullability::Required as i32,
                            type_variation_reference: 0,
                        })),
                    })),
                },
            ]
        );
    }

    #[test]
    fn test_udts() {
        let mut extensions = SimpleExtensions::default();
        extensions
            .add_extension_urn("some_source".to_string(), 4)
            .unwrap();
        extensions
            .add_extension(ExtensionKind::Type, 4, 42, "udt".to_string())
            .unwrap();
        let cx = ParseCtx {
            extensions: &extensions,
        };

        let typed =
            parse_typed::<rules::user_defined_type<'_>>("udt#42<i64?>", "user_defined_type")
                .unwrap();
        let t = typed.lower(&cx).unwrap();
        assert_eq!(
            t,
            Type {
                kind: Some(Kind::UserDefined(proto::r#type::UserDefined {
                    type_reference: 42,
                    type_variation_reference: 0,
                    nullability: Nullability::Required as i32,
                    type_parameters: vec![Parameter {
                        parameter: Some(proto::r#type::parameter::Parameter::DataType(Type {
                            kind: Some(Kind::I64(proto::r#type::I64 {
                                nullability: Nullability::Nullable as i32,
                                type_variation_reference: 0,
                            })),
                        })),
                    }],
                }))
            }
        );
    }
}
