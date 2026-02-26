use pest_typed::Spanned;
use substrait::proto::r#type::{Kind, Nullability, Parameter};
use substrait::proto::{self, Type};

use super::common::{ErrorKind, MessageParseError, parse_typed, rules, typed_to_pest_span};
use super::convert::{Lower, Name, ParseCtx, TypeAnchor, UrnAnchor};
use crate::extensions::SimpleExtensions;
use crate::extensions::simple::ExtensionKind;

// Given a name and an optional anchor, get the anchor and validate it. Errors will be pushed to the Scope error accumulator,
// and the anchor will be returned if it is valid.
pub(crate) fn get_and_validate_anchor(
    extensions: &SimpleExtensions,
    kind: ExtensionKind,
    anchor: Option<u32>,
    name: &str,
    span: pest::Span,
) -> Result<u32, MessageParseError> {
    match anchor {
        Some(a) => match extensions.is_name_unique(kind, a, name) {
            Ok(_) => Ok(a),
            Err(e) => {
                let message = "Error matching name to anchor".to_string();
                let error = MessageParseError {
                    message: kind.name(),
                    kind: ErrorKind::Lookup(e),
                    error: Box::new(pest::error::Error::new_from_span(
                        pest::error::ErrorVariant::CustomError { message },
                        span,
                    )),
                };
                Err(error)
            }
        },
        None => match extensions.find_by_name(kind, name) {
            Ok(a) => Ok(a),
            Err(e) => {
                let message = "Error finding extension for name".to_string();
                let error = MessageParseError {
                    message: kind.name(),
                    kind: ErrorKind::Lookup(e),
                    error: Box::new(pest::error::Error::new_from_span(
                        pest::error::ErrorVariant::CustomError { message },
                        span,
                    )),
                };
                Err(error)
            }
        },
    }
}

fn parse_name_text(node: &rules::name<'_>) -> String {
    Name::from(node).0
}

fn parse_nullability(node: &rules::nullability<'_>) -> Nullability {
    match node.span.as_str() {
        "?" => Nullability::Nullable,
        "" => Nullability::Required,
        "⁉" => Nullability::Unspecified,
        other => unreachable!("grammar guarantees nullability token, got: {other}"),
    }
}

fn parse_parameter_node(
    extensions: &SimpleExtensions,
    node: &rules::parameter<'_>,
) -> Result<Parameter, MessageParseError> {
    if let Some(data_type) = node.r#type() {
        return Ok(Parameter {
            parameter: Some(proto::r#type::parameter::Parameter::DataType(
                parse_type_node(extensions, data_type)?,
            )),
        });
    }

    if let Some(integer) = node.integer() {
        return Err(MessageParseError::invalid(
            "parameter",
            typed_to_pest_span(integer.span()),
            format!(
                "Integer parameters are not currently supported: {}",
                integer.span.as_str()
            ),
        ));
    }

    if let Some(name) = node.name() {
        return Err(MessageParseError::invalid(
            "parameter",
            typed_to_pest_span(name.span()),
            format!(
                "Named parameters are not currently supported: {}",
                name.span.as_str()
            ),
        ));
    }

    // Grammar guarantees parameter is one of: type, integer, or name.
    unreachable!("parameter must be type, integer, or name");
}

fn parse_simple_type_node(node: &rules::simple_type<'_>) -> Result<Type, MessageParseError> {
    let name = node.simple_type_name().span.as_str();
    let nullability = parse_nullability(node.nullability());

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
                typed_to_pest_span(node.simple_type_name().span()),
                format!("Type '{name}' is not currently supported"),
            ));
        }
    };

    Ok(Type { kind: Some(kind) })
}

fn parse_compound_type_node(
    extensions: &SimpleExtensions,
    node: &rules::compound_type<'_>,
) -> Result<Type, MessageParseError> {
    if let Some(list_type) = node.list_type() {
        return parse_list_type_node(extensions, list_type);
    }
    if node.map_type().is_some() {
        return Err(MessageParseError::invalid(
            "Type",
            typed_to_pest_span(node.span()),
            "Map types are not currently supported",
        ));
    }
    if node.struct_type().is_some() {
        return Err(MessageParseError::invalid(
            "Type",
            typed_to_pest_span(node.span()),
            "Struct types are not currently supported",
        ));
    }
    // Grammar guarantees compound_type is one of: list_type, map_type, struct_type.
    unreachable!("compound_type must be list_type, map_type, or struct_type")
}

fn parse_list_type_node(
    extensions: &SimpleExtensions,
    node: &rules::list_type<'_>,
) -> Result<Type, MessageParseError> {
    let nullability = parse_nullability(node.nullability());
    let inner = parse_type_node(extensions, node.r#type())?;

    Ok(Type {
        kind: Some(Kind::List(Box::new(proto::r#type::List {
            nullability: nullability as i32,
            r#type: Some(Box::new(inner)),
            type_variation_reference: 0,
        }))),
    })
}

fn parse_parameters_node(
    extensions: &SimpleExtensions,
    node: &rules::parameters<'_>,
) -> Result<Vec<Parameter>, MessageParseError> {
    let mut params = Vec::new();
    if let Some((first, rest)) = node.parameter() {
        params.push(parse_parameter_node(extensions, first)?);
        for parameter in rest {
            params.push(parse_parameter_node(extensions, parameter)?);
        }
    }
    Ok(params)
}

fn parse_user_defined_type_node(
    extensions: &SimpleExtensions,
    node: &rules::user_defined_type<'_>,
) -> Result<Type, MessageParseError> {
    let span = typed_to_pest_span(node.span());
    let name = parse_name_text(node.name());
    let anchor = node.anchor().map(TypeAnchor::try_from).transpose()?;

    // TODO: Handle urn_anchor; validate that it matches the anchor
    let _urn_anchor = node.urn_anchor().map(UrnAnchor::try_from).transpose()?;

    let nullability = parse_nullability(node.nullability());
    let parameters = match node.parameters() {
        Some(parameters) => parse_parameters_node(extensions, parameters)?,
        None => Vec::new(),
    };

    let anchor = get_and_validate_anchor(
        extensions,
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

pub(crate) fn parse_type_node(
    extensions: &SimpleExtensions,
    node: &rules::r#type<'_>,
) -> Result<Type, MessageParseError> {
    if let Some(simple_type) = node.simple_type() {
        return parse_simple_type_node(simple_type);
    }
    if let Some(compound_type) = node.compound_type() {
        return parse_compound_type_node(extensions, compound_type);
    }
    if let Some(user_defined) = node.user_defined_type() {
        return parse_user_defined_type_node(extensions, user_defined);
    }

    // Grammar guarantees type is one of: simple_type, compound_type, user_defined_type.
    unreachable!("type must be simple_type, compound_type, or user_defined_type")
}

impl Lower for rules::r#type<'_> {
    type Output = Type;

    fn lower(&self, cx: &ParseCtx<'_>) -> Result<Self::Output, MessageParseError> {
        parse_type_node(cx.extensions, self)
    }
}

pub(crate) fn parse_type(
    extensions: &SimpleExtensions,
    input: &str,
) -> Result<Type, MessageParseError> {
    let cx = ParseCtx { extensions };
    let typed = parse_typed::<rules::r#type<'_>>(input, "Type")?;
    typed.lower(&cx)
}

#[cfg(test)]
mod tests {
    use substrait::proto::r#type::{I64, Kind, Nullability};

    use super::*;

    #[test]
    fn test_parse_simple_type() {
        let typed = parse_typed::<rules::simple_type<'_>>("i64", "simple_type").unwrap();
        let t = parse_simple_type_node(&typed).unwrap();
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
        let t = parse_simple_type_node(&typed).unwrap();
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
        let t = parse_type(&extensions, "i64").unwrap();
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
        let typed = parse_typed::<rules::list_type<'_>>("list<i64>", "list_type").unwrap();
        let t = parse_list_type_node(&extensions, &typed).unwrap();
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
        let typed = parse_typed::<rules::parameters<'_>>("<i64?,string>", "parameters").unwrap();
        let t = parse_parameters_node(&extensions, &typed).unwrap();
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

        let typed =
            parse_typed::<rules::user_defined_type<'_>>("udt#42<i64?>", "user_defined_type")
                .unwrap();
        let t = parse_user_defined_type_node(&extensions, &typed).unwrap();
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
