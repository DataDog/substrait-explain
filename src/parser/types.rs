use pest::iterators::Pair;
use substrait::proto::r#type::{Kind, Nullability, Parameter};
use substrait::proto::{self, Type};

use super::{ParsePair, Rule, ScopedParsePair, iter_pairs, unwrap_single_pair};
use crate::extensions::SimpleExtensions;
use crate::extensions::simple::ExtensionKind;
use crate::parser::{ErrorKind, MessageParseError};

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

impl ParsePair for Nullability {
    fn rule() -> Rule {
        Rule::nullability
    }

    fn message() -> &'static str {
        "Nullability"
    }

    fn parse_pair(pair: Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Rule::nullability);
        match pair.as_str() {
            "?" => Nullability::Nullable,
            "" => Nullability::Required,
            "⁉" => Nullability::Unspecified,
            _ => panic!("Invalid nullability: {}", pair.as_str()),
        }
    }
}

impl ScopedParsePair for Parameter {
    fn rule() -> Rule {
        Rule::parameter
    }

    fn message() -> &'static str {
        "Parameter"
    }

    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Rule::parameter);
        let inner = unwrap_single_pair(pair);
        match inner.as_rule() {
            Rule::r#type => Ok(Parameter {
                parameter: Some(proto::r#type::parameter::Parameter::DataType(
                    Type::parse_pair(extensions, inner)?,
                )),
            }),
            _ => unimplemented!("{:?}", inner.as_rule()),
        }
    }
}

fn parse_simple_type(pair: Pair<Rule>) -> Type {
    assert_eq!(pair.as_rule(), Rule::simple_type);
    let mut iter = iter_pairs(pair.into_inner());
    let name = iter.pop_if(Rule::simple_type_name).unwrap().as_str();
    let nullability = iter.parse_next::<Nullability>();
    iter.done();

    let kind = match name {
        "boolean" => Kind::Bool(proto::r#type::Boolean {
            nullability: nullability.into(),
            type_variation_reference: 0,
        }),
        "i64" => Kind::I64(proto::r#type::I64 {
            nullability: nullability.into(),
            type_variation_reference: 0,
        }),
        "i32" => Kind::I32(proto::r#type::I32 {
            nullability: nullability.into(),
            type_variation_reference: 0,
        }),
        "i16" => Kind::I16(proto::r#type::I16 {
            nullability: nullability.into(),
            type_variation_reference: 0,
        }),
        "i8" => Kind::I8(proto::r#type::I8 {
            nullability: nullability.into(),
            type_variation_reference: 0,
        }),
        "fp32" => Kind::Fp32(proto::r#type::Fp32 {
            nullability: nullability.into(),
            type_variation_reference: 0,
        }),
        "fp64" => Kind::Fp64(proto::r#type::Fp64 {
            nullability: nullability.into(),
            type_variation_reference: 0,
        }),
        "string" => Kind::String(proto::r#type::String {
            nullability: nullability.into(),
            type_variation_reference: 0,
        }),
        _ => unimplemented!("{}", name),
    };

    Type { kind: Some(kind) }
}

fn parse_compound_type(
    extensions: &SimpleExtensions,
    pair: Pair<Rule>,
) -> Result<Type, MessageParseError> {
    assert_eq!(pair.as_rule(), Rule::compound_type);
    let inner = unwrap_single_pair(pair);
    match inner.as_rule() {
        Rule::list_type => parse_list_type(extensions, inner),
        // Rule::map_type => parse_map_type(inner),
        // Rule::struct_type => parse_struct_type(inner),
        _ => unimplemented!("{:?}", inner.as_rule()),
    }
}

fn parse_list_type(
    extensions: &SimpleExtensions,
    pair: Pair<Rule>,
) -> Result<Type, MessageParseError> {
    assert_eq!(pair.as_rule(), Rule::list_type);
    let mut iter = iter_pairs(pair.into_inner());
    let nullability = iter.parse_next::<Nullability>();
    let inner = iter.parse_next_scoped::<Type>(extensions)?;
    iter.done();

    Ok(Type {
        kind: Some(Kind::List(Box::new(proto::r#type::List {
            nullability: nullability.into(),
            r#type: Some(Box::new(inner)),
            type_variation_reference: 0,
        }))),
    })
}

fn parse_parameters(
    extensions: &SimpleExtensions,
    pair: Pair<Rule>,
) -> Result<Vec<Parameter>, MessageParseError> {
    assert_eq!(pair.as_rule(), Rule::parameters);
    let mut iter = iter_pairs(pair.into_inner());
    let mut params = Vec::new();
    while let Some(param) = iter.parse_if_next_scoped::<Parameter>(extensions) {
        params.push(param?);
    }
    iter.done();
    Ok(params)
}

fn parse_user_defined_type(
    extensions: &SimpleExtensions,
    pair: Pair<Rule>,
) -> Result<Type, MessageParseError> {
    let span = pair.as_span();
    assert_eq!(pair.as_rule(), Rule::user_defined_type);
    let mut iter = iter_pairs(pair.into_inner());
    let name = iter.pop_if(Rule::name).unwrap().as_str();

    let anchor = iter
        .pop_if(Rule::anchor)
        .map(|n| unwrap_single_pair(n).as_str().parse::<u32>().unwrap());

    // TODO: Handle uri_anchor; validate that it matches the anchor
    let _uri_anchor = iter
        .pop_if(Rule::uri_anchor)
        .map(|n| unwrap_single_pair(n).as_str().parse::<u32>().unwrap());

    let nullability = iter.parse_next::<Nullability>();
    let parameters = match iter.pop_if(Rule::parameters) {
        Some(p) => parse_parameters(extensions, p)?,
        None => Vec::new(),
    };
    iter.done();

    let anchor = get_and_validate_anchor(extensions, ExtensionKind::Type, anchor, name, span)?;

    Ok(Type {
        kind: Some(Kind::UserDefined(proto::r#type::UserDefined {
            type_reference: anchor,
            nullability: nullability.into(),
            type_parameters: parameters,
            type_variation_reference: 0,
        })),
    })
}

impl ScopedParsePair for Type {
    fn rule() -> Rule {
        Rule::r#type
    }

    fn message() -> &'static str {
        "Type"
    }

    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Rule::r#type);
        let inner = unwrap_single_pair(pair);
        match inner.as_rule() {
            Rule::simple_type => Ok(parse_simple_type(inner)),
            Rule::compound_type => parse_compound_type(extensions, inner),
            Rule::user_defined_type => parse_user_defined_type(extensions, inner),
            _ => unimplemented!("{:?}", inner.as_rule()),
        }
    }
}

#[cfg(test)]
mod tests {
    use pest::Parser;
    use substrait::proto::r#type::{I64, Kind, Nullability};

    use super::*;
    use crate::parser::ExpressionParser;

    #[test]
    fn test_parse_simple_type() {
        let mut pairs = ExpressionParser::parse(Rule::simple_type, "i64").unwrap();
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);
        let t = parse_simple_type(pair);
        assert_eq!(
            t,
            Type {
                kind: Some(Kind::I64(I64 {
                    nullability: Nullability::Required as i32,
                    type_variation_reference: 0,
                })),
            }
        );

        let mut pairs = ExpressionParser::parse(Rule::simple_type, "string?").unwrap();
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);
        let t = parse_simple_type(pair);
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
        let mut pairs = ExpressionParser::parse(Rule::r#type, "i64").unwrap();
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);
        let t = Type::parse_pair(&extensions, pair).unwrap();
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
        let mut pairs = ExpressionParser::parse(Rule::list_type, "list<i64>").unwrap();
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);
        let t = parse_list_type(&extensions, pair).unwrap();
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
        let mut pairs = ExpressionParser::parse(Rule::parameters, "<i64?,string>").unwrap();
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);
        let t = parse_parameters(&extensions, pair).unwrap();
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
            .add_extension_uri("some_source".to_string(), 4)
            .unwrap();
        extensions
            .add_extension(ExtensionKind::Type, 4, 42, "udt".to_string())
            .unwrap();
        let mut pairs = ExpressionParser::parse(Rule::user_defined_type, "udt#42<i64?>").unwrap();
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);

        let t = parse_user_defined_type(&extensions, pair).unwrap();
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
