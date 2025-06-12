use pest::iterators::Pair;
use substrait::proto::{
    self, Type,
    r#type::{Kind, Nullability, Parameter},
};

use crate::{extensions::ExtensionLookup, textify::Scope};

use super::{ParsePair, Rule, ScopedParsePair, iter_pairs, unwrap_single_pair};

impl ParsePair for Nullability {
    fn rule() -> Rule {
        Rule::nullability
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

    fn parse_pair<S: Scope>(scope: &mut S, pair: Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Rule::parameter);
        let inner = unwrap_single_pair(pair);
        match inner.as_rule() {
            Rule::r#type => Parameter {
                parameter: Some(proto::r#type::parameter::Parameter::DataType(
                    Type::parse_pair(scope, inner),
                )),
            },
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
        "string" => Kind::String(proto::r#type::String {
            nullability: nullability.into(),
            type_variation_reference: 0,
        }),
        _ => unimplemented!("{}", name),
    };

    Type { kind: Some(kind) }
}

fn parse_compound_type<S: Scope>(scope: &mut S, pair: Pair<Rule>) -> Type {
    assert_eq!(pair.as_rule(), Rule::compound_type);
    let inner = unwrap_single_pair(pair);
    match inner.as_rule() {
        Rule::list_type => parse_list_type(scope, inner),
        // Rule::map_type => parse_map_type(inner),
        // Rule::struct_type => parse_struct_type(inner),
        _ => unimplemented!("{:?}", inner.as_rule()),
    }
}

fn parse_list_type<S: Scope>(scope: &mut S, pair: Pair<Rule>) -> Type {
    assert_eq!(pair.as_rule(), Rule::list_type);
    let mut iter = iter_pairs(pair.into_inner());
    let nullability = iter.parse_next::<Nullability>();
    let inner = iter.parse_next_scoped::<_, Type>(scope);
    iter.done();

    Type {
        kind: Some(Kind::List(Box::new(proto::r#type::List {
            nullability: nullability.into(),
            r#type: Some(Box::new(inner)),
            type_variation_reference: 0,
        }))),
    }
}

fn parse_parameters<S: Scope>(scope: &mut S, pair: Pair<Rule>) -> Vec<Parameter> {
    assert_eq!(pair.as_rule(), Rule::parameters);
    let mut iter = iter_pairs(pair.into_inner());
    let mut params = Vec::new();
    while let Some(param) = iter.parse_if_next_scoped::<Parameter, _>(scope) {
        // TODO: Other kinds of parameters
        params.push(param);
    }
    iter.done();
    params
}

fn parse_user_defined_type<S: Scope>(scope: &mut S, pair: Pair<Rule>) -> Type {
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
        Some(p) => dbg!(parse_parameters(scope, p)),
        None => Vec::new(),
    };
    iter.done();

    let anchor = match anchor {
        None => match scope.extensions().lookup_type(name) {
            Ok(t) => t.type_anchor,
            Err(e) => {
                scope.push_error(e.into());
                0
            }
        },
        Some(anchor) => match scope.extensions().find_type_with_anchor(name, anchor) {
            Ok(t) => t.type_anchor,
            Err(e) => {
                scope.push_error(e.into());
                anchor
            }
        },
    };

    Type {
        kind: Some(Kind::UserDefined(proto::r#type::UserDefined {
            type_reference: anchor,
            nullability: nullability.into(),
            type_parameters: parameters,
            type_variation_reference: 0,
        })),
    }
}

impl ScopedParsePair for Type {
    fn rule() -> Rule {
        Rule::r#type
    }

    fn parse_pair<S: Scope>(scope: &mut S, pair: Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Rule::r#type);
        let inner = unwrap_single_pair(pair);
        match inner.as_rule() {
            Rule::simple_type => parse_simple_type(inner),
            Rule::compound_type => parse_compound_type(scope, inner),
            Rule::user_defined_type => parse_user_defined_type(scope, inner),
            _ => unimplemented!("{:?}", inner.as_rule()),
        }
    }
}

#[cfg(test)]
mod tests {
    use pest::Parser;
    use substrait::proto::r#type::{I64, Kind, Nullability};

    use crate::{fixtures::TestContext, parser::ExpressionParser, textify::ErrorQueue};

    use super::*;

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
        let ctx = TestContext::default();
        let errors = ErrorQueue::default();
        let mut scope = ctx.scope(&errors);

        let mut pairs = ExpressionParser::parse(Rule::r#type, "i64").unwrap();
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);
        let t = Type::parse_pair(&mut scope, pair);
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
        let ctx = TestContext::default();
        let errors = ErrorQueue::default();
        let mut scope = ctx.scope(&errors);

        let mut pairs = ExpressionParser::parse(Rule::list_type, "list<i64>").unwrap();
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);
        let t = parse_list_type(&mut scope, pair);
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
        let ctx = TestContext::default();
        let errors = ErrorQueue::default();
        let mut scope = ctx.scope(&errors);

        let mut pairs = ExpressionParser::parse(Rule::parameters, "<i64?,string>").unwrap();
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);
        let t = parse_parameters(&mut scope, pair);
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
        let ctx = TestContext::default();
        let errors = ErrorQueue::default();
        let mut scope = ctx.scope(&errors);

        let mut pairs = ExpressionParser::parse(Rule::user_defined_type, "udt#42<i64?>").unwrap();
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);

        let t = parse_user_defined_type(&mut scope, pair);
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
