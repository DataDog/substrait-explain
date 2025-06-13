use substrait::proto::expression::field_reference::ReferenceType;
use substrait::proto::expression::literal::LiteralType;
use substrait::proto::expression::{
    FieldReference, Literal, ReferenceSegment, RexType, ScalarFunction, reference_segment,
};
use substrait::proto::function_argument::ArgType;
use substrait::proto::r#type::{I64, Kind, Nullability};
use substrait::proto::{Expression, FunctionArgument, Type};

use super::{ParsePair, Rule, unescape_string, unwrap_single_pair};
use crate::extensions::simple::ExtensionKind;
use crate::parser::ScopedParsePair;
use crate::parser::types::get_and_validate_anchor;
use crate::textify::{Scope, TextifyError};

fn reference(index: i32) -> FieldReference {
    // XXX: Why is it so hard to make a struct field reference? This is
    // surprisingly complex
    FieldReference {
        reference_type: Some(ReferenceType::DirectReference(ReferenceSegment {
            reference_type: Some(reference_segment::ReferenceType::StructField(Box::new(
                reference_segment::StructField {
                    field: index,
                    child: None,
                },
            ))),
        })),
        root_type: None,
    }
}

impl ParsePair for FieldReference {
    fn rule() -> Rule {
        Rule::reference
    }

    fn parse_pair(pair: pest::iterators::Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());
        let inner = unwrap_single_pair(pair);
        let index: i32 = inner.as_str().parse().unwrap();

        // TODO: Other types of references.
        reference(index)
    }
}

fn to_int_literal(
    scope: &impl Scope,
    value: pest::iterators::Pair<Rule>,
    typ: Option<Type>,
) -> Literal {
    assert_eq!(value.as_rule(), Rule::integer);
    let parsed_value: i64 = value.as_str().parse().unwrap();

    const DEFAULT_KIND: Kind = Kind::I64(I64 {
        type_variation_reference: 0,
        nullability: Nullability::Required as i32,
    });

    // If no type is provided, we assume i64, Nullability::Required.
    let kind = typ.and_then(|t| t.kind).unwrap_or(DEFAULT_KIND);

    let (lit, nullability, tvar) = match &kind {
        // If no type is provided, we assume i64, Nullability::Required.
        Kind::I8(i) => (
            LiteralType::I8(parsed_value as i32),
            i.nullability,
            i.type_variation_reference,
        ),
        Kind::I16(i) => (
            LiteralType::I16(parsed_value as i32),
            i.nullability,
            i.type_variation_reference,
        ),
        Kind::I32(i) => (
            LiteralType::I32(parsed_value as i32),
            i.nullability,
            i.type_variation_reference,
        ),
        Kind::I64(i) => (
            LiteralType::I64(parsed_value),
            i.nullability,
            i.type_variation_reference,
        ),
        k => {
            scope.push_error(
                TextifyError::invalid(
                    "int_literal_type",
                    Some(value.as_str().to_string()),
                    format!("Invalid type for integer literal: {:?}", k),
                )
                .into(),
            );
            (
                LiteralType::I64(parsed_value),
                Nullability::Required as i32,
                0,
            )
        }
    };

    Literal {
        literal_type: Some(lit),
        nullable: nullability != Nullability::Required as i32,
        type_variation_reference: tvar,
    }
}

impl ScopedParsePair for Literal {
    fn rule() -> Rule {
        Rule::literal
    }

    fn parse_pair<S: Scope>(scope: &mut S, pair: pest::iterators::Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());
        let mut pairs = pair.into_inner();
        let value = pairs.next().unwrap();
        let typ = pairs.next();
        let typ = typ.map(|t| Type::parse_pair(scope, t));
        assert!(pairs.next().is_none());

        match value.as_rule() {
            Rule::integer => to_int_literal(scope, value, typ),
            // Rule::string_literal => LiteralType::String(unescape_string(inner.as_str(), '\'', '\'')),
            _ => unreachable!("Literal unexpected rule: {:?}", value.as_rule()),
        }
    }
}

impl ScopedParsePair for ScalarFunction {
    fn rule() -> Rule {
        Rule::function_call
    }

    fn parse_pair<S: Scope>(scope: &mut S, pair: pest::iterators::Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());
        let mut pairs = pair.into_inner();
        let name = Name::parse_pair(pairs.next().unwrap());
        let mut next = pairs.next().unwrap();

        // TODO: Function Options.

        let mut anchor = None;
        if let Rule::anchor = next.as_rule() {
            anchor = Some(unwrap_single_pair(next).as_str().parse().unwrap());
            next = pairs.next().unwrap();
        }

        // TODO: Handle uri_anchor.
        let mut _uri_anchor: Option<u32> = None;
        if let Rule::uri_anchor = next.as_rule() {
            _uri_anchor = Some(unwrap_single_pair(next).as_str().parse().unwrap());
            next = pairs.next().unwrap();
        }
        assert_eq!(next.as_rule(), Rule::argument_list);
        // TODO: Handle enum and type arguments.
        let arguments = next
            .into_inner()
            .map(|e| Expression::parse_pair(scope, e))
            .map(|e| FunctionArgument {
                arg_type: Some(ArgType::Value(e)),
            })
            .collect();

        let output_type = pairs.next().map(|t| Type::parse_pair(scope, t));

        if let Some(v) = pairs.next() {
            panic!("Expected no more pairs, found '{}': {:?}", v.as_str(), v);
        }

        let anchor = get_and_validate_anchor(scope, ExtensionKind::Function, anchor, &name.0);

        ScalarFunction {
            function_reference: anchor,
            arguments,
            // TODO: Function Options.
            options: vec![],
            output_type,
            #[allow(deprecated)]
            args: vec![],
        }
    }
}

impl ScopedParsePair for Expression {
    fn rule() -> Rule {
        Rule::expression
    }

    fn parse_pair<S: Scope>(scope: &mut S, pair: pest::iterators::Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());
        let inner = unwrap_single_pair(pair);

        match inner.as_rule() {
            // Rule::reference => RexType::Selection(Reference::parse_pair(inner)),
            // Expression::Reference(Reference::parse_pair(inner)),
            Rule::literal => Expression {
                rex_type: Some(RexType::Literal(Literal::parse_pair(scope, inner))),
            },
            Rule::function_call => Expression {
                rex_type: Some(RexType::ScalarFunction(ScalarFunction::parse_pair(
                    scope, inner,
                ))),
            },
            Rule::reference => Expression {
                rex_type: Some(RexType::Selection(Box::new(FieldReference::parse_pair(
                    inner,
                )))),
            },
            _ => unimplemented!("Expression unexpected rule: {:?}", inner.as_rule()),
        }
    }
}

struct Name(String);

impl ParsePair for Name {
    fn rule() -> Rule {
        Rule::name
    }

    fn parse_pair(pair: pest::iterators::Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());
        let inner = unwrap_single_pair(pair);
        match inner.as_rule() {
            Rule::identifier => Name(inner.as_str().to_string()),
            Rule::quoted_name => Name(unescape_string(inner.as_str(), '\'', '\'')),
            _ => unreachable!("Name unexpected rule: {:?}", inner.as_rule()),
        }
    }
}

#[cfg(test)]
mod tests {
    use pest::Parser as PestParser;

    use super::*;
    use crate::fixtures::TestContext;
    use crate::parser::ExpressionParser;
    use crate::textify::ErrorQueue;

    fn parse_exact(rule: Rule, input: &str) -> pest::iterators::Pair<Rule> {
        let mut pairs = ExpressionParser::parse(rule, input).unwrap();
        assert_eq!(pairs.as_str(), input);
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);
        pair
    }

    fn assert_parses_to<T: ParsePair + PartialEq + std::fmt::Debug>(input: &str, expected: T) {
        let pair = parse_exact(T::rule(), input);
        let actual = T::parse_pair(pair);
        assert_eq!(actual, expected);
    }

    fn assert_parses_with<S: Scope, T: ScopedParsePair + PartialEq + std::fmt::Debug>(
        scope: &mut S,
        input: &str,
        expected: T,
    ) {
        let pair = parse_exact(T::rule(), input);
        let actual = T::parse_pair(scope, pair);
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_parse_field_reference() {
        assert_parses_to("$1", reference(1));
    }

    #[test]
    fn test_parse_integer_literal() {
        let ctx = TestContext::new();
        let errors = ErrorQueue::default();
        let expected = Literal {
            literal_type: Some(LiteralType::I64(1)),
            nullable: false,
            type_variation_reference: 0,
        };
        assert_parses_with(&mut ctx.scope(&errors), "1", expected);
    }

    // #[test]
    // fn test_parse_string_literal() {
    //     assert_parses_to("'hello'", Literal::String("hello".to_string()));
    // }

    // #[test]
    // fn test_parse_function_call_simple() {
    //     assert_parses_to(
    //         "add()",
    //         FunctionCall {
    //             name: "add".to_string(),
    //             parameters: None,
    //             anchor: None,
    //             uri_anchor: None,
    //             arguments: vec![],
    //         },
    //     );
    // }

    // #[test]
    // fn test_parse_function_call_with_parameters() {
    //     assert_parses_to(
    //         "add<param1, param2>()",
    //         FunctionCall {
    //             name: "add".to_string(),
    //             parameters: Some(vec!["param1".to_string(), "param2".to_string()]),
    //             anchor: None,
    //             uri_anchor: None,
    //             arguments: vec![],
    //         },
    //     );
    // }

    // #[test]
    // fn test_parse_function_call_with_anchor() {
    //     assert_parses_to(
    //         "add#1()",
    //         FunctionCall {
    //             name: "add".to_string(),
    //             parameters: None,
    //             anchor: Some(1),
    //             uri_anchor: None,
    //             arguments: vec![],
    //         },
    //     );
    // }

    // #[test]
    // fn test_parse_function_call_with_uri_anchor() {
    //     assert_parses_to(
    //         "add@1()",
    //         FunctionCall {
    //             name: "add".to_string(),
    //             parameters: None,
    //             anchor: None,
    //             uri_anchor: Some(1),
    //             arguments: vec![],
    //         },
    //     );
    // }

    // #[test]
    // fn test_parse_function_call_all_optionals() {
    //     assert_parses_to(
    //         "add<param1, param2>#1@2()",
    //         FunctionCall {
    //             name: "add".to_string(),
    //             parameters: Some(vec!["param1".to_string(), "param2".to_string()]),
    //             anchor: Some(1),
    //             uri_anchor: Some(2),
    //             arguments: vec![],
    //         },
    //     );
    // }

    // #[test]
    // fn test_parse_function_call_with_simple_arguments() {
    //     assert_parses_to(
    //         "add(1, 2)",
    //         FunctionCall {
    //             name: "add".to_string(),
    //             parameters: None,
    //             anchor: None,
    //             uri_anchor: None,
    //             arguments: vec![
    //                 Expression::Literal(Literal::Integer(1)),
    //                 Expression::Literal(Literal::Integer(2)),
    //             ],
    //         },
    //     );
    // }

    // #[test]
    // fn test_parse_function_call_with_nested_function() {
    //     assert_parses_to(
    //         "outer_func(inner_func(), $1)",
    //         Expression::FunctionCall(Box::new(FunctionCall {
    //             name: "outer_func".to_string(),
    //             parameters: None,
    //             anchor: None,
    //             uri_anchor: None,
    //             arguments: vec![
    //                 Expression::FunctionCall(Box::new(FunctionCall {
    //                     name: "inner_func".to_string(),
    //                     parameters: None,
    //                     anchor: None,
    //                     uri_anchor: None,
    //                     arguments: vec![],
    //                 })),
    //                 Expression::Reference(Reference(1)),
    //             ],
    //         })),
    //     );
    // }

    // #[test]
    // fn test_parse_function_call_funny_names() {
    //     assert_parses_to(
    //         "'funny name'<param1, param2>#1@2()",
    //         FunctionCall {
    //             name: "funny name".to_string(),
    //             parameters: Some(vec!["param1".to_string(), "param2".to_string()]),
    //             anchor: Some(1),
    //             uri_anchor: Some(2),
    //             arguments: vec![],
    //         },
    //     );
    // }

    // #[test]
    // fn test_parse_empty_string_literal() {
    //     assert_parses_to("''", Literal::String("".to_string()));
    // }
}
