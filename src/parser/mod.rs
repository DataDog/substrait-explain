use pest::Parser;
use pest_derive::Parser;

mod extensions;
mod structural;

use crate::structure::{Expression, FunctionCall, Literal, Reference};

#[derive(Parser)]
#[grammar = "parser/expression_grammar.pest"] // Path relative to src
pub struct ExpressionParser;

pub fn parse_expression(rule: Rule, input: &str) -> Result<Expression, pest::error::Error<Rule>> {
    // Parse the string, if possible. This can fail if the input is invalid.
    let mut pairs = ExpressionParser::parse(rule, input)?;
    // If parsing was successful, there should be exactly one pair, parseable exactly into an Expression.
    let pair = pairs.next().unwrap();
    assert_eq!(pairs.next(), None);
    Ok(Expression::parse(pair))
}

fn unwrap_single_pair(pair: pest::iterators::Pair<Rule>) -> pest::iterators::Pair<Rule> {
    let mut pairs = pair.into_inner();
    let pair = pairs.next().unwrap();
    assert_eq!(pairs.next(), None);
    pair
}

fn unescape_string(s: &str, opener: char, closer: char) -> String {
    let mut result = String::new();
    let mut chars = s.chars();
    let first = chars.next().unwrap();
    assert_eq!(first, opener);
    // Skip the opening quote
    while let Some(c) = chars.next() {
        match c {
            c if c == closer => {
                // Skip the closing quote, and assert that there are no more characters.
                assert_eq!(chars.next(), None);
                break;
            }
            '\\' => {
                let next = chars.next().unwrap();
                match next {
                    'n' => result.push('\n'),
                    't' => result.push('\t'),
                    'r' => result.push('\r'),
                    // For all other characters (especially `"`, `'`, and `\`), we just
                    // push the character.
                    _ => result.push(next),
                }
            }
            _ => result.push(c),
        }
    }
    result
}

// A trait for converting a pest::iterators::Pair<Rule> into a Rust type. This
// is used to convert from the uniformly structured nesting
// pest::iterators::Pair<Rule> into more structured types.
trait ParsePair {
    // The rule that this type is parsed from.
    fn rule() -> Rule;

    // Parse a single instance of this type from a pest::iterators::Pair<Rule>.
    // The input must match the rule returned by `rule`; otherwise, a panic is
    // expected.
    fn parse(pair: pest::iterators::Pair<Rule>) -> Self;
}

trait ParsePairStr: ParsePair + Sized {
    fn parse_str(s: &str) -> Result<Self, pest::error::Error<Rule>> {
        let mut pairs = ExpressionParser::parse(Self::rule(), s)?;
        let pair = pairs.next().unwrap();
        Ok(Self::parse(pair))
    }
}

impl ParsePair for Reference {
    fn rule() -> Rule {
        Rule::reference
    }

    fn parse(pair: pest::iterators::Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());
        let inner = unwrap_single_pair(pair);
        Reference(inner.as_str().parse().unwrap())
    }
}

impl ParsePair for Literal {
    fn rule() -> Rule {
        Rule::literal
    }

    fn parse(pair: pest::iterators::Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());
        let inner = unwrap_single_pair(pair);
        match inner.as_rule() {
            Rule::integer => Literal::Integer(inner.as_str().parse().unwrap()),
            Rule::string_literal => Literal::String(unescape_string(inner.as_str(), '\'', '\'')),
            _ => unreachable!("Literal unexpected rule: {:?}", inner.as_rule()),
        }
    }
}

impl ParsePair for FunctionCall {
    fn rule() -> Rule {
        Rule::function_call
    }

    fn parse(pair: pest::iterators::Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());
        let mut pairs = pair.into_inner();
        let name = Name::parse(pairs.next().unwrap());
        let mut next = pairs.next().unwrap();

        let mut parameters = None;
        if let Rule::parameters = next.as_rule() {
            parameters = Some(next.into_inner().map(|p| Name::parse(p).0).collect());
            next = pairs.next().unwrap();
        }
        // TODO: Function Options.
        let mut anchor = None;
        if let Rule::anchor = next.as_rule() {
            anchor = Some(unwrap_single_pair(next).as_str().parse().unwrap());
            next = pairs.next().unwrap();
        }
        let mut uri_anchor = None;
        if let Rule::uri_anchor = next.as_rule() {
            uri_anchor = Some(unwrap_single_pair(next).as_str().parse().unwrap());
            next = pairs.next().unwrap();
        }
        assert_eq!(next.as_rule(), Rule::argument_list);
        let arguments = next.into_inner().map(Expression::parse).collect();

        assert_eq!(pairs.next(), None);
        FunctionCall {
            name: name.0,
            parameters: parameters,
            anchor: anchor,
            uri_anchor: uri_anchor,
            arguments: arguments,
        }
    }
}

impl ParsePair for Expression {
    fn rule() -> Rule {
        Rule::expression
    }

    fn parse(pair: pest::iterators::Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());
        let inner = unwrap_single_pair(pair);
        match inner.as_rule() {
            Rule::reference => Expression::Reference(Reference::parse(inner)),
            Rule::literal => Expression::Literal(Literal::parse(inner)),
            Rule::function_call => Expression::FunctionCall(Box::new(FunctionCall::parse(inner))),
            _ => unreachable!("Expression unexpected rule: {:?}", inner.as_rule()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct Name(String);

impl ParsePair for Name {
    fn rule() -> Rule {
        Rule::name
    }

    fn parse(pair: pest::iterators::Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());
        let inner = unwrap_single_pair(pair);
        match inner.as_rule() {
            Rule::identifier => Name(inner.as_str().to_string()),
            Rule::quoted_name => Name(unescape_string(inner.as_str(), '\"', '\"').to_string()),
            _ => unreachable!("Name unexpected rule: {:?}", inner.as_rule()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_exact(rule: Rule, input: &str) -> pest::iterators::Pair<Rule> {
        // println!("Parsing exact: {:?} input: {}", rule, input);
        let mut pairs = ExpressionParser::parse(rule, input).unwrap();
        let pair = pairs.next().unwrap();
        assert_eq!(pair.as_str(), input);
        assert_eq!(pairs.next(), None);
        pair
    }

    fn assert_parses_to<T: ParsePair + PartialEq + std::fmt::Debug>(input: &str, expected: T) {
        let pair = parse_exact(T::rule(), input);
        let actual = T::parse(pair);
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_parse_field_reference() {
        assert_parses_to("$0", Reference(0));
        assert_parses_to("$123", Reference(123));
    }

    #[test]
    fn test_parse_integer_literal() {
        assert_parses_to("42", Literal::Integer(42));
        assert_parses_to("0", Literal::Integer(0));
    }

    #[test]
    fn test_parse_string_literal() {
        assert_parses_to("'hello'", Literal::String("hello".to_string()));
        assert_parses_to("'world'", Literal::String("world".to_string()));
        assert_parses_to("'  spaced  '", Literal::String("  spaced  ".to_string()));
    }

    #[test]
    fn test_parse_function_call_simple() {
        assert_parses_to(
            "my_func()",
            FunctionCall {
                name: "my_func".to_string(),
                parameters: None,
                anchor: None,
                uri_anchor: None,
                arguments: vec![],
            },
        );
    }

    #[test]
    fn test_parse_function_call_with_parameters() {
        assert_parses_to(
            "generic_func<T, U>()",
            Expression::FunctionCall(Box::new(FunctionCall {
                name: "generic_func".to_string(),
                parameters: Some(vec!["T".to_string(), "U".to_string()]),
                anchor: None,
                uri_anchor: None,
                arguments: vec![],
            })),
        );
    }

    #[test]
    fn test_parse_function_call_with_anchor() {
        assert_parses_to(
            "func_anchor#123()",
            Expression::FunctionCall(Box::new(FunctionCall {
                name: "func_anchor".to_string(),
                parameters: None,
                anchor: Some(123),
                uri_anchor: None,
                arguments: vec![],
            })),
        );
    }

    #[test]
    fn test_parse_function_call_with_uri_anchor() {
        assert_parses_to(
            "func_uri@456()",
            Expression::FunctionCall(Box::new(FunctionCall {
                name: "func_uri".to_string(),
                parameters: None,
                anchor: None,
                uri_anchor: Some(456),
                arguments: vec![],
            })),
        );
    }

    #[test]
    fn test_parse_function_call_all_optionals() {
        assert_parses_to(
            "uber_func<Param1, Param2>#789@101()",
            Expression::FunctionCall(Box::new(FunctionCall {
                name: "uber_func".to_string(),
                parameters: Some(vec!["Param1".to_string(), "Param2".to_string()]),
                anchor: Some(789),
                uri_anchor: Some(101),
                arguments: vec![],
            })),
        );
    }

    #[test]
    fn test_parse_function_call_with_simple_arguments() {
        assert_parses_to(
            "add($0, 100, 'test')",
            Expression::FunctionCall(Box::new(FunctionCall {
                name: "add".to_string(),
                parameters: None,
                anchor: None,
                uri_anchor: None,
                arguments: vec![
                    Expression::Reference(Reference(0)),
                    Expression::Literal(Literal::Integer(100)),
                    Expression::Literal(Literal::String("test".to_string())),
                ],
            })),
        );
    }

    #[test]
    fn test_parse_function_call_with_nested_function() {
        assert_parses_to(
            "outer_func(inner_func(), $1)",
            Expression::FunctionCall(Box::new(FunctionCall {
                name: "outer_func".to_string(),
                parameters: None,
                anchor: None,
                uri_anchor: None,
                arguments: vec![
                    Expression::FunctionCall(Box::new(FunctionCall {
                        name: "inner_func".to_string(),
                        parameters: None,
                        anchor: None,
                        uri_anchor: None,
                        arguments: vec![],
                    })),
                    Expression::Reference(Reference(1)),
                ],
            })),
        );
    }

    #[test]
    fn test_parse_function_call_funny_names() {
        assert_parses_to(
            "\"some weird outer func\"(\"an 3 even ! \\\\ weirder \\' \\\" [] () inner func\"(), $1)",
            Expression::FunctionCall(Box::new(FunctionCall {
                name: "some weird outer func".to_string(),
                parameters: None,
                anchor: None,
                uri_anchor: None,
                arguments: vec![
                    Expression::FunctionCall(Box::new(FunctionCall {
                        name: "an 3 even ! \\ weirder \' \" [] () inner func".to_string(),
                        parameters: None,
                        anchor: None,
                        uri_anchor: None,
                        arguments: vec![],
                    })),
                    Expression::Reference(Reference(1)),
                ],
            })),
        );
    }

    #[test]
    fn test_parse_empty_string_literal() {
        assert_parses_to("''", Expression::Literal(Literal::String("".to_string())));
    }
}
