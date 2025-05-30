use pest::Parser;
use pest_derive::Parser;

// Import the AST structures
pub mod ast;
use ast::*;

#[derive(Parser)]
#[grammar = "parser/expression_grammar.pest"] // Path relative to src
pub struct ExpressionParser;

// Function to parse an input string into your Expression AST
pub fn parse_expression_str(input: &str) -> Result<Expression, pest::error::Error<Rule>> {
    let pairs = ExpressionParser::parse(Rule::expression_complete, input)?;
    // `pairs` will contain a single pair matching `Rule::expression` if successful.
    // We need to build the AST from this pair.
    // We'll assume the first pair is the one we want if parsing `Rule::expression` directly.
    build_ast_from_expression(pairs.peek().unwrap())
}

// Helper function to build AST from an expression pair
fn build_ast_from_expression(
    pair: pest::iterators::Pair<Rule>,
) -> Result<Expression, pest::error::Error<Rule>> {
    println!("Building AST from pair: {:?}", pair);

    match pair.as_rule() {
        Rule::reference => {
            let inner = pair.into_inner().next().unwrap(); // Skips '$', gets to `integer`
            Ok(Expression::Reference(parse_reference(inner)))
        }
        Rule::literal => {
            let inner_pair = pair.into_inner().next().unwrap();
            match inner_pair.as_rule() {
                Rule::integer => Ok(Expression::Literal(Literal::Integer(
                    inner_pair.as_str().parse().unwrap(), // Handle parse error
                ))),
                Rule::string_literal => {
                    let str_content = inner_pair.as_str();
                    // Remove leading/trailing quotes. A more robust solution would handle escapes.
                    Ok(Expression::Literal(Literal::String(
                        str_content[1..str_content.len() - 1].to_string(),
                    )))
                }
                _ => unreachable!("Expected integer or string_literal in literal"),
            }
        }
        Rule::function_call => {
            let mut inner_pairs = pair.into_inner();
            let name = inner_pairs.next().unwrap().as_str().to_string();

            let mut parameters = None;
            let mut variant = None;
            let mut anchor = None;
            let mut uri_anchor = None;
            let mut arguments_pair = None;

            // Iterate through optional parts and the required argument_list
            for current_pair in inner_pairs {
                match current_pair.as_rule() {
                    Rule::parameters => {
                        let mut params_vec = Vec::new();
                        for param_pair in current_pair.into_inner() {
                            if param_pair.as_rule() == Rule::identifier {
                                params_vec.push(param_pair.as_str().to_string());
                            }
                        }
                        parameters = Some(params_vec);
                    }
                    Rule::variant => {
                        variant = Some(
                            current_pair
                                .into_inner()
                                .next()
                                .unwrap()
                                .as_str()
                                .to_string(),
                        );
                    }
                    Rule::anchor => {
                        anchor = Some(
                            current_pair
                                .into_inner()
                                .next()
                                .unwrap()
                                .as_str()
                                .parse()
                                .unwrap(),
                        );
                    }
                    Rule::uri_anchor => {
                        uri_anchor = Some(
                            current_pair
                                .into_inner()
                                .next()
                                .unwrap()
                                .as_str()
                                .parse()
                                .unwrap(),
                        );
                    }
                    Rule::argument_list => {
                        arguments_pair = Some(current_pair);
                    }
                    Rule::WHITESPACE => { /* skip */ }
                    _ => unreachable!(
                        "Unexpected rule in function_call: {:?}",
                        current_pair.as_rule()
                    ),
                }
            }

            let mut arguments = Vec::new();
            if let Some(args_p) = arguments_pair {
                for arg_expr_pair in args_p.into_inner() {
                    if arg_expr_pair.as_rule() == Rule::expression {
                        arguments.push(build_ast_from_expression(arg_expr_pair)?);
                    }
                }
            }

            Ok(Expression::FunctionCall(Box::new(FunctionCall {
                name,
                parameters,
                variant,
                anchor,
                uri_anchor,
                arguments,
            })))
        }
        _ => unreachable!(
            "Expression rule should have matched reference, literal, or function_call, found {:?}",
            pair.as_rule()
        ),
    }
}

fn parse_reference(pair: pest::iterators::Pair<Rule>) -> u32 {
    let inner_pair = pair.into_inner().next().unwrap(); // Skips '$', gets to `integer`
    inner_pair.as_str().parse().unwrap()
}

// Note: The Rule enum is automatically generated by pest based on your .pest grammar file.
// You'll see variants like Rule::identifier, Rule::integer, Rule::function_call, etc.

#[cfg(test)]
mod tests {
    use super::*; // Imports ExpressionParser, parse_expression_str, and ast types

    fn parse_exact(rule: Rule, input: &str) -> pest::iterators::Pair<Rule> {
        let mut pairs = ExpressionParser::parse(rule, input).unwrap();
        let pair = pairs.next().unwrap();
        // assert_eq!(pair.as_str(), input);
        assert_eq!(pairs.next(), None);
        pair
    }

    #[test]
    fn test_parse_reference() {
        let input = "$52";

        let pair = parse_exact(Rule::reference, input);
        assert_eq!(parse_reference(pair), 52);
    }

    fn assert_parses_to(input: &str, expected_expr: Expression) {
        match parse_expression_str(input) {
            Ok(parsed_expr) => assert_eq!(parsed_expr, expected_expr),
            Err(e) => panic!("Parsing failed for input '{}':\n{}", input, e),
        }
    }

    #[test]
    fn test_parse_field_reference() {
        assert_parses_to("$0", Expression::Reference(0));
        assert_parses_to("$123", Expression::Reference(123));
    }

    #[test]
    fn test_parse_integer_literal() {
        assert_parses_to("42", Expression::Literal(Literal::Integer(42)));
        assert_parses_to("0", Expression::Literal(Literal::Integer(0)));
    }

    #[test]
    fn test_parse_string_literal() {
        assert_parses_to(
            "'hello'",
            Expression::Literal(Literal::String("hello".to_string())),
        );
        assert_parses_to(
            "\"world\"",
            Expression::Literal(Literal::String("world".to_string())),
        );
        assert_parses_to(
            "'  spaced  '",
            Expression::Literal(Literal::String("  spaced  ".to_string())),
        );
    }

    #[test]
    fn test_parse_function_call_simple() {
        assert_parses_to(
            "my_func()",
            Expression::FunctionCall(Box::new(FunctionCall {
                name: "my_func".to_string(),
                parameters: None,
                variant: None,
                anchor: None,
                uri_anchor: None,
                arguments: vec![],
            })),
        );
    }

    #[test]
    fn test_parse_function_call_with_parameters() {
        assert_parses_to(
            "generic_func<T, U>()",
            Expression::FunctionCall(Box::new(FunctionCall {
                name: "generic_func".to_string(),
                parameters: Some(vec!["T".to_string(), "U".to_string()]),
                variant: None,
                anchor: None,
                uri_anchor: None,
                arguments: vec![],
            })),
        );
    }

    #[test]
    fn test_parse_function_call_with_variant() {
        assert_parses_to(
            "func_variant[special]()",
            Expression::FunctionCall(Box::new(FunctionCall {
                name: "func_variant".to_string(),
                parameters: None,
                variant: Some("special".to_string()),
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
                variant: None,
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
                variant: None,
                anchor: None,
                uri_anchor: Some(456),
                arguments: vec![],
            })),
        );
    }

    #[test]
    fn test_parse_function_call_all_optionals() {
        assert_parses_to(
            "uber_func<Param1, Param2>[VariantX]#789@101()",
            Expression::FunctionCall(Box::new(FunctionCall {
                name: "uber_func".to_string(),
                parameters: Some(vec!["Param1".to_string(), "Param2".to_string()]),
                variant: Some("VariantX".to_string()),
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
                variant: None,
                anchor: None,
                uri_anchor: None,
                arguments: vec![
                    Expression::Reference(0),
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
                variant: None,
                anchor: None,
                uri_anchor: None,
                arguments: vec![
                    Expression::FunctionCall(Box::new(FunctionCall {
                        name: "inner_func".to_string(),
                        parameters: None,
                        variant: None,
                        anchor: None,
                        uri_anchor: None,
                        arguments: vec![],
                    })),
                    Expression::Reference(1),
                ],
            })),
        );
    }

    #[test]
    fn test_parse_function_call_with_whitespace_and_mixed_optionals() {
        assert_parses_to(
            "   spaced_func  <  P1 , P2 >  #11   (  $5 , another  ( )   )  ",
            Expression::FunctionCall(Box::new(FunctionCall {
                name: "spaced_func".to_string(),
                parameters: Some(vec!["P1".to_string(), "P2".to_string()]),
                variant: None,
                anchor: Some(11),
                uri_anchor: None,
                arguments: vec![
                    Expression::Reference(5),
                    Expression::FunctionCall(Box::new(FunctionCall {
                        name: "another".to_string(),
                        parameters: None,
                        variant: None,
                        anchor: None,
                        uri_anchor: None,
                        arguments: vec![],
                    })),
                ],
            })),
        );
    }

    #[test]
    fn test_parse_empty_string_literal() {
        assert_parses_to("''", Expression::Literal(Literal::String("".to_string())));
        assert_parses_to("\"\"", Expression::Literal(Literal::String("".to_string())));
    }
}
