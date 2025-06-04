use super::{
    ExpressionParser, ParsePair, ParsePairStr, Rule, structural::ParseError, unwrap_single_pair,
};
use crate::structure::{SimpleExtensionDeclaration, URIExtensionDeclaration};

use pest::Parser;

impl ParsePair for URIExtensionDeclaration {
    fn rule() -> Rule {
        Rule::extension_uri_declaration
    }

    fn parse(pair: pest::iterators::Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());

        let mut pairs = pair.into_inner();
        let anchor = pairs.next().unwrap();
        let anchor_int = unwrap_single_pair(anchor).as_str().parse::<u32>().unwrap();
        let uri = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);

        URIExtensionDeclaration {
            anchor: anchor_int,
            uri: uri.as_str().to_string(),
        }
    }
}

impl ParsePairStr for URIExtensionDeclaration {}

pub fn parse_extension_uri_declaration(line: &str) -> Result<URIExtensionDeclaration, ParseError> {
    let mut pairs = ExpressionParser::parse(URIExtensionDeclaration::rule(), line).unwrap();
    let pair = unwrap_single_pair(pairs.next().unwrap());
    Ok(URIExtensionDeclaration::parse(pair))
}

impl ParsePair for SimpleExtensionDeclaration {
    fn rule() -> Rule {
        Rule::simple_extension
    }

    fn parse(pair: pest::iterators::Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());
        let mut pairs = pair.into_inner();
        let anchor = pairs.next().unwrap();
        let anchor_int = unwrap_single_pair(anchor).as_str().parse::<u32>().unwrap();
        let uri_anchor = pairs.next().unwrap();
        let uri_anchor_int = unwrap_single_pair(uri_anchor)
            .as_str()
            .parse::<u32>()
            .unwrap();
        let name = pairs.next().unwrap();
        let name_str = unwrap_single_pair(name).as_str();

        SimpleExtensionDeclaration {
            anchor: anchor_int,
            uri_anchor: uri_anchor_int,
            name: name_str.to_string(),
        }
    }
}

pub fn parse_simple_extension_declaration(
    line: &str,
) -> Result<SimpleExtensionDeclaration, ParseError> {
    let mut pairs = ExpressionParser::parse(SimpleExtensionDeclaration::rule(), line).unwrap();

    let pair = pairs.next().unwrap();
    dbg!(&pair);
    Ok(SimpleExtensionDeclaration::parse(pair))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_uri_extension_declaration() {
        let line = "@1: /my/uri1";
        let uri = parse_extension_uri_declaration(line).unwrap();
        assert_eq!(uri.anchor, 1);
        assert_eq!(uri.uri, "/my/uri1");
    }

    #[test]
    fn test_parse_simple_extension_declaration() {
        // Assumes a format like "@anchor: uri_anchor:name"
        let line = "#5@2: my_function_name";
        let decl = parse_simple_extension_declaration(line).unwrap();
        assert_eq!(decl.anchor, 5);
        assert_eq!(decl.uri_anchor, 2);
        assert_eq!(decl.name, "my_function_name");

        // Test with a different name format, e.g. with underscores and numbers
        let line2 = "#10  @200: another_ext_123";
        let decl = parse_simple_extension_declaration(line2).unwrap();
        assert_eq!(decl.anchor, 10);
        assert_eq!(decl.uri_anchor, 200);
        assert_eq!(decl.name, "another_ext_123");
    }

    #[test]
    fn test_parse_uri_extension_declaration_str() {
        let line = "@1: /my/uri1";
        let uri = URIExtensionDeclaration::parse_str(line).unwrap();
        assert_eq!(uri.anchor, 1);
        assert_eq!(uri.uri, "/my/uri1");
    }
}
