use std::fmt;
use std::str::FromStr;

use thiserror::Error;

use super::{ParsePair, Rule, RuleIter, unwrap_single_pair};
use crate::extensions::simple::{self, ExtensionKind};
use crate::extensions::{InsertError, SimpleExtensions};
use crate::parser::structural::IndentedLine;

#[derive(Debug, Clone, Error)]
pub enum ExtensionParseError {
    #[error("Unexpected line, expected {0}")]
    UnexpectedLine(ExtensionParserState),
    #[error("Error adding extension: {0}")]
    ExtensionError(#[from] InsertError),
    #[error("Error parsing message: {0}")]
    Message(#[from] super::MessageParseError),
}

/// The state of the extension parser - tracking what section of extension
/// parsing we are in.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ExtensionParserState {
    // The extensions section, after parsing the 'Extensions:' header, before
    // parsing any subsection headers.
    Extensions,
    // The extension URIs section, after parsing the 'URIs:' subsection header,
    // and any URIs so far.
    ExtensionUris,
    // In a subsection, after parsing the subsection header, and any
    // declarations so far.
    ExtensionDeclarations(ExtensionKind),
}

impl fmt::Display for ExtensionParserState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExtensionParserState::Extensions => write!(f, "Subsection Header, e.g. 'URIs:'"),
            ExtensionParserState::ExtensionUris => write!(f, "Extension URIs"),
            ExtensionParserState::ExtensionDeclarations(kind) => {
                write!(f, "Extension Declaration for {kind}")
            }
        }
    }
}

/// The parser for the extension section of the Substrait file format.
///
/// This is responsible for parsing the extension section of the file, which
/// contains the extension URIs and declarations. Note that this parser does not
/// parse the header; otherwise, this is symmetric with the
/// SimpleExtensions::write method.
#[derive(Debug)]
pub struct ExtensionParser {
    state: ExtensionParserState,
    extensions: SimpleExtensions,
}

impl Default for ExtensionParser {
    fn default() -> Self {
        Self {
            state: ExtensionParserState::Extensions,
            extensions: SimpleExtensions::new(),
        }
    }
}

impl ExtensionParser {
    pub fn parse_line(&mut self, line: IndentedLine) -> Result<(), ExtensionParseError> {
        if line.1.is_empty() {
            // Blank lines are allowed between subsections, so if we see
            // one, we revert out of the subsection.
            self.state = ExtensionParserState::Extensions;
            return Ok(());
        }

        match self.state {
            ExtensionParserState::Extensions => self.parse_subsection(line),
            ExtensionParserState::ExtensionUris => self.parse_extension_uris(line),
            ExtensionParserState::ExtensionDeclarations(extension_kind) => {
                self.parse_declarations(line, extension_kind)
            }
        }
    }

    fn parse_subsection(&mut self, line: IndentedLine) -> Result<(), ExtensionParseError> {
        match line {
            IndentedLine(0, simple::EXTENSION_URIS_HEADER) => {
                self.state = ExtensionParserState::ExtensionUris;
                Ok(())
            }
            IndentedLine(0, simple::EXTENSION_FUNCTIONS_HEADER) => {
                self.state = ExtensionParserState::ExtensionDeclarations(ExtensionKind::Function);
                Ok(())
            }
            IndentedLine(0, simple::EXTENSION_TYPES_HEADER) => {
                self.state = ExtensionParserState::ExtensionDeclarations(ExtensionKind::Type);
                Ok(())
            }
            IndentedLine(0, simple::EXTENSION_TYPE_VARIATIONS_HEADER) => {
                self.state =
                    ExtensionParserState::ExtensionDeclarations(ExtensionKind::TypeVariation);
                Ok(())
            }
            _ => Err(ExtensionParseError::UnexpectedLine(self.state)),
        }
    }

    fn parse_extension_uris(&mut self, line: IndentedLine) -> Result<(), ExtensionParseError> {
        match line {
            IndentedLine(0, _s) => self.parse_subsection(line), // Pass the original line with 0 indent
            IndentedLine(1, s) => {
                let uri =
                    URIExtensionDeclaration::from_str(s).map_err(ExtensionParseError::Message)?;
                self.extensions.add_extension_uri(uri.uri, uri.anchor)?;
                Ok(())
            }
            _ => Err(ExtensionParseError::UnexpectedLine(self.state)),
        }
    }

    fn parse_declarations(
        &mut self,
        line: IndentedLine,
        extension_kind: ExtensionKind,
    ) -> Result<(), ExtensionParseError> {
        match line {
            IndentedLine(0, _s) => self.parse_subsection(line), // Pass the original line with 0 indent
            IndentedLine(1, s) => {
                let decl = SimpleExtensionDeclaration::from_str(s)?;
                self.extensions.add_extension(
                    extension_kind,
                    decl.uri_anchor,
                    decl.anchor,
                    decl.name,
                )?;
                Ok(())
            }
            _ => Err(ExtensionParseError::UnexpectedLine(self.state)),
        }
    }

    pub fn extensions(&self) -> &SimpleExtensions {
        &self.extensions
    }

    pub fn state(&self) -> ExtensionParserState {
        self.state
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct URIExtensionDeclaration {
    pub anchor: u32,
    pub uri: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SimpleExtensionDeclaration {
    pub anchor: u32,
    pub uri_anchor: u32,
    pub name: String,
}

impl ParsePair for URIExtensionDeclaration {
    fn rule() -> Rule {
        Rule::extension_uri_declaration
    }

    fn message() -> &'static str {
        "URIExtensionDeclaration"
    }

    fn parse_pair(pair: pest::iterators::Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());

        let mut iter = RuleIter::from(pair.into_inner());
        let anchor_pair = iter.pop(Rule::uri_anchor);
        let anchor = unwrap_single_pair(anchor_pair)
            .as_str()
            .parse::<u32>()
            .unwrap();
        let uri = iter.pop(Rule::uri).as_str().to_string();
        iter.done();

        URIExtensionDeclaration { anchor, uri }
    }
}

impl FromStr for URIExtensionDeclaration {
    type Err = super::MessageParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse_str(s)
    }
}

impl ParsePair for SimpleExtensionDeclaration {
    fn rule() -> Rule {
        Rule::simple_extension
    }

    fn message() -> &'static str {
        "SimpleExtensionDeclaration"
    }

    fn parse_pair(pair: pest::iterators::Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());
        let mut iter = RuleIter::from(pair.into_inner());
        let anchor_pair = iter.pop(Rule::anchor);
        let anchor = unwrap_single_pair(anchor_pair)
            .as_str()
            .parse::<u32>()
            .unwrap();
        let uri_anchor_pair = iter.pop(Rule::uri_anchor);
        let uri_anchor = unwrap_single_pair(uri_anchor_pair)
            .as_str()
            .parse::<u32>()
            .unwrap();
        let name_pair = iter.pop(Rule::name);
        let name = unwrap_single_pair(name_pair).as_str().to_string();
        iter.done();

        SimpleExtensionDeclaration {
            anchor,
            uri_anchor,
            name,
        }
    }
}

impl FromStr for SimpleExtensionDeclaration {
    type Err = super::MessageParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse_str(s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    #[test]
    fn test_parse_uri_extension_declaration() {
        let line = "@1: /my/uri1";
        let uri = URIExtensionDeclaration::parse_str(line).unwrap();
        assert_eq!(uri.anchor, 1);
        assert_eq!(uri.uri, "/my/uri1");
    }

    #[test]
    fn test_parse_simple_extension_declaration() {
        // Assumes a format like "@anchor: uri_anchor:name"
        let line = "#5@2: my_function_name";
        let decl = SimpleExtensionDeclaration::from_str(line).unwrap();
        assert_eq!(decl.anchor, 5);
        assert_eq!(decl.uri_anchor, 2);
        assert_eq!(decl.name, "my_function_name");

        // Test with a different name format, e.g. with underscores and numbers
        let line2 = "#10  @200: another_ext_123";
        let decl = SimpleExtensionDeclaration::from_str(line2).unwrap();
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

    #[test]
    fn test_extensions_round_trip_plan() {
        let input = r#"
=== Extensions
URIs:
  @  1: /uri/common
  @  2: /uri/specific_funcs
Functions:
  # 10 @  1: func_a
  # 11 @  2: func_b_special
Types:
  # 20 @  1: SomeType
Type Variations:
  # 30 @  2: VarX
"#
        .trim_start();

        // Parse the input using the structural parser
        let plan = Parser::parse(input).unwrap();

        // Verify the plan has the expected extensions
        assert_eq!(plan.extension_uris.len(), 2);
        assert_eq!(plan.extensions.len(), 4);

        // Convert the plan extensions back to SimpleExtensions
        let (extensions, _errors) =
            SimpleExtensions::from_extensions(&plan.extension_uris, &plan.extensions);

        // Convert back to string
        let output = extensions.to_string("  ");

        // The output should match the input
        assert_eq!(output, input);
    }
}
