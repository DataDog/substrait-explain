use std::fmt;
use std::str::FromStr;

use thiserror::Error;

use super::{ParsePair, Rule, RuleIter, unescape_string, unwrap_single_pair};
use crate::extensions::simple::{self, ExtensionKind};
use crate::extensions::{
    ExtensionArgs, ExtensionColumn, ExtensionRelationType, ExtensionValue, InsertError,
    RawExpression, SimpleExtensions,
};
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
    // The extension URNs section, after parsing the 'URNs:' subsection header,
    // and any URNs so far.
    ExtensionUrns,
    // In a subsection, after parsing the subsection header, and any
    // declarations so far.
    ExtensionDeclarations(ExtensionKind),
}

impl fmt::Display for ExtensionParserState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExtensionParserState::Extensions => write!(f, "Subsection Header, e.g. 'URNs:'"),
            ExtensionParserState::ExtensionUrns => write!(f, "Extension URNs"),
            ExtensionParserState::ExtensionDeclarations(kind) => {
                write!(f, "Extension Declaration for {kind}")
            }
        }
    }
}

/// The parser for the extension section of the Substrait file format.
///
/// This is responsible for parsing the extension section of the file, which
/// contains the extension URNs and declarations. Note that this parser does not
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
            ExtensionParserState::ExtensionUrns => self.parse_extension_urns(line),
            ExtensionParserState::ExtensionDeclarations(extension_kind) => {
                self.parse_declarations(line, extension_kind)
            }
        }
    }

    fn parse_subsection(&mut self, line: IndentedLine) -> Result<(), ExtensionParseError> {
        match line {
            IndentedLine(0, simple::EXTENSION_URNS_HEADER) => {
                self.state = ExtensionParserState::ExtensionUrns;
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

    fn parse_extension_urns(&mut self, line: IndentedLine) -> Result<(), ExtensionParseError> {
        match line {
            IndentedLine(0, _s) => self.parse_subsection(line), // Pass the original line with 0 indent
            IndentedLine(1, s) => {
                let urn =
                    URNExtensionDeclaration::from_str(s).map_err(ExtensionParseError::Message)?;
                self.extensions.add_extension_urn(urn.urn, urn.anchor)?;
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
                    decl.urn_anchor,
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
pub struct URNExtensionDeclaration {
    pub anchor: u32,
    pub urn: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SimpleExtensionDeclaration {
    pub anchor: u32,
    pub urn_anchor: u32,
    pub name: String,
}

impl ParsePair for URNExtensionDeclaration {
    fn rule() -> Rule {
        Rule::extension_urn_declaration
    }

    fn message() -> &'static str {
        "URNExtensionDeclaration"
    }

    fn parse_pair(pair: pest::iterators::Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());

        let mut iter = RuleIter::from(pair.into_inner());
        let anchor_pair = iter.pop(Rule::urn_anchor);
        let anchor = unwrap_single_pair(anchor_pair)
            .as_str()
            .parse::<u32>()
            .unwrap();
        let urn = iter.pop(Rule::urn).as_str().to_string();
        iter.done();

        URNExtensionDeclaration { anchor, urn }
    }
}

impl FromStr for URNExtensionDeclaration {
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
        let urn_anchor_pair = iter.pop(Rule::urn_anchor);
        let urn_anchor = unwrap_single_pair(urn_anchor_pair)
            .as_str()
            .parse::<u32>()
            .unwrap();
        let name_pair = iter.pop(Rule::name);
        let name = unwrap_single_pair(name_pair).as_str().to_string();
        iter.done();

        SimpleExtensionDeclaration {
            anchor,
            urn_anchor,
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

// Extension relation parsing implementations
// These were moved from extensions/registry.rs to maintain clean architecture

use crate::extensions::any::Any;
use crate::parser::expressions::{FieldIndex, Name};

impl ParsePair for ExtensionValue {
    fn rule() -> Rule {
        Rule::extension_argument
    }

    fn message() -> &'static str {
        "ExtensionValue"
    }

    fn parse_pair(pair: pest::iterators::Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());

        let inner = unwrap_single_pair(pair); // Extract the actual content

        match inner.as_rule() {
            Rule::enum_value => {
                // Strip leading '&' and store the identifier
                let s = inner.as_str().trim_start_matches('&').to_string();
                ExtensionValue::Enum(s)
            }
            Rule::reference => {
                // Reuse the existing FieldIndex parser, then extract the i32
                let field_index = FieldIndex::parse_pair(inner);
                ExtensionValue::Reference(field_index.0)
            }
            Rule::literal => {
                // Literal can contain integer, float, boolean, or string_literal
                let mut literal_inner = inner.into_inner();
                let value_pair = literal_inner.next().unwrap();
                match value_pair.as_rule() {
                    Rule::string_literal => ExtensionValue::String(unescape_string(value_pair)),
                    Rule::integer => {
                        let int_val = value_pair.as_str().parse::<i64>().unwrap();
                        ExtensionValue::Integer(int_val)
                    }
                    Rule::float => {
                        let float_val = value_pair.as_str().parse::<f64>().unwrap();
                        ExtensionValue::Float(float_val)
                    }
                    Rule::boolean => {
                        let bool_val = value_pair.as_str() == "true";
                        ExtensionValue::Boolean(bool_val)
                    }
                    _ => panic!("Unexpected literal value type: {:?}", value_pair.as_rule()),
                }
            }
            Rule::string_literal => ExtensionValue::String(unescape_string(inner)),
            Rule::integer => {
                // Direct integer (not wrapped in literal rule)
                let int_val = inner.as_str().parse::<i64>().unwrap();
                ExtensionValue::Integer(int_val)
            }
            Rule::float => {
                // Direct float (not wrapped in literal rule)
                let float_val = inner.as_str().parse::<f64>().unwrap();
                ExtensionValue::Float(float_val)
            }
            Rule::boolean => {
                // Direct boolean (not wrapped in literal rule)
                let bool_val = inner.as_str() == "true";
                ExtensionValue::Boolean(bool_val)
            }
            Rule::expression => {
                ExtensionValue::Expression(RawExpression::new(inner.as_str().to_string()))
            }
            _ => panic!("Unexpected extension argument type: {:?}", inner.as_rule()),
        }
    }
}

impl ParsePair for ExtensionColumn {
    fn rule() -> Rule {
        Rule::extension_column
    }

    fn message() -> &'static str {
        "ExtensionColumn"
    }

    fn parse_pair(pair: pest::iterators::Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());

        let inner = unwrap_single_pair(pair); // Extract the actual content

        match inner.as_rule() {
            Rule::named_column => {
                let mut iter = inner.into_inner();
                let name_pair = iter.next().unwrap(); // Grammar guarantees name exists
                let type_pair = iter.next().unwrap(); // Grammar guarantees type exists

                let name = Name::parse_pair(name_pair).0.to_string(); // Reuse existing Name parser
                let type_spec = type_pair.as_str().to_string(); // Types are complex, store as string for now

                ExtensionColumn::Named { name, type_spec }
            }
            Rule::reference => {
                // Reuse the existing FieldIndex parser, then extract the i32
                let field_index = FieldIndex::parse_pair(inner);
                ExtensionColumn::Reference(field_index.0)
            }
            Rule::expression => {
                ExtensionColumn::Expression(RawExpression::new(inner.as_str().to_string()))
            }
            _ => panic!("Unexpected extension column type: {:?}", inner.as_rule()),
        }
    }
}

/// Fully parsed extension invocation, including the user-supplied name and the
/// structured argument payload.
#[derive(Debug, Clone)]
pub struct ExtensionInvocation {
    pub name: String,
    pub args: ExtensionArgs,
}

impl ParsePair for ExtensionInvocation {
    fn rule() -> Rule {
        Rule::extension_relation
    }

    fn message() -> &'static str {
        "ExtensionInvocation"
    }

    fn parse_pair(pair: pest::iterators::Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());

        let mut iter = pair.into_inner();

        // Parse extension name to determine relation type and custom name
        let extension_name_pair = iter.next().unwrap(); // Grammar guarantees extension_name exists
        let full_extension_name = extension_name_pair.as_str();

        // Extract the relation type and custom name from the extension name
        // (e.g., "ExtensionLeaf:ParquetScan" -> "ExtensionLeaf" and "ParquetScan")
        let (relation_type_str, custom_name) = if full_extension_name.contains(':') {
            let parts: Vec<&str> = full_extension_name.splitn(2, ':').collect();
            (parts[0], parts[1].to_string())
        } else {
            (full_extension_name, "UnknownExtension".to_string())
        };

        let relation_type = ExtensionRelationType::from_str(relation_type_str).unwrap();
        let mut args = ExtensionArgs::new(relation_type);

        // Parse optional arguments
        let ext_arguments = iter.next().unwrap();
        match ext_arguments.as_rule() {
            Rule::arguments => {
                arguments_rule_parsing(ext_arguments, &mut args);
            }
            r => unreachable!("Unexpected rule in ExtensionArgs: {:?}", r),
        }

        // parse optional output columns
        let extension_columns = iter.next();
        if let Some(value) = extension_columns {
            match value.as_rule() {
                Rule::extension_columns => {
                    for col_pair in value.into_inner() {
                        if col_pair.as_rule() == Rule::extension_column {
                            let column = ExtensionColumn::parse_pair(col_pair);
                            args.output_columns.push(column);
                        }
                    }
                }
                r => unreachable!("Unexpected rule in ExtensionArgs: {:?}", r),
            }
        }

        ExtensionInvocation {
            name: custom_name,
            args,
        }
    }
}

/// Whether an advanced-extension line is an enhancement or an optimization.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AdvExtType {
    /// `+ Enh:Name[args]` — alters plan semantics, cannot be ignored
    Enhancement,
    /// `+ Opt:Name[args]` — hints, can be ignored by the executor
    Optimization,
}

/// A parsed `+ Enh:Name[args]` or `+ Opt:Name[args]` line.
#[derive(Debug, Clone)]
pub struct AdvExtInvocation {
    pub ext_type: AdvExtType,
    pub name: String,
    pub args: ExtensionArgs,
}

impl ParsePair for AdvExtInvocation {
    fn rule() -> Rule {
        Rule::adv_extension
    }

    fn message() -> &'static str {
        "AdvExtInvocation"
    }

    fn parse_pair(pair: pest::iterators::Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Self::rule());

        let mut iter = pair.into_inner();

        // First token: adv_ext_type ("Enh" or "Opt")
        let type_pair = iter.next().unwrap();
        let ext_type = match type_pair.as_str() {
            "Enh" => AdvExtType::Enhancement,
            "Opt" => AdvExtType::Optimization,
            other => unreachable!("Unexpected adv_ext_type: {other}"),
        };

        // Second token: name
        let name_pair = iter.next().unwrap();
        let name = Name::parse_pair(name_pair).0.to_string();

        // Remaining tokens: optional arguments (same structure as extension_relation)
        // Use Leaf as the relation_type placeholder — adv_extensions don't have children
        let mut args = ExtensionArgs::new(crate::extensions::ExtensionRelationType::Leaf);

        for inner_pair in iter {
            match inner_pair.as_rule() {
                Rule::arguments => {
                    arguments_rule_parsing(inner_pair, &mut args);
                }
                r => unreachable!("Unexpected rule in AdvExtInvocation args: {r:?}"),
            }
        }

        AdvExtInvocation {
            ext_type,
            name,
            args,
        }
    }
}

fn arguments_rule_parsing(inner_pair: pest::iterators::Pair<'_, Rule>, args: &mut ExtensionArgs) {
    for arg in inner_pair.into_inner() {
        match arg.as_rule() {
            Rule::extension_arguments => {
                // Parse positional arguments
                for arg_pair in arg.into_inner() {
                    if arg_pair.as_rule() == Rule::extension_argument {
                        args.positional.push(ExtensionValue::parse_pair(arg_pair));
                    }
                }
            }
            Rule::extension_named_arguments => {
                for arg_pair in arg.into_inner() {
                    if arg_pair.as_rule() == Rule::extension_named_argument {
                        let mut arg_iter = arg_pair.into_inner();
                        let name_p = arg_iter.next().unwrap();
                        let value_p = arg_iter.next().unwrap();
                        let key = Name::parse_pair(name_p).0.to_string();
                        let val = ExtensionValue::parse_pair(value_p);
                        args.named.insert(key, val);
                    }
                }
            }
            Rule::empty => {}
            r => unreachable!("Unexpected rule in extension args: {r:?}"),
        }
    }
}

impl ExtensionRelationType {
    /// Create appropriate relation structure from extension detail and children.
    /// This method handles the structural logic for creating different extension relation types.
    pub fn create_rel(
        self,
        detail: Option<Any>,
        children: Vec<Box<substrait::proto::Rel>>,
    ) -> Result<substrait::proto::Rel, String> {
        use substrait::proto::rel::RelType;
        use substrait::proto::{ExtensionLeafRel, ExtensionMultiRel, ExtensionSingleRel};

        // Validate child count matches relation type
        self.validate_child_count(children.len())?;

        let rel_type = match self {
            ExtensionRelationType::Leaf => RelType::ExtensionLeaf(ExtensionLeafRel {
                common: None,
                detail: detail.map(Into::into),
            }),
            ExtensionRelationType::Single => {
                let input = children.into_iter().next().map(|child| *child);
                RelType::ExtensionSingle(Box::new(ExtensionSingleRel {
                    common: None,
                    detail: detail.map(Into::into),
                    input: input.map(Box::new),
                }))
            }
            ExtensionRelationType::Multi => {
                let inputs = children.into_iter().map(|child| *child).collect();
                RelType::ExtensionMulti(ExtensionMultiRel {
                    common: None,
                    detail: detail.map(Into::into),
                    inputs,
                })
            }
        };

        Ok(substrait::proto::Rel {
            rel_type: Some(rel_type),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    #[test]
    fn test_parse_urn_extension_declaration() {
        let line = "@1: /my/urn1";
        let urn = URNExtensionDeclaration::parse_str(line).unwrap();
        assert_eq!(urn.anchor, 1);
        assert_eq!(urn.urn, "/my/urn1");
    }

    #[test]
    fn test_parse_simple_extension_declaration() {
        // Assumes a format like "@anchor: urn_anchor:name"
        let line = "#5@2: my_function_name";
        let decl = SimpleExtensionDeclaration::from_str(line).unwrap();
        assert_eq!(decl.anchor, 5);
        assert_eq!(decl.urn_anchor, 2);
        assert_eq!(decl.name, "my_function_name");

        // Test with a different name format, e.g. with underscores and numbers
        let line2 = "#10  @200: another_ext_123";
        let decl = SimpleExtensionDeclaration::from_str(line2).unwrap();
        assert_eq!(decl.anchor, 10);
        assert_eq!(decl.urn_anchor, 200);
        assert_eq!(decl.name, "another_ext_123");
    }

    #[test]
    fn test_parse_urn_extension_declaration_str() {
        let line = "@1: /my/urn1";
        let urn = URNExtensionDeclaration::parse_str(line).unwrap();
        assert_eq!(urn.anchor, 1);
        assert_eq!(urn.urn, "/my/urn1");
    }

    #[test]
    fn test_extensions_round_trip_plan() {
        let input = r#"
=== Extensions
URNs:
  @  1: /urn/common
  @  2: /urn/specific_funcs
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
        assert_eq!(plan.extension_urns.len(), 2);
        assert_eq!(plan.extensions.len(), 4);

        // Convert the plan extensions back to SimpleExtensions
        let (extensions, errors) =
            SimpleExtensions::from_extensions(&plan.extension_urns, &plan.extensions);

        assert!(errors.is_empty());
        // Convert back to string
        let output = extensions.to_string("  ");

        // The output should match the input
        assert_eq!(output, input);
    }
}
