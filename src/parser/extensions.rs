use std::fmt;
use std::str::FromStr;

use thiserror::Error;

use super::common::{MessageParseError, parse_typed, rules, unescape_string};
use crate::extensions::any::Any;
use crate::extensions::simple::{self, ExtensionKind};
use crate::extensions::{
    ExtensionArgs, ExtensionColumn, ExtensionRelationType, ExtensionValue, InsertError,
    RawExpression, SimpleExtensions,
};
use crate::parser::expressions::{parse_field_index_node, parse_name_node};
use crate::parser::structural::IndentedLine;

#[derive(Debug, Clone, Error)]
pub enum ExtensionParseError {
    #[error("Unexpected line, expected {0}")]
    UnexpectedLine(ExtensionParserState),
    #[error("Error adding extension: {0}")]
    ExtensionError(#[from] InsertError),
    #[error("Error parsing message: {0}")]
    Message(#[from] MessageParseError),
}

/// The state of the extension parser - tracking what section of extension
/// parsing we are in.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ExtensionParserState {
    Extensions,
    ExtensionUrns,
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
            IndentedLine(0, _s) => self.parse_subsection(line),
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
            IndentedLine(0, _s) => self.parse_subsection(line),
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

    #[cfg(test)]
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

fn parse_urn_extension_declaration_node(
    node: &rules::extension_urn_declaration<'_>,
) -> URNExtensionDeclaration {
    URNExtensionDeclaration {
        anchor: node
            .urn_anchor()
            .integer()
            .span
            .as_str()
            .parse::<u32>()
            .unwrap(),
        urn: node.urn().span.as_str().to_string(),
    }
}

fn parse_simple_extension_declaration_node(
    node: &rules::simple_extension<'_>,
) -> SimpleExtensionDeclaration {
    SimpleExtensionDeclaration {
        anchor: node
            .anchor()
            .integer()
            .span
            .as_str()
            .parse::<u32>()
            .unwrap(),
        urn_anchor: node
            .urn_anchor()
            .integer()
            .span
            .as_str()
            .parse::<u32>()
            .unwrap(),
        name: parse_name_node(node.name()).0,
    }
}

impl FromStr for URNExtensionDeclaration {
    type Err = MessageParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let typed =
            parse_typed::<rules::extension_urn_declaration<'_>>(s, "URNExtensionDeclaration")?;
        Ok(parse_urn_extension_declaration_node(&typed))
    }
}

impl FromStr for SimpleExtensionDeclaration {
    type Err = MessageParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let typed = parse_typed::<rules::simple_extension<'_>>(s, "SimpleExtensionDeclaration")?;
        Ok(parse_simple_extension_declaration_node(&typed))
    }
}

fn parse_extension_value_node(node: &rules::extension_argument<'_>) -> ExtensionValue {
    if let Some(reference) = node.reference() {
        let field_index = parse_field_index_node(reference);
        return ExtensionValue::Reference(field_index.0);
    }

    if let Some(literal) = node.literal() {
        if let Some(string_literal) = literal.string_literal() {
            return ExtensionValue::String(unescape_string(string_literal.span.as_str(), '\''));
        }
        if let Some(integer) = literal.integer() {
            return ExtensionValue::Integer(integer.span.as_str().parse::<i64>().unwrap());
        }
        if let Some(float) = literal.float() {
            return ExtensionValue::Float(float.span.as_str().parse::<f64>().unwrap());
        }
        if let Some(boolean) = literal.boolean() {
            return ExtensionValue::Boolean(boolean.span.as_str() == "true");
        }
        panic!("Unexpected literal value type in extension argument");
    }

    if let Some(expression) = node.expression() {
        return ExtensionValue::Expression(RawExpression::new(
            expression.span.as_str().to_string(),
        ));
    }

    panic!("Unexpected extension argument type");
}

fn parse_extension_column_node(node: &rules::extension_column<'_>) -> ExtensionColumn {
    if let Some(named_column) = node.named_column() {
        return ExtensionColumn::Named {
            name: parse_name_node(named_column.name()).0,
            type_spec: named_column.r#type().span.as_str().to_string(),
        };
    }

    if let Some(reference) = node.reference() {
        let field_index = parse_field_index_node(reference);
        return ExtensionColumn::Reference(field_index.0);
    }

    if let Some(expression) = node.expression() {
        return ExtensionColumn::Expression(RawExpression::new(
            expression.span.as_str().to_string(),
        ));
    }

    panic!("Unexpected extension column type");
}

/// Fully parsed extension invocation, including the user-supplied name and the
/// structured argument payload.
#[derive(Debug, Clone)]
pub struct ExtensionInvocation {
    pub name: String,
    pub args: ExtensionArgs,
}

fn parse_extension_invocation_node(node: &rules::extension_relation<'_>) -> ExtensionInvocation {
    let extension_name = node.extension_name();
    let relation_type =
        ExtensionRelationType::from_str(extension_name.extension_type().span.as_str())
            .expect("grammar guarantees known extension relation type");
    let name = parse_name_node(extension_name.name()).0;

    let mut args = ExtensionArgs::new(relation_type);

    if let Some(positional) = node.extension_arguments() {
        let (first, rest) = positional.extension_argument();
        args.add_positional_arg(parse_extension_value_node(first));
        for arg in rest {
            args.add_positional_arg(parse_extension_value_node(arg));
        }
    }

    if let Some((group_a, group_b)) = node.extension_named_arguments() {
        for group in [group_a, group_b].into_iter().flatten() {
            let (first, rest) = group.extension_named_argument();
            let mut named_items = Vec::with_capacity(rest.len() + 1);
            named_items.push(first);
            named_items.extend(rest);
            for named in named_items {
                let key = parse_name_node(named.name()).0;
                let value = parse_extension_value_node(named.extension_argument());
                args.add_named_arg(key, value);
            }
        }
    }

    if let Some(columns) = node.extension_columns() {
        let (first, rest) = columns.extension_column();
        args.add_output_column(parse_extension_column_node(first));
        for column in rest {
            args.add_output_column(parse_extension_column_node(column));
        }
    }

    ExtensionInvocation { name, args }
}

pub(crate) fn parse_extension_invocation(
    input: &str,
) -> Result<ExtensionInvocation, MessageParseError> {
    let typed = parse_typed::<rules::extension_relation<'_>>(input, "ExtensionInvocation")?;
    Ok(parse_extension_invocation_node(&typed))
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
        let urn = URNExtensionDeclaration::from_str(line).unwrap();
        assert_eq!(urn.anchor, 1);
        assert_eq!(urn.urn, "/my/urn1");
    }

    #[test]
    fn test_parse_simple_extension_declaration() {
        let line = "#5@2: my_function_name";
        let decl = SimpleExtensionDeclaration::from_str(line).unwrap();
        assert_eq!(decl.anchor, 5);
        assert_eq!(decl.urn_anchor, 2);
        assert_eq!(decl.name, "my_function_name");

        let line2 = "#10  @200: another_ext_123";
        let decl = SimpleExtensionDeclaration::from_str(line2).unwrap();
        assert_eq!(decl.anchor, 10);
        assert_eq!(decl.urn_anchor, 200);
        assert_eq!(decl.name, "another_ext_123");
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

        let plan = Parser::parse(input).unwrap();

        assert_eq!(plan.extension_urns.len(), 2);
        assert_eq!(plan.extensions.len(), 4);

        let (extensions, errors) =
            SimpleExtensions::from_extensions(&plan.extension_urns, &plan.extensions);

        assert!(errors.is_empty());
        let output = extensions.to_string("  ");
        assert_eq!(output, input);
    }
}
