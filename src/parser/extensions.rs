use std::fmt;

use thiserror::Error;

use crate::extensions::any::Any;
use crate::extensions::simple::{self, ExtensionKind};
use crate::extensions::{
    ExtensionArgs, ExtensionColumn, ExtensionRegistry, ExtensionRelationType, ExtensionValue,
    InsertError, RawExpression, SimpleExtensions,
};
use crate::parser::errors::{MessageParseError, ParseContext, ParseError};
use crate::parser::structural::IndentedLine;
use crate::parser::{ast, lalrpop_line};

#[derive(Debug, Clone, Error)]
pub enum ExtensionParseError {
    #[error("Unexpected line, expected {0}")]
    UnexpectedLine(ExtensionParserState),
    #[error("Error adding extension: {0}")]
    ExtensionError(#[from] InsertError),
    #[error("Error parsing message: {0}")]
    Message(#[from] MessageParseError),
}

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
    pub fn parse_line(&mut self, line: IndentedLine<'_>) -> Result<(), ExtensionParseError> {
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

    fn parse_subsection(&mut self, line: IndentedLine<'_>) -> Result<(), ExtensionParseError> {
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

    fn parse_extension_urns(&mut self, line: IndentedLine<'_>) -> Result<(), ExtensionParseError> {
        match line {
            IndentedLine(0, _s) => self.parse_subsection(line),
            IndentedLine(1, s) => {
                let urn = parse_urn_declaration(s)?;
                self.extensions.add_extension_urn(urn.urn, urn.anchor)?;
                Ok(())
            }
            _ => Err(ExtensionParseError::UnexpectedLine(self.state)),
        }
    }

    fn parse_declarations(
        &mut self,
        line: IndentedLine<'_>,
        extension_kind: ExtensionKind,
    ) -> Result<(), ExtensionParseError> {
        match line {
            IndentedLine(0, _s) => self.parse_subsection(line),
            IndentedLine(1, s) => {
                let decl = parse_simple_declaration(s)?;
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

fn parse_urn_declaration(input: &str) -> Result<ast::ExtensionUrnDeclaration, MessageParseError> {
    let parsed = lalrpop_line::parse_extension_urn_declaration(input).map_err(|e| {
        MessageParseError::syntax("URNExtensionDeclaration", e.describe_with_line(input))
    })?;
    if parsed.urn.trim().is_empty() {
        return Err(MessageParseError::invalid(
            "URNExtensionDeclaration",
            "URN cannot be empty",
        ));
    }
    Ok(parsed)
}

fn parse_simple_declaration(input: &str) -> Result<ast::ExtensionDeclaration, MessageParseError> {
    let parsed = lalrpop_line::parse_extension_declaration(input).map_err(|e| {
        MessageParseError::syntax("SimpleExtensionDeclaration", e.describe_with_line(input))
    })?;
    if parsed.name.trim().is_empty() {
        return Err(MessageParseError::invalid(
            "SimpleExtensionDeclaration",
            "name cannot be empty",
        ));
    }
    Ok(parsed)
}

pub fn resolve_extension_detail(
    registry: &ExtensionRegistry,
    line_no: i64,
    line: &str,
    extension_name: &str,
    extension_args: &ExtensionArgs,
) -> Result<Option<Any>, ParseError> {
    let detail = registry.parse_extension(extension_name, extension_args);

    match detail {
        Ok(any) => Ok(Some(any)),
        Err(crate::extensions::registry::ExtensionError::ExtensionNotFound(_)) => {
            Err(ParseError::UnregisteredExtension {
                name: extension_name.to_string(),
                context: ParseContext::new(line_no, line.to_string()),
            })
        }
        Err(err) => Err(ParseError::ExtensionDetail(
            ParseContext::new(line_no, line.to_string()),
            err,
        )),
    }
}

pub fn extension_value_from_arg(arg: &ast::Arg) -> Result<ExtensionValue, MessageParseError> {
    match arg {
        ast::Arg::Expr(ast::Expr::FieldRef(idx)) => Ok(ExtensionValue::Reference(*idx)),
        ast::Arg::Expr(ast::Expr::Literal(lit)) => match (&lit.value, &lit.typ) {
            (ast::LiteralValue::String(value), None) => Ok(ExtensionValue::String(value.clone())),
            (ast::LiteralValue::Integer(value), None) => Ok(ExtensionValue::Integer(*value)),
            (ast::LiteralValue::Float(value), None) => Ok(ExtensionValue::Float(*value)),
            (ast::LiteralValue::Boolean(value), None) => Ok(ExtensionValue::Boolean(*value)),
            _ => Ok(ExtensionValue::Expression(RawExpression::new(
                lit.to_string(),
            ))),
        },
        ast::Arg::Expr(expr) => Ok(ExtensionValue::Expression(RawExpression::new(
            expr.to_string(),
        ))),
        ast::Arg::NamedColumn(_, _)
        | ast::Arg::Enum(_)
        | ast::Arg::Tuple(_)
        | ast::Arg::Wildcard => Ok(ExtensionValue::Expression(RawExpression::new(
            arg.to_string(),
        ))),
    }
}

pub fn extension_column_from_arg(arg: &ast::Arg) -> Result<ExtensionColumn, MessageParseError> {
    match arg {
        ast::Arg::NamedColumn(name, typ) => Ok(ExtensionColumn::Named {
            name: name.clone(),
            type_spec: typ.to_string(),
        }),
        ast::Arg::Expr(ast::Expr::FieldRef(idx)) => Ok(ExtensionColumn::Reference(*idx)),
        ast::Arg::Expr(expr) => Ok(ExtensionColumn::Expression(RawExpression::new(
            expr.to_string(),
        ))),
        ast::Arg::Enum(_) | ast::Arg::Tuple(_) | ast::Arg::Wildcard => Ok(
            ExtensionColumn::Expression(RawExpression::new(arg.to_string())),
        ),
    }
}

impl ExtensionRelationType {
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
        let urn = parse_urn_declaration(line).unwrap();
        assert_eq!(urn.anchor, 1);
        assert_eq!(urn.urn, "/my/urn1");
    }

    #[test]
    fn test_parse_simple_extension_declaration() {
        let line = "#5@2: my_function_name";
        let decl = parse_simple_declaration(line).unwrap();
        assert_eq!(decl.anchor, 5);
        assert_eq!(decl.urn_anchor, 2);
        assert_eq!(decl.name, "my_function_name");

        let line2 = "#10  @200: another_ext_123";
        let decl = parse_simple_declaration(line2).unwrap();
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
