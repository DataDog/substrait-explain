//! AST -> protobuf lowering.
//!
//! Relation lowering stays explicit (per relation type), while leaf AST nodes
//! (`Expr`, `Literal`, `TypeExpr`) use the `Lower` trait for uniform conversion.

mod expr;
mod literals;
mod relations;
mod types;
mod validate;

use substrait::proto::Rel;

use crate::extensions::{ExtensionRegistry, SimpleExtensions};
use crate::parser::ast;
use crate::parser::errors::{MessageParseError, ParseContext, ParseError};

pub(crate) trait Lower {
    type Output;

    fn lower(&self, ctx: &LowerCtx<'_>, message: &'static str) -> Result<Self::Output, ParseError>;
}

pub(crate) struct LowerCtx<'a> {
    pub(crate) extensions: &'a SimpleExtensions,
    pub(crate) registry: &'a ExtensionRegistry,
    pub(crate) line: &'a str,
    pub(crate) line_no: i64,
}

impl<'a> LowerCtx<'a> {
    pub(crate) fn new(
        extensions: &'a SimpleExtensions,
        registry: &'a ExtensionRegistry,
        line: &'a str,
        line_no: i64,
    ) -> Self {
        Self {
            extensions,
            registry,
            line,
            line_no,
        }
    }

    pub(crate) fn parse_context(&self) -> ParseContext {
        // ParseContext owns line text, so errors can be surfaced without
        // borrowing constraints.
        ParseContext::new(self.line_no, self.line.to_string())
    }

    pub(crate) fn invalid<T>(
        &self,
        message: &'static str,
        description: impl ToString,
    ) -> Result<T, ParseError> {
        Err(ParseError::Plan(
            self.parse_context(),
            MessageParseError::invalid(message, description),
        ))
    }
}

#[allow(clippy::vec_box)]
pub fn lower_relation(
    extensions: &SimpleExtensions,
    registry: &ExtensionRegistry,
    relation: &ast::Relation,
    line: &str,
    line_no: i64,
    child_relations: Vec<Box<Rel>>,
    input_field_count: usize,
) -> Result<Rel, ParseError> {
    let ctx = LowerCtx::new(extensions, registry, line, line_no);

    if relation.args.invalid_order {
        return ctx.invalid(
            "relation_arguments",
            "named arguments must follow positional arguments",
        );
    }

    match &relation.name {
        ast::RelationName::Standard(name) => {
            relations::lower_standard(&ctx, relation, name, child_relations, input_field_count)
        }
        ast::RelationName::Extension(kind, name) => {
            relations::lower_extension(&ctx, relation, *kind, name, child_relations)
        }
    }
}
