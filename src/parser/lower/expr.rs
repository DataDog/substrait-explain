//! Expression lowering helpers.

use substrait::proto::expression::field_reference::ReferenceType;
use substrait::proto::expression::if_then::IfClause;
use substrait::proto::expression::literal::LiteralType;
use substrait::proto::expression::{
    FieldReference, IfThen, ReferenceSegment, RexType, ScalarFunction, reference_segment,
};
use substrait::proto::function_argument::ArgType;
use substrait::proto::{Expression, FunctionArgument};

use super::types::resolve_anchor;
use super::{Lower, LowerCtx};
use crate::extensions::simple::ExtensionKind;
use crate::parser::ast;
use crate::parser::errors::ParseError;

pub(crate) fn lower_arg_as_expression(
    ctx: &LowerCtx<'_>,
    arg: &ast::Arg,
    message: &'static str,
) -> Result<Expression, ParseError> {
    match arg {
        ast::Arg::Expr(expr) => expr.lower(ctx, message),
        _ => ctx.invalid(
            message,
            format!("expected expression argument, got '{arg}'"),
        ),
    }
}

pub(crate) fn fetch_value_expression(
    ctx: &LowerCtx<'_>,
    value: &ast::Arg,
    field: &'static str,
) -> Result<Expression, ParseError> {
    match value {
        ast::Arg::Expr(ast::Expr::Literal(ast::Literal {
            value: ast::LiteralValue::Integer(number),
            typ: None,
        })) => {
            // Keep integer limit/offset as direct I64 literals.
            if *number < 0 {
                return ctx.invalid(
                    "Fetch",
                    format!("Fetch {field} must be non-negative, got: {number}"),
                );
            }
            Ok(i64_literal_expr(*number))
        }
        ast::Arg::Expr(expr) => expr.lower(ctx, "Fetch"),
        _ => ctx.invalid(
            "Fetch",
            format!("Fetch {field} must be an expression, got '{value}'"),
        ),
    }
}

pub(crate) fn i64_literal_expr(value: i64) -> Expression {
    Expression {
        rex_type: Some(RexType::Literal(substrait::proto::expression::Literal {
            nullable: false,
            type_variation_reference: 0,
            literal_type: Some(LiteralType::I64(value)),
        })),
    }
}

pub(crate) fn field_reference(field: i32) -> FieldReference {
    FieldReference {
        reference_type: Some(ReferenceType::DirectReference(ReferenceSegment {
            reference_type: Some(reference_segment::ReferenceType::StructField(Box::new(
                reference_segment::StructField { field, child: None },
            ))),
        })),
        root_type: None,
    }
}

pub(crate) fn field_ref_expression(field: i32) -> Expression {
    Expression {
        rex_type: Some(RexType::Selection(Box::new(field_reference(field)))),
    }
}

impl Lower for ast::Expr {
    type Output = Expression;

    fn lower(&self, ctx: &LowerCtx<'_>, message: &'static str) -> Result<Self::Output, ParseError> {
        let rex_type = match self {
            ast::Expr::FieldRef(index) => RexType::Selection(Box::new(field_reference(*index))),
            ast::Expr::Literal(literal) => RexType::Literal(literal.lower(ctx, message)?),
            ast::Expr::FunctionCall(call) => RexType::ScalarFunction(call.lower(ctx, message)?),
            ast::Expr::IfThen(if_then) => RexType::IfThen(Box::new(if_then.lower(ctx, message)?)),
            ast::Expr::Identifier(name) => {
                // Bare identifiers are valid for names in the AST, but not as
                // scalar expressions in relational operators.
                return ctx.invalid(message, format!("unexpected bare identifier '{name}'"));
            }
            ast::Expr::DottedName(names) => {
                return ctx.invalid(
                    message,
                    format!("unexpected dotted name '{}'", names.join(".")),
                );
            }
        };

        Ok(Expression {
            rex_type: Some(rex_type),
        })
    }
}

impl Lower for ast::IfThenExpr {
    type Output = IfThen;

    fn lower(&self, ctx: &LowerCtx<'_>, message: &'static str) -> Result<Self::Output, ParseError> {
        if self.clauses.is_empty() {
            return ctx.invalid(message, "if_then requires at least one clause");
        }

        let mut clauses = Vec::with_capacity(self.clauses.len());
        for (condition, result) in &self.clauses {
            clauses.push(IfClause {
                r#if: Some(condition.lower(ctx, message)?),
                then: Some(result.lower(ctx, message)?),
            });
        }

        Ok(IfThen {
            ifs: clauses,
            r#else: Some(Box::new(self.else_expr.lower(ctx, message)?)),
        })
    }
}

impl Lower for ast::FunctionCall {
    type Output = ScalarFunction;

    fn lower(&self, ctx: &LowerCtx<'_>, message: &'static str) -> Result<Self::Output, ParseError> {
        let function_reference = resolve_anchor(
            ctx,
            ExtensionKind::Function,
            self.anchor,
            &self.name,
            message,
        )?;

        let mut arguments = Vec::with_capacity(self.args.len());
        for arg in &self.args {
            arguments.push(FunctionArgument {
                arg_type: Some(ArgType::Value(arg.lower(ctx, message)?)),
            });
        }

        let output_type = match &self.output_type {
            Some(t) => Some(t.lower(ctx, message)?),
            None => None,
        };

        Ok(ScalarFunction {
            function_reference,
            arguments,
            options: vec![],
            output_type,
            #[allow(deprecated)]
            args: vec![],
        })
    }
}
