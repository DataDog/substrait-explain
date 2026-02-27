//! Core typed-node lowering for Substrait expressions.
//!
//! Relation parsing delegates expression fragments to this module. The same
//! lowering path is also exposed via `ParseFragment` impls for expression-level
//! protobuf messages.

use chrono::{DateTime, NaiveDate, NaiveDateTime, NaiveTime};
use pest_typed::Spanned;
use substrait::proto::aggregate_rel::Measure;
use substrait::proto::expression::if_then::IfClause;
use substrait::proto::expression::literal::LiteralType;
use substrait::proto::expression::{FieldReference, IfThen, Literal, RexType, ScalarFunction};
use substrait::proto::function_argument::ArgType;
use substrait::proto::r#type::{Fp64, I64, Kind, Nullability};
use substrait::proto::{AggregateFunction, Expression, FunctionArgument, Type};

use crate::extensions::SimpleExtensions;
use crate::extensions::simple::ExtensionKind;
use crate::parser::ParseFragment;
use crate::parser::common::{MessageParseError, parse_typed, rules, unescape_string};
use crate::parser::convert::{Anchor, AnchorKind, Lower, Name, ParseCtx, UrnAnchor};
use crate::parser::types::get_and_validate_anchor;

fn to_int_literal(
    value: &rules::integer<'_>,
    typ: Option<Type>,
) -> Result<Literal, MessageParseError> {
    let parsed_value: i64 = value.span.as_str().parse().unwrap();

    const DEFAULT_KIND: Kind = Kind::I64(I64 {
        type_variation_reference: 0,
        nullability: Nullability::Required as i32,
    });

    let kind = typ.and_then(|t| t.kind).unwrap_or(DEFAULT_KIND);

    let (lit, nullability, tvar) = match &kind {
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
            return Err(MessageParseError::invalid(
                "int_literal_type",
                value.span(),
                format!("Invalid type for integer literal: {k:?}"),
            ));
        }
    };

    Ok(Literal {
        literal_type: Some(lit),
        nullable: nullability != Nullability::Required as i32,
        type_variation_reference: tvar,
    })
}

fn to_float_literal(
    value: &rules::float<'_>,
    typ: Option<Type>,
) -> Result<Literal, MessageParseError> {
    let parsed_value: f64 = value.span.as_str().parse().unwrap();

    const DEFAULT_KIND: Kind = Kind::Fp64(Fp64 {
        type_variation_reference: 0,
        nullability: Nullability::Required as i32,
    });

    let kind = typ.and_then(|t| t.kind).unwrap_or(DEFAULT_KIND);

    let (lit, nullability, tvar) = match &kind {
        Kind::Fp32(f) => (
            LiteralType::Fp32(parsed_value as f32),
            f.nullability,
            f.type_variation_reference,
        ),
        Kind::Fp64(f) => (
            LiteralType::Fp64(parsed_value),
            f.nullability,
            f.type_variation_reference,
        ),
        k => {
            return Err(MessageParseError::invalid(
                "float_literal_type",
                value.span(),
                format!("Invalid type for float literal: {k:?}"),
            ));
        }
    };

    Ok(Literal {
        literal_type: Some(lit),
        nullable: nullability != Nullability::Required as i32,
        type_variation_reference: tvar,
    })
}

fn to_boolean_literal(value: &rules::boolean<'_>) -> Literal {
    let parsed_value: bool = value.span.as_str().parse().unwrap();

    Literal {
        literal_type: Some(LiteralType::Boolean(parsed_value)),
        nullable: false,
        type_variation_reference: 0,
    }
}

fn to_string_literal(
    value: &rules::string_literal<'_>,
    typ: Option<Type>,
) -> Result<Literal, MessageParseError> {
    let string_value = unescape_string(value.span.as_str(), '\'');

    let Some(typ) = typ else {
        return Ok(Literal {
            literal_type: Some(LiteralType::String(string_value)),
            nullable: false,
            type_variation_reference: 0,
        });
    };

    let Some(kind) = typ.kind else {
        return Ok(Literal {
            literal_type: Some(LiteralType::String(string_value)),
            nullable: false,
            type_variation_reference: 0,
        });
    };

    match &kind {
        Kind::Date(d) => {
            let date_days = parse_date_to_days(&string_value, value.span())?;
            Ok(Literal {
                literal_type: Some(LiteralType::Date(date_days)),
                nullable: d.nullability != Nullability::Required as i32,
                type_variation_reference: d.type_variation_reference,
            })
        }
        Kind::Time(t) => {
            let time_microseconds = parse_time_to_microseconds(&string_value, value.span())?;
            Ok(Literal {
                literal_type: Some(LiteralType::Time(time_microseconds)),
                nullable: t.nullability != Nullability::Required as i32,
                type_variation_reference: t.type_variation_reference,
            })
        }
        #[allow(deprecated)]
        Kind::Timestamp(ts) => {
            let timestamp_microseconds =
                parse_timestamp_to_microseconds(&string_value, value.span())?;
            Ok(Literal {
                literal_type: Some(LiteralType::Timestamp(timestamp_microseconds)),
                nullable: ts.nullability != Nullability::Required as i32,
                type_variation_reference: ts.type_variation_reference,
            })
        }
        _ => Ok(Literal {
            literal_type: Some(LiteralType::String(string_value)),
            nullable: false,
            type_variation_reference: 0,
        }),
    }
}

/// Parse a date string using chrono to days since Unix epoch
fn parse_date_to_days(
    date_str: &str,
    span: pest_typed::Span<'_>,
) -> Result<i32, MessageParseError> {
    let formats = ["%Y-%m-%d", "%Y/%m/%d"];

    for format in &formats {
        if let Ok(date) = NaiveDate::parse_from_str(date_str, format) {
            let epoch = NaiveDate::from_ymd_opt(1970, 1, 1).unwrap();
            let days = date.signed_duration_since(epoch).num_days();
            return Ok(days as i32);
        }
    }

    Err(MessageParseError::invalid(
        "date_parse_format",
        span,
        format!("Invalid date format: '{date_str}'. Expected YYYY-MM-DD or YYYY/MM/DD"),
    ))
}

/// Parse a time string using chrono to microseconds since midnight
fn parse_time_to_microseconds(
    time_str: &str,
    span: pest_typed::Span<'_>,
) -> Result<i64, MessageParseError> {
    let formats = ["%H:%M:%S%.f", "%H:%M:%S"];

    for format in &formats {
        if let Ok(time) = NaiveTime::parse_from_str(time_str, format) {
            let midnight = NaiveTime::from_hms_opt(0, 0, 0).unwrap();
            let duration = time.signed_duration_since(midnight);
            return Ok(duration.num_microseconds().unwrap_or(0));
        }
    }

    Err(MessageParseError::invalid(
        "time_parse_format",
        span,
        format!("Invalid time format: '{time_str}'. Expected HH:MM:SS or HH:MM:SS.fff"),
    ))
}

/// Parse a timestamp string using chrono to microseconds since Unix epoch
fn parse_timestamp_to_microseconds(
    timestamp_str: &str,
    span: pest_typed::Span<'_>,
) -> Result<i64, MessageParseError> {
    let formats = [
        "%Y-%m-%dT%H:%M:%S%.f",
        "%Y-%m-%dT%H:%M:%S",
        "%Y-%m-%d %H:%M:%S%.f",
        "%Y-%m-%d %H:%M:%S",
        "%Y/%m/%dT%H:%M:%S%.f",
        "%Y/%m/%dT%H:%M:%S",
        "%Y/%m/%d %H:%M:%S%.f",
        "%Y/%m/%d %H:%M:%S",
    ];

    for format in &formats {
        if let Ok(datetime) = NaiveDateTime::parse_from_str(timestamp_str, format) {
            let epoch = DateTime::from_timestamp(0, 0).unwrap().naive_utc();
            let duration = datetime.signed_duration_since(epoch);
            return Ok(duration.num_microseconds().unwrap_or(0));
        }
    }

    Err(MessageParseError::invalid(
        "timestamp_parse_format",
        span,
        format!(
            "Invalid timestamp format: '{timestamp_str}'. Expected YYYY-MM-DDTHH:MM:SS or YYYY-MM-DD HH:MM:SS"
        ),
    ))
}

impl Lower for rules::literal<'_> {
    type Output = Literal;

    fn lower(&self, cx: &ParseCtx<'_>) -> Result<Self::Output, MessageParseError> {
        let typ = self.r#type().map(|t| t.lower(cx)).transpose()?;

        if let Some(integer) = self.integer() {
            return to_int_literal(integer, typ);
        }
        if let Some(float) = self.float() {
            return to_float_literal(float, typ);
        }
        if let Some(boolean) = self.boolean() {
            return Ok(to_boolean_literal(boolean));
        }
        if let Some(string_literal) = self.string_literal() {
            return to_string_literal(string_literal, typ);
        }

        // Grammar guarantees literal value is one of integer, float, boolean, string_literal.
        unreachable!("literal must be integer, float, boolean, or string_literal")
    }
}

impl Lower for rules::function_call<'_> {
    type Output = ScalarFunction;

    fn lower(&self, cx: &ParseCtx<'_>) -> Result<Self::Output, MessageParseError> {
        let span = self.span();
        let name = Name::from(self.name());

        let anchor = self
            .anchor()
            .map(|a| Anchor::parse(a, AnchorKind::Function))
            .transpose()?;

        // TODO: Handle urn_anchor and validate that it matches anchor where relevant.
        let _urn_anchor = self.urn_anchor().map(UrnAnchor::try_from).transpose()?;

        let mut arguments = Vec::new();
        if let Some((first, rest)) = self.argument_list().expression() {
            let mut values = Vec::with_capacity(rest.len() + 1);
            values.push(first);
            values.extend(rest);
            for expression in values {
                arguments.push(FunctionArgument {
                    arg_type: Some(ArgType::Value(expression.lower(cx)?)),
                });
            }
        }

        let output_type = self.r#type().map(|t| t.lower(cx)).transpose()?;

        let anchor = get_and_validate_anchor(
            cx.extensions,
            ExtensionKind::Function,
            anchor.map(u32::from),
            &name.0,
            span,
        )?;

        Ok(ScalarFunction {
            function_reference: anchor,
            arguments,
            options: vec![],
            output_type,
            #[allow(deprecated)]
            args: vec![],
        })
    }
}

impl Lower for rules::expression<'_> {
    type Output = Expression;

    fn lower(&self, cx: &ParseCtx<'_>) -> Result<Self::Output, MessageParseError> {
        if let Some(literal) = self.literal() {
            return Ok(Expression {
                rex_type: Some(RexType::Literal(literal.lower(cx)?)),
            });
        }
        if let Some(function_call) = self.function_call() {
            return Ok(Expression {
                rex_type: Some(RexType::ScalarFunction(function_call.lower(cx)?)),
            });
        }
        if let Some(reference) = self.reference() {
            return Ok(Expression {
                rex_type: Some(RexType::Selection(Box::new(FieldReference::try_from(
                    reference,
                )?))),
            });
        }
        if let Some(if_then) = self.if_then() {
            return Ok(Expression {
                rex_type: Some(RexType::IfThen(Box::new(if_then.lower(cx)?))),
            });
        }

        // Grammar guarantees expression is one of literal/function_call/reference/if_then.
        unreachable!("expression must be literal, function_call, reference, or if_then")
    }
}

impl Lower for rules::if_clause<'_> {
    type Output = IfClause;

    fn lower(&self, cx: &ParseCtx<'_>) -> Result<Self::Output, MessageParseError> {
        let (condition, result) = self.expression();
        Ok(IfClause {
            r#if: Some(condition.lower(cx)?),
            then: Some(result.lower(cx)?),
        })
    }
}

impl Lower for rules::if_then<'_> {
    type Output = IfThen;

    fn lower(&self, cx: &ParseCtx<'_>) -> Result<Self::Output, MessageParseError> {
        let (first_clause, rest_clauses) = self.if_clause();
        let mut ifs = Vec::with_capacity(rest_clauses.len() + 1);

        ifs.push(first_clause.lower(cx)?);
        for clause in rest_clauses {
            ifs.push(clause.lower(cx)?);
        }

        Ok(IfThen {
            ifs,
            r#else: Some(Box::new(self.expression().lower(cx)?)),
        })
    }
}

impl Lower for rules::aggregate_measure<'_> {
    type Output = Measure;

    fn lower(&self, cx: &ParseCtx<'_>) -> Result<Self::Output, MessageParseError> {
        let scalar = self.function_call().lower(cx)?;
        Ok(Measure {
            measure: Some(AggregateFunction {
                function_reference: scalar.function_reference,
                arguments: scalar.arguments,
                options: scalar.options,
                output_type: scalar.output_type,
                invocation: 0,
                phase: 0,
                sorts: vec![],
                #[allow(deprecated)]
                args: scalar.args,
            }),
            filter: None,
        })
    }
}

impl ParseFragment for Literal {
    fn parse_fragment(
        extensions: &SimpleExtensions,
        input: &str,
    ) -> Result<Self, MessageParseError> {
        let cx = ParseCtx { extensions };
        let typed = parse_typed::<rules::literal<'_>>(input, "Literal")?;
        typed.lower(&cx)
    }
}

impl ParseFragment for ScalarFunction {
    fn parse_fragment(
        extensions: &SimpleExtensions,
        input: &str,
    ) -> Result<Self, MessageParseError> {
        let cx = ParseCtx { extensions };
        let typed = parse_typed::<rules::function_call<'_>>(input, "ScalarFunction")?;
        typed.lower(&cx)
    }
}

impl ParseFragment for Expression {
    fn parse_fragment(
        extensions: &SimpleExtensions,
        input: &str,
    ) -> Result<Self, MessageParseError> {
        let cx = ParseCtx { extensions };
        let typed = parse_typed::<rules::expression<'_>>(input, "Expression")?;
        typed.lower(&cx)
    }
}

impl ParseFragment for FieldReference {
    fn parse_fragment(
        _extensions: &SimpleExtensions,
        input: &str,
    ) -> Result<Self, MessageParseError> {
        let typed = parse_typed::<rules::reference<'_>>(input, "FieldReference")?;
        FieldReference::try_from(&typed)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixtures::TestContext;
    use crate::parser::convert::FieldIndex;

    fn make_literal_bool(value: bool) -> Expression {
        Expression {
            rex_type: Some(RexType::Literal(Literal {
                literal_type: Some(LiteralType::Boolean(value)),
                nullable: false,
                type_variation_reference: 0,
            })),
        }
    }

    #[test]
    fn test_parse_field_reference() {
        assert_eq!(
            FieldReference::parse_fragment(&SimpleExtensions::default(), "$1").unwrap(),
            FieldReference::from(FieldIndex(1))
        );
    }

    #[test]
    fn test_parse_integer_literal() {
        let extensions = SimpleExtensions::default();
        let expected = Literal {
            literal_type: Some(LiteralType::I64(1)),
            nullable: false,
            type_variation_reference: 0,
        };
        assert_eq!(Literal::parse_fragment(&extensions, "1").unwrap(), expected);
    }

    #[test]
    fn test_parse_float_literal() {
        let extensions = SimpleExtensions::default();
        let expected = Literal {
            literal_type: Some(LiteralType::Fp64(3.82)),
            nullable: false,
            type_variation_reference: 0,
        };
        assert_eq!(
            Literal::parse_fragment(&extensions, "3.82").unwrap(),
            expected
        );
    }

    #[test]
    fn test_parse_negative_float_literal() {
        let extensions = SimpleExtensions::default();
        let expected = Literal {
            literal_type: Some(LiteralType::Fp64(-2.5)),
            nullable: false,
            type_variation_reference: 0,
        };
        assert_eq!(
            Literal::parse_fragment(&extensions, "-2.5").unwrap(),
            expected
        );
    }

    #[test]
    fn test_parse_boolean_literals() {
        let extensions = SimpleExtensions::default();
        assert_eq!(
            Literal::parse_fragment(&extensions, "true").unwrap(),
            Literal {
                literal_type: Some(LiteralType::Boolean(true)),
                nullable: false,
                type_variation_reference: 0,
            }
        );
        assert_eq!(
            Literal::parse_fragment(&extensions, "false").unwrap(),
            Literal {
                literal_type: Some(LiteralType::Boolean(false)),
                nullable: false,
                type_variation_reference: 0,
            }
        );
    }

    #[test]
    fn test_parse_float_literal_with_fp32_type() {
        let extensions = SimpleExtensions::default();
        let result = Literal::parse_fragment(&extensions, "3.82:fp32").unwrap();

        match result.literal_type {
            Some(LiteralType::Fp32(val)) => assert!((val - 3.82).abs() < f32::EPSILON),
            _ => panic!("Expected Fp32 literal type"),
        }
    }

    #[test]
    fn test_parse_date_literal() {
        let extensions = SimpleExtensions::default();
        let result = Literal::parse_fragment(&extensions, "'2023-12-25':date").unwrap();

        match result.literal_type {
            Some(LiteralType::Date(days)) => assert!(days > 0, "Expected positive days"),
            _ => panic!("Expected Date literal type, got: {:?}", result.literal_type),
        }
    }

    #[test]
    fn test_parse_time_literal() {
        let extensions = SimpleExtensions::default();
        let result = Literal::parse_fragment(&extensions, "'14:30:45':time").unwrap();

        match result.literal_type {
            Some(LiteralType::Time(microseconds)) => {
                let expected = (14 * 3600 + 30 * 60 + 45) * 1_000_000;
                assert_eq!(microseconds, expected);
            }
            _ => panic!("Expected Time literal type, got: {:?}", result.literal_type),
        }
    }

    #[test]
    fn test_parse_timestamp_literal_with_t() {
        let extensions = SimpleExtensions::default();
        let result =
            Literal::parse_fragment(&extensions, "'2023-01-01T12:00:00':timestamp").unwrap();

        match result.literal_type {
            #[allow(deprecated)]
            Some(LiteralType::Timestamp(microseconds)) => {
                assert!(
                    microseconds > 0,
                    "Expected positive microseconds since epoch"
                );
            }
            _ => panic!(
                "Expected Timestamp literal type, got: {:?}",
                result.literal_type
            ),
        }
    }

    #[test]
    fn test_parse_timestamp_literal_with_space() {
        let extensions = SimpleExtensions::default();
        let result =
            Literal::parse_fragment(&extensions, "'2023-01-01 12:00:00':timestamp").unwrap();

        match result.literal_type {
            #[allow(deprecated)]
            Some(LiteralType::Timestamp(microseconds)) => {
                assert!(
                    microseconds > 0,
                    "Expected positive microseconds since epoch"
                );
            }
            _ => panic!(
                "Expected Timestamp literal type, got: {:?}",
                result.literal_type
            ),
        }
    }

    #[test]
    fn test_parse_if_then_single_clause() {
        let extensions = SimpleExtensions::default();
        let cx = ParseCtx {
            extensions: &extensions,
        };
        let typed =
            parse_typed::<rules::if_then<'_>>("if_then(true -> 42, _ -> 0)", "IfThen").unwrap();
        let result = typed.lower(&cx).unwrap();

        assert_eq!(result.ifs.len(), 1);
        assert!(result.r#else.is_some());
    }

    #[test]
    fn test_parse_if_then_with_typed_literals() {
        let extensions = SimpleExtensions::default();
        let cx = ParseCtx {
            extensions: &extensions,
        };
        let typed =
            parse_typed::<rules::if_then<'_>>("if_then(true -> 100:i32, _ -> -100:i32)", "IfThen")
                .unwrap();
        let result = typed.lower(&cx).unwrap();

        assert_eq!(result.ifs.len(), 1);
        assert!(result.r#else.is_some());
    }

    #[test]
    fn test_parse_if_clause_with_whitespace_variations() {
        let extensions = SimpleExtensions::default();
        let cx = ParseCtx {
            extensions: &extensions,
        };
        let inputs = vec!["true->false", "true -> false", "true  ->  false"];

        for input in inputs {
            let typed = parse_typed::<rules::if_clause<'_>>(input, "IfClause").unwrap();
            let result = typed.lower(&cx).unwrap();
            assert!(result.r#if.is_some());
            assert!(result.then.is_some());
        }
    }

    #[test]
    fn test_if_clause_structure() {
        let extensions = SimpleExtensions::default();
        let cx = ParseCtx {
            extensions: &extensions,
        };
        let typed = parse_typed::<rules::if_clause<'_>>("42 -> 100", "IfClause").unwrap();
        let result = typed.lower(&cx).unwrap();

        let if_expr = result.r#if.as_ref().unwrap();
        let then_expr = result.then.as_ref().unwrap();

        match (&if_expr.rex_type, &then_expr.rex_type) {
            (Some(RexType::Literal(_)), Some(RexType::Literal(_))) => {}
            _ => panic!("Expected both if and then to be literals"),
        }
    }

    #[test]
    fn test_if_then_structure() {
        let extensions = SimpleExtensions::default();
        let cx = ParseCtx {
            extensions: &extensions,
        };
        let typed =
            parse_typed::<rules::if_then<'_>>("if_then(true -> 1, false -> 2, _ -> 0)", "IfThen")
                .unwrap();
        let result = typed.lower(&cx).unwrap();

        assert_eq!(result.ifs.len(), 2);
        for clause in &result.ifs {
            assert!(clause.r#if.is_some());
            assert!(clause.then.is_some());
        }
        assert!(result.r#else.is_some());
    }

    #[test]
    fn test_parse_if_then_mixed_types_in_conditions() {
        let extensions = SimpleExtensions::default();
        let cx = ParseCtx {
            extensions: &extensions,
        };
        let input = "if_then(true -> 1, true -> 'yes', 'yes' -> true, 42 -> 2, $0 -> 3, _ -> 0)";
        let typed = parse_typed::<rules::if_then<'_>>(input, "IfThen").unwrap();
        let result = typed.lower(&cx).unwrap();

        assert_eq!(result.ifs.len(), 5);
        assert!(result.r#else.is_some());
    }

    #[test]
    fn test_if_then_preserves_clause_order() {
        let extensions = SimpleExtensions::default();
        let cx = ParseCtx {
            extensions: &extensions,
        };
        let input = "if_then(1 -> 10, 2 -> 20, 3 -> 30, _ -> 0)";
        let typed = parse_typed::<rules::if_then<'_>>(input, "IfThen").unwrap();
        let result = typed.lower(&cx).unwrap();

        assert_eq!(result.ifs.len(), 3);

        for (i, clause) in result.ifs.iter().enumerate() {
            if let Some(Expression {
                rex_type: Some(RexType::Literal(lit)),
            }) = &clause.r#if
                && let Some(LiteralType::I64(val)) = &lit.literal_type {
                    assert_eq!(*val, (i as i64) + 1);
                }
        }
    }

    #[test]
    fn test_parse_if_then() {
        let extensions = SimpleExtensions::default();
        let cx = ParseCtx {
            extensions: &extensions,
        };

        let c1 = IfClause {
            r#if: Some(make_literal_bool(true)),
            then: Some(make_literal_bool(true)),
        };

        let c2 = IfClause {
            r#if: Some(make_literal_bool(false)),
            then: Some(make_literal_bool(false)),
        };

        let expected = IfThen {
            ifs: vec![c1, c2],
            r#else: Some(Box::new(make_literal_bool(false))),
        };

        let typed = parse_typed::<rules::if_then<'_>>(
            "if_then(true -> true , false -> false, _ -> false)",
            "IfThen",
        )
        .unwrap();
        let actual = typed.lower(&cx).unwrap();
        assert_eq!(actual, expected);
    }

    #[test]
    fn test_parse_function_and_expression_via_fixture() {
        let ctx = TestContext::new()
            .with_urn(4, "some_source")
            .with_function(4, 12, "foo")
            .with_function(4, 14, "bar");

        assert!(ScalarFunction::parse_fragment(&ctx.extensions, "foo()").is_ok());
        assert!(Expression::parse_fragment(&ctx.extensions, "bar(12)").is_ok());
    }
}
