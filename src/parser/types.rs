use pest::iterators::Pair;
use substrait::proto::r#type::{Kind, Nullability, Parameter};
use substrait::proto::{self, Type};

use super::{ParsePair, Rule, ScopedParsePair, iter_pairs, unwrap_single_pair};
use crate::extensions::SimpleExtensions;
use crate::extensions::simple::{ExtensionKind, MissingReference};
use crate::parser::{ErrorKind, MessageParseError};

fn make_lookup_error(
    kind: ExtensionKind,
    e: MissingReference,
    message: &str,
    span: pest::Span,
) -> MessageParseError {
    MessageParseError {
        message: kind.name(),
        kind: ErrorKind::Lookup(e),
        error: Box::new(pest::error::Error::new_from_span(
            pest::error::ErrorVariant::CustomError {
                message: message.to_string(),
            },
            span,
        )),
    }
}

/// Given a name (plain or compound) and an optional anchor, resolve and
/// validate the function anchor.
///
/// When `anchor` is `Some(a)`:
///   - Looks up the stored name for `a`.
///   - Accepts the anchor if the stored compound name equals `name` (exact
///     match) OR if `name` equals the base name of the stored compound name
///     (e.g. `name = "equal"` matches stored `"equal:any_any"`).
///
/// When `anchor` is `None`:
///   - First tries an exact match via `find_by_name` (handles both plain names
///     and full compound names like `"equal:any_any"`).
///   - If that returns `MissingName` and `name` contains no `:`, falls back to
///     `find_by_base_name` so that `equal(…)` resolves when `equal:any_any` is
///     the only registered overload.
pub(crate) fn get_and_validate_anchor(
    extensions: &SimpleExtensions,
    kind: ExtensionKind,
    anchor: Option<u32>,
    name: &str,
    span: pest::Span,
) -> Result<u32, MessageParseError> {
    match anchor {
        Some(a) => {
            // Explicit anchor: validate the supplied name is compatible with
            // the stored compound name (accept either exact or base-name match).
            match extensions.find_by_anchor(kind, a) {
                Err(e) => Err(make_lookup_error(
                    kind,
                    e,
                    "Error matching name to anchor",
                    span,
                )),
                Ok((_, stored)) => {
                    use crate::extensions::simple::base_name;
                    if stored == name || base_name(stored) == name {
                        // why do we need both these conditions? Shouldnt just base_name(stored) == name cover it?
                        Ok(a)
                    } else {
                        Err(make_lookup_error(
                            kind,
                            MissingReference::Mismatched(kind, name.to_string(), a),
                            "Error matching name to anchor",
                            span,
                        ))
                    }
                }
            }
        }
        None => {
            // No anchor: try exact compound-name match first.
            match extensions.find_by_name(kind, name) {
                Ok(a) => Ok(a),
                // If the name has no ':' and wasn't found as an exact key, it
                // might be a base-name reference into compound-named entries.
                Err(MissingReference::MissingName(_, _)) if !name.contains(':') => {
                    extensions.find_by_base_name(kind, name).map_err(|e| {
                        make_lookup_error(kind, e, "Error finding extension for name", span)
                    })
                }
                Err(e) => Err(make_lookup_error(
                    kind,
                    e,
                    "Error finding extension for name",
                    span,
                )),
            }
        }
    }
}

impl ParsePair for Nullability {
    fn rule() -> Rule {
        Rule::nullability
    }

    fn message() -> &'static str {
        "Nullability"
    }

    fn parse_pair(pair: Pair<Rule>) -> Self {
        assert_eq!(pair.as_rule(), Rule::nullability);
        match pair.as_str() {
            "?" => Nullability::Nullable,
            "" => Nullability::Required,
            "⁉" => Nullability::Unspecified,
            _ => panic!("Invalid nullability: {}", pair.as_str()),
        }
    }
}

impl ScopedParsePair for Parameter {
    fn rule() -> Rule {
        Rule::parameter
    }

    fn message() -> &'static str {
        "Parameter"
    }

    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Rule::parameter);
        let inner = unwrap_single_pair(pair);
        match inner.as_rule() {
            Rule::r#type => Ok(Parameter {
                parameter: Some(proto::r#type::parameter::Parameter::DataType(
                    Type::parse_pair(extensions, inner)?,
                )),
            }),
            _ => unimplemented!("{:?}", inner.as_rule()),
        }
    }
}

fn parse_simple_type(pair: Pair<Rule>) -> Type {
    assert_eq!(pair.as_rule(), Rule::simple_type);
    let mut iter = iter_pairs(pair.into_inner());
    let name = iter.pop(Rule::simple_type_name).as_str();
    let nullability = iter.parse_next::<Nullability>();
    iter.done();

    let kind = match name {
        "boolean" => Kind::Bool(proto::r#type::Boolean {
            nullability: nullability.into(),
            type_variation_reference: 0,
        }),
        "i64" => Kind::I64(proto::r#type::I64 {
            nullability: nullability.into(),
            type_variation_reference: 0,
        }),
        "i32" => Kind::I32(proto::r#type::I32 {
            nullability: nullability.into(),
            type_variation_reference: 0,
        }),
        "i16" => Kind::I16(proto::r#type::I16 {
            nullability: nullability.into(),
            type_variation_reference: 0,
        }),
        "i8" => Kind::I8(proto::r#type::I8 {
            nullability: nullability.into(),
            type_variation_reference: 0,
        }),
        "fp32" => Kind::Fp32(proto::r#type::Fp32 {
            nullability: nullability.into(),
            type_variation_reference: 0,
        }),
        "fp64" => Kind::Fp64(proto::r#type::Fp64 {
            nullability: nullability.into(),
            type_variation_reference: 0,
        }),
        "string" => Kind::String(proto::r#type::String {
            nullability: nullability.into(),
            type_variation_reference: 0,
        }),
        "binary" => Kind::Binary(proto::r#type::Binary {
            nullability: nullability.into(),
            type_variation_reference: 0,
        }),
        #[allow(deprecated)]
        "timestamp" => Kind::Timestamp(proto::r#type::Timestamp {
            nullability: nullability.into(),
            type_variation_reference: 0,
        }),
        #[allow(deprecated)]
        "timestamp_tz" => Kind::TimestampTz(proto::r#type::TimestampTz {
            nullability: nullability.into(),
            type_variation_reference: 0,
        }),
        "date" => Kind::Date(proto::r#type::Date {
            nullability: nullability.into(),
            type_variation_reference: 0,
        }),
        "time" => Kind::Time(proto::r#type::Time {
            nullability: nullability.into(),
            type_variation_reference: 0,
        }),
        "interval_year" => Kind::IntervalYear(proto::r#type::IntervalYear {
            nullability: nullability.into(),
            type_variation_reference: 0,
        }),
        "uuid" => Kind::Uuid(proto::r#type::Uuid {
            nullability: nullability.into(),
            type_variation_reference: 0,
        }),
        _ => unreachable!("Type {} exists in parser but not implemented in code", name),
    };
    Type { kind: Some(kind) }
}

fn parse_compound_type(
    extensions: &SimpleExtensions,
    pair: Pair<Rule>,
) -> Result<Type, MessageParseError> {
    assert_eq!(pair.as_rule(), Rule::compound_type);
    let inner = unwrap_single_pair(pair);
    match inner.as_rule() {
        Rule::list_type => parse_list_type(extensions, inner),
        // Rule::map_type => parse_map_type(inner),
        // Rule::struct_type => parse_struct_type(inner),
        _ => unimplemented!("{:?}", inner.as_rule()),
    }
}

fn parse_list_type(
    extensions: &SimpleExtensions,
    pair: Pair<Rule>,
) -> Result<Type, MessageParseError> {
    assert_eq!(pair.as_rule(), Rule::list_type);
    let mut iter = iter_pairs(pair.into_inner());
    let nullability = iter.parse_next::<Nullability>();
    let inner = iter.parse_next_scoped::<Type>(extensions)?;
    iter.done();

    Ok(Type {
        kind: Some(Kind::List(Box::new(proto::r#type::List {
            nullability: nullability.into(),
            r#type: Some(Box::new(inner)),
            type_variation_reference: 0,
        }))),
    })
}

fn parse_parameters(
    extensions: &SimpleExtensions,
    pair: Pair<Rule>,
) -> Result<Vec<Parameter>, MessageParseError> {
    assert_eq!(pair.as_rule(), Rule::parameters);
    let mut iter = iter_pairs(pair.into_inner());
    let mut params = Vec::new();
    while let Some(param) = iter.parse_if_next_scoped::<Parameter>(extensions) {
        params.push(param?);
    }
    iter.done();
    Ok(params)
}

fn parse_user_defined_type(
    extensions: &SimpleExtensions,
    pair: Pair<Rule>,
) -> Result<Type, MessageParseError> {
    let span = pair.as_span();
    assert_eq!(pair.as_rule(), Rule::user_defined_type);
    let mut iter = iter_pairs(pair.into_inner());
    let name = iter.pop(Rule::name).as_str();
    let anchor = iter
        .try_pop(Rule::anchor)
        .map(|n| unwrap_single_pair(n).as_str().parse::<u32>().unwrap());

    // TODO: Handle urn_anchor; validate that it matches the anchor
    let _urn_anchor = iter
        .try_pop(Rule::urn_anchor)
        .map(|n| unwrap_single_pair(n).as_str().parse::<u32>().unwrap());

    let nullability = iter.parse_next::<Nullability>();
    let parameters = match iter.try_pop(Rule::parameters) {
        Some(p) => parse_parameters(extensions, p)?,
        None => Vec::new(),
    };
    iter.done();

    let anchor = get_and_validate_anchor(extensions, ExtensionKind::Type, anchor, name, span)?;

    Ok(Type {
        kind: Some(Kind::UserDefined(proto::r#type::UserDefined {
            type_reference: anchor,
            nullability: nullability.into(),
            type_parameters: parameters,
            type_variation_reference: 0,
        })),
    })
}

impl ScopedParsePair for Type {
    fn rule() -> Rule {
        Rule::r#type
    }

    fn message() -> &'static str {
        "Type"
    }

    fn parse_pair(
        extensions: &SimpleExtensions,
        pair: Pair<Rule>,
    ) -> Result<Self, MessageParseError> {
        assert_eq!(pair.as_rule(), Rule::r#type);
        let inner = unwrap_single_pair(pair);
        match inner.as_rule() {
            Rule::simple_type => Ok(parse_simple_type(inner)),
            Rule::compound_type => parse_compound_type(extensions, inner),
            Rule::user_defined_type => parse_user_defined_type(extensions, inner),
            _ => unreachable!(
                "Grammar guarantees type can only be simple_type, compound_type, or user_defined_type, got: {:?}",
                inner.as_rule()
            ),
        }
    }
}

#[cfg(test)]
mod tests {
    use pest::Parser;
    use substrait::proto::r#type::{I64, Kind, Nullability};

    use super::*;
    use crate::parser::ExpressionParser;

    #[test]
    fn test_parse_simple_type() {
        let mut pairs = ExpressionParser::parse(Rule::simple_type, "i64").unwrap();
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);
        let t = parse_simple_type(pair);
        assert_eq!(
            t,
            Type {
                kind: Some(Kind::I64(I64 {
                    nullability: Nullability::Required as i32,
                    type_variation_reference: 0,
                })),
            }
        );

        let mut pairs = ExpressionParser::parse(Rule::simple_type, "string?").unwrap();
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);
        let t = parse_simple_type(pair);
        assert_eq!(
            t,
            Type {
                kind: Some(Kind::String(proto::r#type::String {
                    nullability: Nullability::Nullable as i32,
                    type_variation_reference: 0,
                })),
            }
        );
    }

    #[test]
    fn test_parse_type() {
        let extensions = SimpleExtensions::default();
        let mut pairs = ExpressionParser::parse(Rule::r#type, "i64").unwrap();
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);
        let t = Type::parse_pair(&extensions, pair).unwrap();
        assert_eq!(
            t,
            Type {
                kind: Some(Kind::I64(I64 {
                    nullability: Nullability::Required as i32,
                    type_variation_reference: 0,
                }))
            }
        );
    }

    #[test]
    fn test_parse_list_type() {
        let extensions = SimpleExtensions::default();
        let mut pairs = ExpressionParser::parse(Rule::list_type, "list<i64>").unwrap();
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);
        let t = parse_list_type(&extensions, pair).unwrap();
        assert_eq!(
            t,
            Type {
                kind: Some(Kind::List(Box::new(proto::r#type::List {
                    nullability: Nullability::Required as i32,
                    r#type: Some(Box::new(Type {
                        kind: Some(Kind::I64(I64 {
                            nullability: Nullability::Required as i32,
                            type_variation_reference: 0,
                        }))
                    })),
                    type_variation_reference: 0,
                })))
            }
        );
    }

    #[test]
    fn test_parse_parameters() {
        let extensions = SimpleExtensions::default();
        let mut pairs = ExpressionParser::parse(Rule::parameters, "<i64?,string>").unwrap();
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);
        let t = parse_parameters(&extensions, pair).unwrap();
        assert_eq!(
            t,
            vec![
                Parameter {
                    parameter: Some(proto::r#type::parameter::Parameter::DataType(Type {
                        kind: Some(Kind::I64(proto::r#type::I64 {
                            nullability: Nullability::Nullable as i32,
                            type_variation_reference: 0,
                        })),
                    })),
                },
                Parameter {
                    parameter: Some(proto::r#type::parameter::Parameter::DataType(Type {
                        kind: Some(Kind::String(proto::r#type::String {
                            nullability: Nullability::Required as i32,
                            type_variation_reference: 0,
                        })),
                    })),
                },
            ]
        );
    }

    #[test]
    fn test_udts() {
        let mut extensions = SimpleExtensions::default();
        extensions
            .add_extension_urn("some_source".to_string(), 4)
            .unwrap();
        extensions
            .add_extension(ExtensionKind::Type, 4, 42, "udt".to_string())
            .unwrap();
        let mut pairs = ExpressionParser::parse(Rule::user_defined_type, "udt#42<i64?>").unwrap();
        let pair = pairs.next().unwrap();
        assert_eq!(pairs.next(), None);

        let t = parse_user_defined_type(&extensions, pair).unwrap();
        assert_eq!(
            t,
            Type {
                kind: Some(Kind::UserDefined(proto::r#type::UserDefined {
                    type_reference: 42,
                    type_variation_reference: 0,
                    nullability: Nullability::Required as i32,
                    type_parameters: vec![Parameter {
                        parameter: Some(proto::r#type::parameter::Parameter::DataType(Type {
                            kind: Some(Kind::I64(proto::r#type::I64 {
                                nullability: Nullability::Nullable as i32,
                                type_variation_reference: 0,
                            })),
                        })),
                    }],
                }))
            }
        );
    }

    // ---- Tests for get_and_validate_anchor ----

    fn make_fn_extensions_with_compound_names() -> SimpleExtensions {
        let mut exts = SimpleExtensions::new();
        exts.add_extension_urn("test_urn".to_string(), 1).unwrap();
        exts.add_extension(ExtensionKind::Function, 1, 1, "equal:any_any".to_string())
            .unwrap();
        exts.add_extension(ExtensionKind::Function, 1, 2, "equal:str_str".to_string())
            .unwrap();
        exts.add_extension(ExtensionKind::Function, 1, 3, "add:i64_i64".to_string())
            .unwrap();
        exts
    }

    fn dummy_span() -> pest::Span<'static> {
        // A minimal Pest span for testing — points at an empty string.
        pest::Span::new("", 0, 0).unwrap()
    }

    #[test]
    fn test_anchor_exact_compound_name_match() {
        // Explicit anchor + exact compound name → resolves
        let exts = make_fn_extensions_with_compound_names();
        let result = get_and_validate_anchor(
            &exts,
            ExtensionKind::Function,
            Some(1),
            "equal:any_any",
            dummy_span(),
        );
        assert_eq!(result.unwrap(), 1);
    }

    #[test]
    fn test_anchor_base_name_match() {
        // Explicit anchor + base name only → resolves because stored name starts
        // with that base name
        let exts = make_fn_extensions_with_compound_names();
        let result = get_and_validate_anchor(
            &exts,
            ExtensionKind::Function,
            Some(1),
            "equal",
            dummy_span(),
        );
        assert_eq!(result.unwrap(), 1);
    }

    #[test]
    fn test_anchor_mismatched_name() {
        // Explicit anchor + wrong name → error
        let exts = make_fn_extensions_with_compound_names();
        let result = get_and_validate_anchor(
            &exts,
            ExtensionKind::Function,
            Some(1),
            "like",
            dummy_span(),
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_no_anchor_exact_compound_name() {
        // No anchor, full compound name → resolves directly
        let exts = make_fn_extensions_with_compound_names();
        let result = get_and_validate_anchor(
            &exts,
            ExtensionKind::Function,
            None,
            "equal:any_any",
            dummy_span(),
        );
        assert_eq!(result.unwrap(), 1);
        let result2 = get_and_validate_anchor(
            &exts,
            ExtensionKind::Function,
            None,
            "equal:str_str",
            dummy_span(),
        );
        assert_eq!(result2.unwrap(), 2);
    }

    #[test]
    fn test_no_anchor_base_name_unique() {
        // No anchor, base name only, only one overload → falls back to find_by_base_name
        let exts = make_fn_extensions_with_compound_names();
        // "add" has only one overload (add:i64_i64)
        let result =
            get_and_validate_anchor(&exts, ExtensionKind::Function, None, "add", dummy_span());
        assert_eq!(result.unwrap(), 3);
    }

    #[test]
    fn test_no_anchor_base_name_ambiguous() {
        // No anchor, base name only, multiple overloads → DuplicateName error
        let exts = make_fn_extensions_with_compound_names();
        // "equal" has two overloads
        let result =
            get_and_validate_anchor(&exts, ExtensionKind::Function, None, "equal", dummy_span());
        assert!(result.is_err());
    }

    #[test]
    fn test_no_anchor_plain_stored_name() {
        // Functions stored without a signature still resolve by their plain name
        let mut exts = SimpleExtensions::new();
        exts.add_extension_urn("urn".to_string(), 1).unwrap();
        exts.add_extension(ExtensionKind::Function, 1, 10, "coalesce".to_string())
            .unwrap();

        let result = get_and_validate_anchor(
            &exts,
            ExtensionKind::Function,
            None,
            "coalesce",
            dummy_span(),
        );
        assert_eq!(result.unwrap(), 10);
    }
}
