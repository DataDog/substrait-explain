use super::foundation::{NONSPECIFIC, Scope};
use super::{Textify, TextifyError};
use crate::extensions::SimpleExtensions;

use std::fmt;

use ptype::parameter::Parameter;
use substrait::proto;
use substrait::proto::extensions::simple_extension_declaration::ExtensionTypeVariation;
use substrait::proto::r#type as ptype;

const UNKNOWN_TYPE_VARIATION: &str = "!{unknown_variant}";
const NULLABILITY_UNSPECIFIED: &str = "⁉";

impl Textify for ptype::Nullability {
    fn name() -> &'static str {
        "Nullability"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &mut S, w: &mut W) -> fmt::Result {
        match self {
            ptype::Nullability::Unspecified => {
                ctx.push_error(TextifyError::invalid(
                    "Nullability",
                    NONSPECIFIC,
                    "Nullability left Unspecified",
                ));

                // TODO: what should unspecified Nullabilitylook like?
                w.write_str(NULLABILITY_UNSPECIFIED)?;
            }
            ptype::Nullability::Nullable => write!(w, "?")?,
            ptype::Nullability::Required => {}
        };
        Ok(())
    }
}

fn textify_type_variation<S: Scope, W: fmt::Write>(
    ctx: &mut S,
    f: &mut W,
    name: &str,
    anchor: u32,
) -> fmt::Result {
    if anchor == 0 {
        // This is the default, this doesn't count as a type variation
        return Ok(());
    }
    let type_variation: Option<ExtensionTypeVariation> =
        ctx.extensions().find_type_variation(anchor);

    let (name, uri) = match type_variation {
        Some(ref ext) => (ext.name.as_str(), Some(ext.extension_uri_reference)),

        None => {
            ctx.push_error(TextifyError::invalid(
                "ExtensionTypeVariation",
                Some(name.to_string()),
                format!("Unknown type variation {}", anchor),
            ));
            (UNKNOWN_TYPE_VARIATION, None)
        }
    };

    write!(f, "[{}", name)?;

    if ctx.options().show_simple_extension_anchors || type_variation.is_none() {
        write!(f, "#{}", anchor)?;
    }

    if let Some(uri) = uri {
        if ctx.options().show_extension_uris {
            write!(f, "@{}", uri)?;
        }
    }
    write!(f, "]")?;

    Ok(())
}

// Textify a type with parameters.
//
// P will generally be the Parameter type, but it can be any type that
// implements Textify.
fn textify_type<S: Scope, W: fmt::Write, P: Textify, I: IntoIterator<Item = Option<P>>>(
    ctx: &mut S,
    f: &mut W,
    name: impl AsRef<str>,
    nullability: ptype::Nullability,
    variant: u32,
    params: I,
) -> fmt::Result {
    write!(f, "{}", name.as_ref())?;
    nullability.textify(ctx, f)?;
    textify_type_variation(ctx, f, name.as_ref(), variant)?;

    let mut first = true;
    for param in params.into_iter() {
        if first {
            write!(f, "<")?;
            first = false;
        } else {
            write!(f, ", ")?;
        }
        ctx.expect(f, &param)?;
    }
    if !first {
        write!(f, ">")?;
    }
    Ok(())
}

macro_rules! textify_kind {
    ($ctx:expr, $f:expr, $kind:ident, $name:expr) => {
        textify_type(
            $ctx,
            $f,
            $name,
            $kind.nullability(),
            $kind.type_variation_reference,
            [] as [Option<Parameter>; 0],
        )
    };
}

impl Textify for Parameter {
    fn name() -> &'static str {
        "Parameter"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &mut S, w: &mut W) -> fmt::Result {
        match self {
            Parameter::Boolean(true) => write!(w, "true")?,
            Parameter::Boolean(false) => write!(w, "false")?,
            Parameter::DataType(t) => t.textify(ctx, w)?,
            Parameter::Enum(e) => write!(w, "{}", e)?,
            Parameter::Integer(i) => write!(w, "{}", i)?,
            // TODO: Do we just put the string in directly?
            Parameter::String(s) => write!(w, "{}", s)?,
            Parameter::Null(_) => write!(w, "null")?,
        };

        Ok(())
    }
}
impl Textify for ptype::Parameter {
    fn name() -> &'static str {
        "Parameter"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &mut S, w: &mut W) -> fmt::Result {
        ctx.expect(w, &self.parameter)
    }
}

impl Textify for ptype::UserDefined {
    fn name() -> &'static str {
        "UserDefined"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &mut S, w: &mut W) -> fmt::Result {
        {
            let type_reference = ctx.extensions().find_type(self.type_reference);
            let params = self.type_parameters.iter().map(|t| Some(t.clone()));
            let typ = match type_reference {
                Some(t) => t,
                None => {
                    return ctx.failure(
                        w,
                        TextifyError::invalid(
                            "UserDefined",
                            Some(format!("{}", self.type_reference)),
                            format!("Type reference {} not found", self.type_reference),
                        ),
                    );
                }
            };

            textify_type(
                ctx,
                w,
                format!("u!{}", typ.name),
                self.nullability(),
                self.type_variation_reference,
                params,
            )
        }
    }
}

impl Textify for ptype::Kind {
    fn name() -> &'static str {
        "Kind"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &mut S, w: &mut W) -> fmt::Result {
        match self {
            // This is the expansion of:
            //     textify_kind!(ctx, w, k, "boolean")
            // Shown here for visibility
            ptype::Kind::Bool(k) => textify_type(
                ctx,
                w,
                "boolean",
                k.nullability(),
                k.type_variation_reference,
                [] as [Option<Parameter>; 0],
            ),
            ptype::Kind::I8(k) => textify_kind!(ctx, w, k, "i8"),
            ptype::Kind::I16(k) => textify_kind!(ctx, w, k, "i16"),
            ptype::Kind::I32(k) => textify_kind!(ctx, w, k, "i32"),
            ptype::Kind::I64(k) => textify_kind!(ctx, w, k, "i64"),
            ptype::Kind::Fp32(k) => textify_kind!(ctx, w, k, "fp32"),
            ptype::Kind::Fp64(k) => textify_kind!(ctx, w, k, "fp64"),
            ptype::Kind::String(k) => textify_kind!(ctx, w, k, "string"),
            ptype::Kind::Binary(k) => textify_kind!(ctx, w, k, "binary"),
            ptype::Kind::Timestamp(k) => textify_kind!(ctx, w, k, "timestamp"),
            ptype::Kind::Date(k) => textify_kind!(ctx, w, k, "date"),
            ptype::Kind::Time(k) => textify_kind!(ctx, w, k, "time"),
            ptype::Kind::IntervalYear(i) => {
                textify_kind!(ctx, w, i, "interval_year")
            }

            ptype::Kind::TimestampTz(ts) => {
                textify_kind!(ctx, w, ts, "timestamp_tz")
            }
            ptype::Kind::Uuid(uuid) => textify_kind!(ctx, w, uuid, "uuid"),

            ptype::Kind::IntervalDay(i) => textify_type(
                ctx,
                w,
                "interval_day",
                i.nullability(),
                i.type_variation_reference,
                // Precision defaults to 6 if unspecified
                [Some(Parameter::Integer(i.precision.unwrap_or(6) as i64))],
            ),
            ptype::Kind::IntervalCompound(i) => textify_type(
                ctx,
                w,
                "interval_compound",
                i.nullability(),
                i.type_variation_reference,
                [Some(Parameter::Integer(i.precision as i64))],
            ),
            ptype::Kind::FixedChar(c) => textify_type(
                ctx,
                w,
                "fixedchar",
                c.nullability(),
                c.type_variation_reference,
                [Some(Parameter::Integer(c.length as i64))],
            ),
            ptype::Kind::Varchar(_c) => todo!(),
            ptype::Kind::FixedBinary(_b) => todo!(),
            ptype::Kind::Decimal(_d) => todo!(),
            ptype::Kind::PrecisionTime(p) => textify_type(
                ctx,
                w,
                "precisiontime",
                p.nullability(),
                p.type_variation_reference,
                [Some(Parameter::Integer(p.precision as i64))],
            ),
            ptype::Kind::PrecisionTimestamp(p) => textify_type(
                ctx,
                w,
                "precisiontimestamp",
                p.nullability(),
                p.type_variation_reference,
                [Some(Parameter::Integer(p.precision as i64))],
            ),
            ptype::Kind::PrecisionTimestampTz(_p) => todo!(),
            ptype::Kind::Struct(s) => textify_type(
                ctx,
                w,
                "struct",
                s.nullability(),
                s.type_variation_reference,
                s.types.iter().map(|t| Some(Parameter::DataType(t.clone()))),
            ),
            ptype::Kind::List(l) => {
                let p = l
                    .r#type
                    .as_ref()
                    .map(|t| Parameter::DataType((**t).to_owned()));
                textify_type(
                    ctx,
                    w,
                    "list",
                    l.nullability(),
                    l.type_variation_reference,
                    [p],
                )
            }
            ptype::Kind::Map(m) => {
                let k = m
                    .key
                    .as_ref()
                    .map(|t| Parameter::DataType((**t).to_owned()));
                let v = m
                    .value
                    .as_ref()
                    .map(|t| Parameter::DataType((**t).to_owned()));
                textify_type(
                    ctx,
                    w,
                    "map",
                    m.nullability(),
                    m.type_variation_reference,
                    [k, v],
                )
            }
            ptype::Kind::UserDefined(u) => u.textify(ctx, w),
            ptype::Kind::UserDefinedTypeReference(r) => {
                // Defer to the UserDefined definition, using defaults for
                // variation, and non-nullable as suggested by the docs
                let udf = ptype::UserDefined {
                    type_reference: *r,
                    type_variation_reference: 0,
                    nullability: ptype::Nullability::Required as i32,
                    type_parameters: vec![],
                };
                ptype::Kind::UserDefined(udf).textify(ctx, w)
            }
        }
    }
}

impl Textify for proto::Type {
    fn name() -> &'static str {
        "Type"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &mut S, w: &mut W) -> fmt::Result {
        ctx.expect(w, &self.kind)
    }
}

#[cfg(test)]
mod tests {
    use super::super::fixtures::TestContext;

    use super::*;

    #[test]
    fn type_display() {
        let ctx = TestContext::new()
            .with_uri(1, "first")
            .with_type_variation(1, 2, "u8");

        let t = proto::Type {
            kind: Some(ptype::Kind::Bool(ptype::Boolean {
                type_variation_reference: 2,
                nullability: ptype::Nullability::Nullable as i32,
            })),
        };

        let s = ctx.textify_no_errors(t);
        assert_eq!(s, "boolean?[u8]");

        let t = proto::Type {
            kind: Some(ptype::Kind::Bool(ptype::Boolean {
                type_variation_reference: 200,
                nullability: ptype::Nullability::Nullable as i32,
            })),
        };
        let (s, errs) = ctx.textify(t);
        assert_eq!(s, "boolean?[!{unknown_variant}#200]");
        assert_eq!(errs.0.len(), 1);
        assert_eq!(errs.0[0].message, "ExtensionTypeVariation");
        assert_eq!(
            errs.0[0].lookup.as_ref().map(|t| t.as_ref()),
            Some("boolean")
        );
        assert_eq!(errs.0[0].description, "Unknown type variation 200");

        let t = proto::Type {
            kind: Some(ptype::Kind::I8(ptype::I8 {
                type_variation_reference: 0,
                nullability: ptype::Nullability::Required as i32,
            })),
        };
        assert_eq!(ctx.textify_no_errors(t), "i8");

        let t = proto::Type {
            kind: Some(ptype::Kind::PrecisionTimestamp(ptype::PrecisionTimestamp {
                type_variation_reference: 0,
                nullability: ptype::Nullability::Nullable as i32,
                precision: 3,
            })),
        };
        assert_eq!(ctx.textify_no_errors(t), "precisiontimestamp?<3>");

        let mut ctx = ctx.with_type_variation(1, 8, "int");
        ctx.options.show_simple_extension_anchors = true;

        let t = proto::Type {
            kind: Some(ptype::Kind::PrecisionTime(ptype::PrecisionTime {
                type_variation_reference: 8,
                nullability: ptype::Nullability::Nullable as i32,
                precision: 9,
            })),
        };
        assert_eq!(ctx.textify_no_errors(t), "precisiontime?[int#8]<9>");
    }

    #[test]
    fn type_display_with_errors() {
        let ctx = TestContext::new()
            .with_uri(1, "first")
            .with_type(1, 100, "cow");

        let t = proto::Type {
            kind: Some(ptype::Kind::UserDefined(ptype::UserDefined {
                type_variation_reference: 0,
                nullability: ptype::Nullability::Required as i32,
                type_reference: 100,
                type_parameters: vec![],
            })),
        };

        let (s, errs) = ctx.textify(t);
        assert!(errs.0.is_empty(), "{}", errs);
        assert_eq!(s, "u!cow");

        let t = proto::Type {
            kind: Some(ptype::Kind::UserDefined(ptype::UserDefined {
                type_variation_reference: 0,
                nullability: ptype::Nullability::Required as i32,
                type_reference: 12589,
                type_parameters: vec![],
            })),
        };

        let (s, errs) = ctx.textify(t);
        assert_eq!(errs.0.len(), 1);
        assert_eq!(errs.0[0].message, "UserDefined");
        assert_eq!(errs.0[0].lookup, Some("12589".into()));
        assert_eq!(errs.0[0].description, "Type reference 12589 not found");
        assert_eq!(s, "!{UserDefined: 12589}");
    }

    #[test]
    fn struct_display() {
        let ctx = TestContext::new();

        let t = proto::Type {
            kind: Some(ptype::Kind::Struct(ptype::Struct {
                type_variation_reference: 0,
                nullability: ptype::Nullability::Nullable as i32,
                types: vec![
                    proto::Type {
                        kind: Some(ptype::Kind::String(ptype::String {
                            type_variation_reference: 0,
                            nullability: ptype::Nullability::Required as i32,
                        })),
                    },
                    proto::Type {
                        kind: Some(ptype::Kind::I8(ptype::I8 {
                            type_variation_reference: 0,
                            nullability: ptype::Nullability::Required as i32,
                        })),
                    },
                    proto::Type {
                        kind: Some(ptype::Kind::I32(ptype::I32 {
                            type_variation_reference: 0,
                            nullability: ptype::Nullability::Nullable as i32,
                        })),
                    },
                    proto::Type {
                        kind: Some(ptype::Kind::TimestampTz(ptype::TimestampTz {
                            type_variation_reference: 0,
                            nullability: ptype::Nullability::Required as i32,
                        })),
                    },
                ],
            })),
        };
        assert_eq!(
            ctx.textify_no_errors(t),
            "struct?<string, i8, i32?, timestamp_tz>"
        );
    }
}
