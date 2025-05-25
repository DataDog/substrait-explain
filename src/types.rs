use crate::textify::SimpleExtensions;

use super::textify::{OutputContext, Textify, TextifyError};

use std::borrow::Cow;
use std::fmt::{self};

use ptype::parameter::Parameter;
use substrait::proto::r#type as ptype;
use substrait::proto::{self};

impl Textify for ptype::Nullability {
    fn textify<W: fmt::Write>(
        &self,
        ctx: &mut OutputContext,
        w: &mut W,
    ) -> Result<(), TextifyError> {
        match self {
            ptype::Nullability::Unspecified => {
                if ctx.options().strict {
                    return Err(TextifyError::InvalidValue {
                        name: "Nullability".to_string(),
                        context: "Unspecified".to_string(),
                    });
                }
                // TODO: what should unspecified look like?
                write!(w, "⁉")?;
            }
            ptype::Nullability::Nullable => write!(w, "?")?,
            ptype::Nullability::Required => {}
        };
        Ok(())
    }
}

// Textify a type with parameters.
//
// P will generally be the Parameter type, but it can be any type that
// implements Textify.
fn textify_type<W: fmt::Write, P: Textify, I: IntoIterator<Item = P>>(
    ctx: &mut OutputContext,
    f: &mut W,
    name: impl AsRef<str>,
    nullability: ptype::Nullability,
    variant: u32,
    params: I,
) -> Result<(), TextifyError> {
    write!(f, "{}", name.as_ref())?;
    nullability.textify(ctx, f)?;
    if variant > 0 {
        write!(f, "[{}]", variant)?;
    };

    let mut first = true;
    for param in params.into_iter() {
        if first {
            write!(f, "<")?;
            first = false;
        } else {
            write!(f, ", ")?;
        }
        param.textify(ctx, f)?;
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
            [] as [Parameter; 0],
        )
    };
}

pub struct MissingValue<'a> {
    name: Cow<'a, str>,
}

impl<'a, T: Into<Cow<'a, str>>> From<T> for MissingValue<'a> {
    fn from(name: T) -> Self {
        MissingValue { name: name.into() }
    }
}

impl<'a> Textify for MissingValue<'a> {
    fn textify<W: fmt::Write>(
        &self,
        ctx: &mut OutputContext,
        w: &mut W,
    ) -> Result<(), TextifyError> {
        if ctx.options().strict {
            return Err(TextifyError::InvalidValue {
                name: format!("Missing argument {}", self.name),
                context: "Expected a value, found None".into(),
            });
        }
        write!(w, "!❬{}❭", self.name)?;
        Ok(())
    }
}

/// A wrapper around a value that should be present but isn't.
///
/// Protobuf messages structurally allow values to be optional that by the spec
/// may be necessary. Depending on options, this might end up as an error or as
/// a placeholder.
pub enum ExpectedValue<'a, T: Clone> {
    Value(Cow<'a, T>),
    Missing(MissingValue<'a>),
}

impl<'a, T: fmt::Debug + Clone> ExpectedValue<'a, T> {
    pub fn from_option(opt: impl Into<Option<&'a T>>, name: impl Into<Cow<'a, str>>) -> Self {
        match opt.into() {
            Some(t) => Self::Value(Cow::Borrowed(t)),
            None => Self::Missing(MissingValue { name: name.into() }),
        }
    }
    pub fn from_option_owned(opt: impl Into<Option<T>>, name: impl Into<Cow<'a, str>>) -> Self {
        match opt.into() {
            Some(t) => Self::Value(Cow::Owned(t)),
            None => Self::Missing(MissingValue { name: name.into() }),
        }
    }
}

impl<'a, T: Textify + fmt::Debug + Clone> Textify for ExpectedValue<'a, T> {
    fn textify<W: fmt::Write>(
        &self,
        ctx: &mut OutputContext,
        w: &mut W,
    ) -> Result<(), TextifyError> {
        match self {
            Self::Value(t) => t.textify(ctx, w),
            Self::Missing(m) => m.textify(ctx, w),
        }
    }
}

impl Textify for Parameter {
    fn textify<W: fmt::Write>(
        &self,
        ctx: &mut OutputContext,
        w: &mut W,
    ) -> Result<(), TextifyError> {
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
    fn textify<W: fmt::Write>(
        &self,
        ctx: &mut OutputContext,
        w: &mut W,
    ) -> Result<(), TextifyError> {
        ExpectedValue::from_option(self.parameter.as_ref(), "Parameter").textify(ctx, w)
    }
}

impl Textify for ptype::Kind {
    fn textify<W: fmt::Write>(
        &self,
        ctx: &mut OutputContext,
        w: &mut W,
    ) -> Result<(), TextifyError> {
        match self {
            ptype::Kind::Bool(k) => textify_type(
                ctx,
                w,
                "boolean",
                k.nullability(),
                k.type_variation_reference,
                [] as [Parameter; 0],
            ),
            //  textify_kind!(ctx, w, k, "boolean"),
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
                [Parameter::Integer(i.precision.unwrap_or(6) as i64)],
            ),
            ptype::Kind::IntervalCompound(i) => textify_type(
                ctx,
                w,
                "interval_compound",
                i.nullability(),
                i.type_variation_reference,
                [Parameter::Integer(i.precision as i64)],
            ),
            ptype::Kind::FixedChar(c) => textify_type(
                ctx,
                w,
                "fixedchar",
                c.nullability(),
                c.type_variation_reference,
                [Parameter::Integer(c.length as i64)],
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
                [Parameter::Integer(p.precision as i64)],
            ),
            ptype::Kind::PrecisionTimestamp(p) => textify_type(
                ctx,
                w,
                "precisiontimestamp",
                p.nullability(),
                p.type_variation_reference,
                [Parameter::Integer(p.precision as i64)],
            ),
            ptype::Kind::PrecisionTimestampTz(_p) => todo!(),
            ptype::Kind::Struct(s) => textify_type(
                ctx,
                w,
                "struct",
                s.nullability(),
                s.type_variation_reference,
                s.types.iter().map(|t| Parameter::DataType(t.clone())),
            ),
            ptype::Kind::List(l) => textify_type(
                ctx,
                w,
                "list",
                l.nullability(),
                l.type_variation_reference,
                [ExpectedValue::<Parameter>::from_option_owned(
                    l.r#type
                        .as_ref()
                        .map(|t| Parameter::DataType((**t).to_owned())),
                    "LIST_PARAMETER",
                )],
            ),
            ptype::Kind::Map(m) => textify_type(
                ctx,
                w,
                "map",
                m.nullability(),
                m.type_variation_reference,
                [
                    ExpectedValue::<Parameter>::from_option_owned(
                        m.key
                            .as_ref()
                            .map(|t| Parameter::DataType((**t).to_owned())),
                        "MAP_KEY",
                    ),
                    ExpectedValue::<Parameter>::from_option_owned(
                        m.value
                            .as_ref()
                            .map(|t| Parameter::DataType((**t).to_owned())),
                        "MAP_VALUE",
                    ),
                ],
            ),
            ptype::Kind::UserDefined(u) => {
                let type_variation = ctx.find_type_variation(u.type_variation_reference);
                if let Some(type_variation) = type_variation {
                    textify_type(
                        ctx,
                        w,
                        format!("u!{}", type_variation.name),
                        u.nullability(),
                        u.type_reference,
                        u.type_parameters.iter().cloned(),
                    )
                } else {
                    MissingValue::from("UserDefined").textify(ctx, w)
                }
            }
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
    fn textify<W: fmt::Write>(
        &self,
        ctx: &mut OutputContext,
        w: &mut W,
    ) -> Result<(), TextifyError> {
        self.kind
            .as_ref()
            .ok_or_else(|| TextifyError::InvalidValue {
                name: "Type".into(),
                context: "Required Kind unset".into(),
            })?
            .textify(ctx, w)
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    fn textify_basic<T: Textify>(t: T) -> String {
        let mut ctx = OutputContext::default();
        let mut s = String::new();
        t.textify(&mut ctx, &mut s).unwrap();
        s
    }

    #[test]
    fn type_display() {
        let t = proto::Type {
            kind: Some(ptype::Kind::Bool(ptype::Boolean {
                type_variation_reference: 2,
                nullability: ptype::Nullability::Nullable as i32,
            })),
        };

        assert_eq!(textify_basic(t), "boolean?[2]");

        let t = proto::Type {
            kind: Some(ptype::Kind::I8(ptype::I8 {
                type_variation_reference: 0,
                nullability: ptype::Nullability::Required as i32,
            })),
        };
        assert_eq!(textify_basic(t), "i8");

        let t = proto::Type {
            kind: Some(ptype::Kind::PrecisionTimestamp(ptype::PrecisionTimestamp {
                type_variation_reference: 0,
                nullability: ptype::Nullability::Nullable as i32,
                precision: 3,
            })),
        };
        assert_eq!(textify_basic(t), "precisiontimestamp?<3>");

        let t = proto::Type {
            kind: Some(ptype::Kind::PrecisionTime(ptype::PrecisionTime {
                type_variation_reference: 8,
                nullability: ptype::Nullability::Nullable as i32,
                precision: 9,
            })),
        };
        assert_eq!(textify_basic(t), "precisiontime?[8]<9>");
    }

    #[test]
    fn struct_display() {
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
        assert_eq!(textify_basic(t), "struct?<string, i8, i32?, timestamp_tz>");
    }
}
