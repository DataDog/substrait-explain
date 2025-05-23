use std::collections::HashMap;
use std::fmt::{self};

use pext::simple_extension_declaration::{
    ExtensionFunction, ExtensionType, ExtensionTypeVariation, MappingType,
};
use ptype::parameter::Parameter;
use substrait::proto;
use substrait::proto::extensions as pext;
use substrait::proto::r#type as ptype;

#[derive(Debug, Clone, Default)]
pub struct OutputContext {
    // Maps from extension URI anchor to URI
    uris: HashMap<u32, pext::SimpleExtensionUri>,
    // Maps from function anchor to (extension URI anchor, function name)
    functions: HashMap<u32, ExtensionFunction>,
    // Maps from type anchor to (extension URI anchor, type name)
    types: HashMap<u32, ExtensionType>,
    // Maps from type variation anchor to (extension URI anchor, type variation name)
    type_variations: HashMap<u32, ExtensionTypeVariation>,

    /// Show the types for columns in a read
    pub read_types: bool,
    /// Show the types for function parameters
    pub fn_types: bool,
    /// Show the nullability of types
    pub nullability: bool,
    /// Error on unspecified fields
    pub strict: bool,
    /// The indent to use for nested types
    pub indent: String,
}

impl OutputContext {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_extensions(
        uris: Vec<pext::SimpleExtensionUri>,
        extensions: Vec<pext::SimpleExtensionDeclaration>,
    ) -> Result<Self, TextifyError> {
        let mut uri_map = HashMap::new();
        let mut functions = HashMap::new();
        let mut types = HashMap::new();
        let mut type_variations = HashMap::new();

        for uri in uris {
            // TODO: Error on duplicate URIs
            uri_map.insert(uri.extension_uri_anchor, uri);
        }

        for extension in extensions {
            match extension.mapping_type {
                Some(MappingType::ExtensionType(t)) => {
                    types.insert(t.type_anchor, t);
                }
                Some(MappingType::ExtensionTypeVariation(v)) => {
                    type_variations.insert(v.type_variation_anchor, v);
                }
                Some(MappingType::ExtensionFunction(f)) => {
                    functions.insert(f.function_anchor, f);
                }
                None => {
                    return Err(TextifyError::InvalidValue {
                        name: "Extension".to_string(),
                        context: "Required MappingType unset".to_string(),
                    });
                }
            }
        }

        Ok(OutputContext {
            uris: uri_map,
            functions,
            types,
            type_variations,

            ..Default::default()
        })
    }

    pub fn find_uri(&self, anchor: u32) -> Option<pext::SimpleExtensionUri> {
        self.uris.get(&anchor).cloned()
    }

    pub fn find_function(&self, anchor: u32) -> Option<ExtensionFunction> {
        self.functions.get(&anchor).cloned()
    }

    pub fn find_type(&self, anchor: u32) -> Option<ExtensionType> {
        self.types.get(&anchor).cloned()
    }

    pub fn find_type_variation(&self, anchor: u32) -> Option<ExtensionTypeVariation> {
        self.type_variations.get(&anchor).cloned()
    }
}

#[derive(Debug, Clone)]
pub enum TextifyError {
    Fmt(fmt::Error),
    /// An invalid value was encountered; could not be converted to a string.
    /// Arguments are
    InvalidValue {
        name: String,
        context: String,
    },
}

impl From<fmt::Error> for TextifyError {
    fn from(e: fmt::Error) -> Self {
        TextifyError::Fmt(e)
    }
}

pub trait Textify {
    fn textify<W: fmt::Write>(
        &self,
        ctx: &mut OutputContext,
        w: &mut W,
    ) -> Result<(), TextifyError>;
}

impl Textify for ptype::Nullability {
    fn textify<W: fmt::Write>(
        &self,
        ctx: &mut OutputContext,
        w: &mut W,
    ) -> Result<(), TextifyError> {
        match self {
            ptype::Nullability::Unspecified => {
                if ctx.strict {
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
    for (_i, param) in params.into_iter().enumerate() {
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

#[macro_export]
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

pub struct MissingValue {
    name: String,
}

impl Textify for MissingValue {
    fn textify<W: fmt::Write>(
        &self,
        ctx: &mut OutputContext,
        w: &mut W,
    ) -> Result<(), TextifyError> {
        if ctx.strict {
            return Err(TextifyError::InvalidValue {
                name: format!("Missing argument {}", self.name),
                context: "Expected a value, found None".into(),
            });
        }
        write!(w, "{{{}}}", self.name)?;
        Ok(())
    }
}

pub enum ExpectedValue<T> {
    Value(T),
    Missing(MissingValue),
}

impl<T: fmt::Debug> ExpectedValue<T> {
    fn from_option(opt: Option<T>, name: impl Into<String>) -> Self {
        match opt {
            Some(t) => Self::Value(t),
            None => Self::Missing(MissingValue { name: name.into() }),
        }
    }
}

impl<T: Textify + fmt::Debug> Textify for ExpectedValue<T> {
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
        match self.parameter.as_ref() {
            Some(p) => p.textify(ctx, w),
            None => MissingValue {
                name: "Parameter".to_string(),
            }
            .textify(ctx, w),
        }
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
                [ExpectedValue::from_option(
                    l.r#type
                        .as_ref()
                        .map(|t| Parameter::DataType((**t).clone())),
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
                    ExpectedValue::from_option(
                        m.key.as_ref().map(|t| Parameter::DataType((**t).clone())),
                        "MAP_KEY",
                    ),
                    ExpectedValue::from_option(
                        m.value.as_ref().map(|t| Parameter::DataType((**t).clone())),
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
                    let m = MissingValue {
                        name: "UserDefined".to_string(),
                    };
                    m.textify(ctx, w)
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
