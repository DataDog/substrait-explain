use std::fmt;
use std::ops::Deref;

use ptype::parameter::Parameter;
use substrait::proto;
use substrait::proto::r#type::{self as ptype};

use super::foundation::{NONSPECIFIC, Scope};
use super::{Textify, TextifyError};
use crate::extensions::simple::ExtensionKind;
use crate::textify::foundation::{ErrorToken, MaybeToken, Visibility};

const NULLABILITY_UNSPECIFIED: &str = "⁉";

impl Textify for ptype::Nullability {
    fn name() -> &'static str {
        "Nullability"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        match self {
            ptype::Nullability::Unspecified => {
                ctx.push_error(
                    TextifyError::invalid(
                        "Nullability",
                        NONSPECIFIC,
                        "Nullability left Unspecified",
                    )
                    .into(),
                );

                // TODO: what should unspecified Nullabilitylook like?
                w.write_str(NULLABILITY_UNSPECIFIED)?;
            }
            ptype::Nullability::Nullable => write!(w, "?")?,
            ptype::Nullability::Required => {}
        };
        Ok(())
    }
}

/// A valid identifier is a sequence of ASCII letters, digits, and underscores,
/// starting with a letter.
///
/// We could expand this at some point to include any valid Unicode identifier
/// (see <https://docs.rs/unicode-ident/latest/unicode_ident/>), but that seems
/// overboard for now.
pub fn is_identifer(s: &str) -> bool {
    let mut chars = s.chars();
    let first = match chars.next() {
        Some(c) => c,
        None => return false,
    };

    if !first.is_ascii_alphabetic() {
        return false;
    }

    for c in chars {
        if !c.is_ascii_alphanumeric() && c != '_' {
            return false;
        }
    }

    true
}

/// Escape a string for use in a literal or quoted identifier.
pub fn escaped(s: &str) -> impl fmt::Display + fmt::Debug {
    s.escape_debug()
}

/// The name of a something to be represented. It will be displayed on its own
/// if the string is a proper identifier, or in double quotes if it is not.
#[derive(Debug, Copy, Clone)]
pub struct Name<'a>(pub &'a str);

impl<'a> fmt::Display for Name<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if is_identifer(self.0) {
            write!(f, "{}", self.0)
        } else {
            write!(f, "\"{}\"", escaped(self.0))
        }
    }
}

impl<'a> Textify for Name<'a> {
    fn name() -> &'static str {
        "Name"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, _ctx: &S, w: &mut W) -> fmt::Result {
        write!(w, "{self}")
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Anchor {
    reference: u32,
    required: bool,
}

impl Anchor {
    pub fn new(reference: u32, required: bool) -> Self {
        Self {
            reference,
            required,
        }
    }
}

impl Textify for Anchor {
    fn name() -> &'static str {
        "Anchor"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        match ctx.options().show_simple_extension_anchors {
            Visibility::Never => return Ok(()),
            Visibility::Required if !self.required => {
                return Ok(());
            }
            Visibility::Required => {}
            Visibility::Always => {}
        }
        write!(w, "#{}", self.reference)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct NamedAnchor<'a> {
    pub name: MaybeToken<&'a str>,
    pub anchor: u32,
    // True if the name is valid and unique for the extension kind, false if not.
    pub unique: bool,
}

impl<'a> NamedAnchor<'a> {
    /// Lookup an anchor in the extensions, and return a NamedAnchor. Errors will be pushed to the ErrorAccumulator along the way.
    pub fn lookup<S: Scope>(ctx: &'a S, kind: ExtensionKind, anchor: u32) -> Self {
        let ext = ctx.extensions().find_by_anchor(kind, anchor);
        let found = ext.map_err(|e| ctx.push_error(e.into())).ok();

        let (name, unique) = match found {
            Some((_, n)) => match ctx.extensions().is_name_unique(kind, anchor, n) {
                // Nothing wrong; may or may not be unique.
                Ok(unique) => (MaybeToken(Ok(n)), unique),
                Err(e) => {
                    ctx.push_error(e.into());
                    (MaybeToken(Err(ErrorToken(kind.name()))), false)
                }
            },
            None => (MaybeToken(Err(ErrorToken(kind.name()))), false),
        };
        Self {
            name,
            anchor,
            unique,
        }
    }
}

impl<'a> Textify for NamedAnchor<'a> {
    fn name() -> &'static str {
        "NamedAnchor"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        let anchor = Anchor::new(self.anchor, !self.unique);
        write!(
            w,
            "{name}{anchor}",
            name = self.name,
            anchor = ctx.display(&anchor)
        )
    }
}

/// The type desciptor of the output of a function call.
///
/// This is optional, and if present, it must be the last argument in the
/// function call.
#[derive(Debug, Copy, Clone)]
pub struct OutputType<T: Deref<Target = proto::Type>>(pub Option<T>);

impl<T: Deref<Target = proto::Type>> Textify for OutputType<T> {
    fn name() -> &'static str {
        "OutputType"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        match self.0 {
            Some(ref t) => write!(w, ":{}", ctx.display(t.deref())),
            None => Ok(()),
        }
    }
}

struct TypeVariation(u32);

impl Textify for TypeVariation {
    fn name() -> &'static str {
        "TypeVariation"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        let &TypeVariation(anchor) = self;
        if anchor == 0 {
            // This is the default, this doesn't count as a type variation
            return Ok(());
        }
        let name_and_anchor = NamedAnchor::lookup(ctx, ExtensionKind::TypeVariation, anchor);

        write!(
            w,
            "[{name_and_anchor}]",
            name_and_anchor = ctx.display(&name_and_anchor)
        )
    }
}

// Textify a standard type with parameters.
//
// P will generally be the Parameter type, but it can be any type that
// implements Textify.
fn textify_type<S: Scope, W: fmt::Write>(
    ctx: &S,
    f: &mut W,
    name: impl AsRef<str>,
    nullability: ptype::Nullability,
    variant: u32,
    params: Parameters,
) -> fmt::Result {
    write!(
        f,
        "{name}{null}{var}{params}",
        name = name.as_ref(),
        null = ctx.display(&nullability),
        var = ctx.display(&TypeVariation(variant)),
        params = ctx.display(&params)
    )
}

macro_rules! textify_kind {
    ($ctx:expr, $f:expr, $kind:ident, $name:expr) => {
        textify_type(
            $ctx,
            $f,
            $name,
            $kind.nullability(),
            $kind.type_variation_reference,
            Parameters(&[]),
        )
    };
}

impl Textify for Parameter {
    fn name() -> &'static str {
        "Parameter"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        match self {
            Parameter::Boolean(true) => write!(w, "true")?,
            Parameter::Boolean(false) => write!(w, "false")?,
            Parameter::DataType(t) => write!(w, "{}", ctx.display(t))?,
            Parameter::Enum(e) => write!(w, "{e}")?,
            Parameter::Integer(i) => write!(w, "{i}")?,
            // TODO: Do we just put the string in directly?
            Parameter::String(s) => write!(w, "{s}")?,
            Parameter::Null(_) => write!(w, "null")?,
        };

        Ok(())
    }
}
impl Textify for ptype::Parameter {
    fn name() -> &'static str {
        "Parameter"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        write!(w, "{}", ctx.expect(self.parameter.as_ref()))
    }
}

struct Parameters<'a>(&'a [Option<Parameter>]);

impl<'a> Textify for Parameters<'a> {
    fn name() -> &'static str {
        "Parameters"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        let mut first = true;
        for param in self.0.iter() {
            if first {
                write!(w, "<")?;
            } else {
                write!(w, ", ")?;
            }
            write!(w, "{}", ctx.expect(param.as_ref()))?;
            first = false;
        }
        if !first {
            write!(w, ">")?;
        }

        Ok(())
    }
}

impl Textify for ptype::UserDefined {
    fn name() -> &'static str {
        "UserDefined"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        {
            let name_and_anchor =
                NamedAnchor::lookup(ctx, ExtensionKind::Type, self.type_reference);

            let param_vec: Vec<Option<Parameter>> = self
                .type_parameters
                .iter()
                .map(|t| t.parameter.clone())
                .collect();
            let params = Parameters(&param_vec);

            write!(
                w,
                "{name_and_anchor}{null}{var}{params}",
                name_and_anchor = ctx.display(&name_and_anchor),
                null = ctx.display(&self.nullability()),
                var = ctx.display(&TypeVariation(self.type_variation_reference)),
                params = ctx.display(&params)
            )
        }
    }
}

impl Textify for ptype::Kind {
    fn name() -> &'static str {
        "Kind"
    }

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
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
                Parameters(&[]),
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
                Parameters(&[Some(Parameter::Integer(i.precision.unwrap_or(6) as i64))]),
            ),
            ptype::Kind::IntervalCompound(i) => textify_type(
                ctx,
                w,
                "interval_compound",
                i.nullability(),
                i.type_variation_reference,
                Parameters(&[Some(Parameter::Integer(i.precision as i64))]),
            ),
            ptype::Kind::FixedChar(c) => textify_type(
                ctx,
                w,
                "fixedchar",
                c.nullability(),
                c.type_variation_reference,
                Parameters(&[Some(Parameter::Integer(c.length as i64))]),
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
                Parameters(&[Some(Parameter::Integer(p.precision as i64))]),
            ),
            ptype::Kind::PrecisionTimestamp(p) => textify_type(
                ctx,
                w,
                "precisiontimestamp",
                p.nullability(),
                p.type_variation_reference,
                Parameters(&[Some(Parameter::Integer(p.precision as i64))]),
            ),
            ptype::Kind::PrecisionTimestampTz(_p) => todo!(),
            ptype::Kind::Struct(s) => textify_type(
                ctx,
                w,
                "struct",
                s.nullability(),
                s.type_variation_reference,
                Parameters(
                    &s.types
                        .iter()
                        .map(|t| Some(Parameter::DataType(t.clone())))
                        .collect::<Vec<_>>(),
                ),
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
                    Parameters(&[p]),
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
                    Parameters(&[k, v]),
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

    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
        write!(w, "{}", ctx.expect(self.kind.as_ref()))
    }
}

// /// A schema is a named struct with a list of fields.
// ///
// /// This outputs the names and types of the fields in the struct,
// /// comma-separated.
// ///
// /// Assumes that the struct is not nullable, that the type variation reference
// /// is 0, and that the names and fields match up; otherwise, pushes errors.
// ///
// /// Names and fields are output without any bracketing; bring your own
// /// bracketing.
// pub struct Schema<'a>(pub &'a proto::NamedStruct);

// impl<'a> Textify for Schema<'a> {
//     fn name() -> &'static str {
//         "Schema"
//     }

//     fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result {
//         let mut fields = self
//             .0
//             .r#struct
//             .as_ref()
//             .map(|s| s.types.iter())
//             .into_iter()
//             .flatten();
//         let mut names = self.0.names.iter();

//         let field_count = self.0.r#struct.as_ref().map(|s| s.types.len()).unwrap_or(0);
//         let name_count = self.0.names.len();

//         if field_count != name_count {
//             ctx.push_error(
//                 TextifyError::invalid(
//                     "Schema",
//                     NONSPECIFIC,
//                     format!(
//                         "Field count ({}) does not match name count ({})",
//                         field_count, name_count
//                     ),
//                 )
//                 .into(),
//             );
//         }

//         write!(w, "[")?;
//         let mut first = true;
//         loop {
//             let field = fields.next();
//             let name = names.next().map(|n| Name(n));
//             if field.is_none() && name.is_none() {
//                 break;
//             }

//             if first {
//                 first = false;
//             } else {
//                 write!(w, ", ")?;
//             }

//             write!(w, "{}:{}", ctx.expect(name.as_ref()), ctx.expect(field))?;
//         }
//         write!(w, "]")?;

//         let s = match &self.0.r#struct {
//             None => return Ok(()),
//             Some(s) => s,
//         };

//         if s.nullability() != Nullability::Required {
//             ctx.push_error(
//                 TextifyError::invalid(
//                     "Schema",
//                     Some("nullabilility"),
//                     "Expected schema to be Nullability::Required",
//                 )
//                 .into(),
//             );
//             s.nullability().textify(ctx, w)?;
//         }
//         if s.type_variation_reference != 0 {
//             ctx.push_error(
//                 TextifyError::invalid(
//                     "Schema",
//                     Some("type_variation_reference"),
//                     "Expected schema to have type_variation_reference 0",
//                 )
//                 .into(),
//             );
//             TypeVariation(s.type_variation_reference).textify(ctx, w)?;
//         }

//         Ok(())
//     }
// }

#[cfg(test)]
mod tests {

    use super::*;
    use crate::extensions::simple::{ExtensionKind, MissingReference};
    use crate::fixtures::TestContext;
    use crate::textify::foundation::Error;

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

        let s = ctx.textify_no_errors(&t);
        assert_eq!(s, "boolean?[u8]");

        let t = proto::Type {
            kind: Some(ptype::Kind::I8(ptype::I8 {
                type_variation_reference: 0,
                nullability: ptype::Nullability::Required as i32,
            })),
        };
        assert_eq!(ctx.textify_no_errors(&t), "i8");

        let t = proto::Type {
            kind: Some(ptype::Kind::PrecisionTimestamp(ptype::PrecisionTimestamp {
                type_variation_reference: 0,
                nullability: ptype::Nullability::Nullable as i32,
                precision: 3,
            })),
        };
        assert_eq!(ctx.textify_no_errors(&t), "precisiontimestamp?<3>");

        let mut ctx = ctx.with_type_variation(1, 8, "int");
        ctx.options.show_simple_extension_anchors = Visibility::Always;

        let t = proto::Type {
            kind: Some(ptype::Kind::PrecisionTime(ptype::PrecisionTime {
                type_variation_reference: 8,
                nullability: ptype::Nullability::Nullable as i32,
                precision: 9,
            })),
        };
        assert_eq!(ctx.textify_no_errors(&t), "precisiontime?[int#8]<9>");
    }

    #[test]
    fn type_display_with_errors() {
        let ctx = TestContext::new()
            .with_uri(1, "first")
            .with_type(1, 100, "cow");

        let t = proto::Type {
            kind: Some(ptype::Kind::Bool(ptype::Boolean {
                type_variation_reference: 200,
                nullability: ptype::Nullability::Nullable as i32,
            })),
        };
        let (s, errs) = ctx.textify(&t);
        assert_eq!(s, "boolean?[!{type_variation}#200]");
        let err = errs.first();
        let (&k, &a) = match err {
            Error::Lookup(MissingReference::MissingAnchor(k, a)) => (k, a),
            _ => panic!("Expected Lookup MissingAnchor: {}", err),
        };

        assert_eq!(k, ExtensionKind::TypeVariation);
        assert_eq!(a, 200);

        let t = proto::Type {
            kind: Some(ptype::Kind::UserDefined(ptype::UserDefined {
                type_variation_reference: 0,
                nullability: ptype::Nullability::Required as i32,
                type_reference: 100,
                type_parameters: vec![],
            })),
        };

        let (s, errs) = ctx.textify(&t);
        assert!(errs.is_empty());
        assert_eq!(s, "cow");

        let t = proto::Type {
            kind: Some(ptype::Kind::UserDefined(ptype::UserDefined {
                type_variation_reference: 0,
                nullability: ptype::Nullability::Required as i32,
                type_reference: 12589,
                type_parameters: vec![],
            })),
        };

        let (s, errs) = ctx.textify(&t);
        let err = errs.first();
        let (&k, &a) = match err {
            Error::Lookup(MissingReference::MissingAnchor(k, a)) => (k, a),
            _ => panic!("Expected Lookup MissingAnchor: {}", err),
        };
        assert_eq!(k, ExtensionKind::Type);
        assert_eq!(a, 12589);
        assert_eq!(s, "!{type}#12589");
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
            ctx.textify_no_errors(&t),
            "struct?<string, i8, i32?, timestamp_tz>"
        );
    }

    #[test]
    fn names_display() {
        let ctx = TestContext::new();

        let n = Name("name");
        assert_eq!(ctx.textify_no_errors(&n), "name");

        let n = Name("name with spaces");
        assert_eq!(ctx.textify_no_errors(&n), "\"name with spaces\"");
    }

    // #[test]
    // fn schema_display() {
    //     let ctx = TestContext::new();

    //     let s = ptype::Struct {
    //         type_variation_reference: 0,
    //         nullability: ptype::Nullability::Required as i32,
    //         types: vec![
    //             proto::Type {
    //                 kind: Some(ptype::Kind::String(ptype::String {
    //                     type_variation_reference: 0,
    //                     nullability: ptype::Nullability::Required as i32,
    //                 })),
    //             },
    //             proto::Type {
    //                 kind: Some(ptype::Kind::I8(ptype::I8 {
    //                     type_variation_reference: 0,
    //                     nullability: ptype::Nullability::Required as i32,
    //                 })),
    //             },
    //             proto::Type {
    //                 kind: Some(ptype::Kind::I32(ptype::I32 {
    //                     type_variation_reference: 0,
    //                     nullability: ptype::Nullability::Nullable as i32,
    //                 })),
    //             },
    //             proto::Type {
    //                 kind: Some(ptype::Kind::TimestampTz(ptype::TimestampTz {
    //                     type_variation_reference: 0,
    //                     nullability: ptype::Nullability::Required as i32,
    //                 })),
    //             },
    //         ],
    //     };

    //     let names = ["a", "b", "c", "d"].iter().map(|s| s.to_string()).collect();
    //     let schema = proto::NamedStruct {
    //         names,
    //         r#struct: Some(s),
    //     };

    //     assert_eq!(
    //         ctx.textify_no_errors(&Schema(&schema)),
    //         "[a:string, b:i8, c:i32?, d:timestamp_tz]"
    //     );
    // }

    // #[test]
    // fn schema_display_with_errors() {
    //     let ctx = TestContext::new();
    //     let string = proto::Type {
    //         kind: Some(ptype::Kind::String(ptype::String {
    //             type_variation_reference: 0,
    //             nullability: ptype::Nullability::Required as i32,
    //         })),
    //     };
    //     let i64 = proto::Type {
    //         kind: Some(ptype::Kind::I8(ptype::I8 {
    //             type_variation_reference: 0,
    //             nullability: ptype::Nullability::Nullable as i32,
    //         })),
    //     };
    //     let fp64 = proto::Type {
    //         kind: Some(ptype::Kind::Fp64(ptype::Fp64 {
    //             type_variation_reference: 0,
    //             nullability: ptype::Nullability::Nullable as i32,
    //         })),
    //     };

    //     let s = ptype::Struct {
    //         type_variation_reference: 0,
    //         nullability: ptype::Nullability::Required as i32,
    //         types: vec![string.clone(), i64, fp64, string],
    //     };

    //     let names = ["name", "id", "distance", "street address"]
    //         .iter()
    //         .map(|s| s.to_string())
    //         .collect();
    //     let schema = proto::NamedStruct {
    //         names,
    //         r#struct: Some(s),
    //     };

    //     let (s, errs) = ctx.textify(&Schema(&schema));
    //     assert_eq!(
    //         s,
    //         "name:string, id:i8?, distance:fp64?, \"street address\":string"
    //     );
    //     assert!(errs.is_empty());
    // }
}
