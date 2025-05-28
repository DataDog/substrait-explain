use std::cell::{RefCell, RefMut};
use std::fmt;
use std::ops::DerefMut;
use std::{borrow::Cow, collections::HashMap};

use substrait::proto::extensions as pext;

use pext::simple_extension_declaration::{
    ExtensionFunction, ExtensionType, ExtensionTypeVariation, MappingType,
};

const ERROR_MARKER_START: &str = "!{"; // "!❬";
const ERROR_MARKER_END: &str = "}"; // "❭";
pub const NONSPECIFIC: Option<&'static str> = None;

/// OutputOptions holds the options for textifying a Substrait type.
#[derive(Debug, Clone)]
pub struct OutputOptions {
    /// Show the extension URIs in the output.
    pub show_extension_uris: bool,
    /// Show the extensions in the output. By default, simple extensions are
    /// expanded into the input.
    pub show_simple_extensions: bool,
    /// Show the anchors of simple extensions in the output, and not just their
    /// names.
    pub show_simple_extension_anchors: bool,
    /// Instead of showing the emitted columns inline, show the emits directly.
    pub show_emit: bool,

    /// Show the types for columns in a read
    pub read_types: bool,
    /// Show the types for function parameters
    pub fn_types: bool,
    /// Show the nullability of types
    pub nullability: bool,
    /// The indent to use for nested types
    pub indent: String,
    /// Show the binary values for literal types as hex strings. Normally, they
    /// are shown as '{{binary}}'
    pub show_literal_binaries: bool,
}

impl Default for OutputOptions {
    fn default() -> Self {
        Self {
            show_extension_uris: false,
            show_simple_extensions: false,
            show_simple_extension_anchors: false,
            show_emit: false,
            read_types: false,
            fn_types: false,
            nullability: false,
            indent: "  ".to_string(),
            show_literal_binaries: false,
        }
    }
}

/// ExtensionLookup contains mappings from anchors to extension URIs, functions,
/// types, and type variations.
#[derive(Default, Debug, Clone)]
pub struct ExtensionLookup {
    // Maps from extension URI anchor to URI
    uris: HashMap<u32, pext::SimpleExtensionUri>,
    // Maps from function anchor to (extension URI anchor, function name)
    functions: HashMap<u32, ExtensionFunction>,
    // Maps from type anchor to (extension URI anchor, type name)
    types: HashMap<u32, ExtensionType>,
    // Maps from type variation anchor to (extension URI anchor, type variation name)
    type_variations: HashMap<u32, ExtensionTypeVariation>,
}

impl ExtensionLookup {
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
                    return Err(TextifyError::invalid(
                        // TODO: Use prost::Name here
                        "SimpleExtensionDeclaration",
                        NONSPECIFIC,
                        "Required MappingType unset",
                    ));
                }
            }
        }

        Ok(ExtensionLookup {
            uris: uri_map,
            functions,
            types,
            type_variations,
        })
    }

    #[cfg(test)]
    #[allow(dead_code)]
    pub(crate) fn add_extension_uri(&mut self, uri: String, anchor: u32) {
        self.uris.insert(
            anchor,
            pext::SimpleExtensionUri {
                extension_uri_anchor: anchor,
                uri,
            },
        );
    }

    #[cfg(test)]
    #[allow(dead_code)]
    pub(crate) fn add_extension_function(&mut self, uri: u32, anchor: u32, name: String) {
        self.functions.insert(
            anchor,
            ExtensionFunction {
                extension_uri_reference: uri,
                function_anchor: anchor,
                name,
            },
        );
    }

    #[cfg(test)]
    #[allow(dead_code)]
    pub(crate) fn add_extension_type(&mut self, uri: u32, anchor: u32, name: String) {
        self.types.insert(
            anchor,
            ExtensionType {
                extension_uri_reference: uri,
                type_anchor: anchor,
                name,
            },
        );
    }

    #[cfg(test)]
    #[allow(dead_code)]
    pub(crate) fn add_extension_type_variation(&mut self, uri: u32, anchor: u32, name: String) {
        self.type_variations.insert(
            anchor,
            ExtensionTypeVariation {
                extension_uri_reference: uri,
                type_variation_anchor: anchor,
                name,
            },
        );
    }
}

pub trait SimpleExtensions {
    fn find_uri(&self, anchor: u32) -> Option<pext::SimpleExtensionUri>;
    fn find_function(&self, anchor: u32) -> Option<ExtensionFunction>;
    fn find_type(&self, anchor: u32) -> Option<ExtensionType>;
    fn find_type_variation(&self, anchor: u32) -> Option<ExtensionTypeVariation>;

    fn find_functions(&self, name: &str) -> impl Iterator<Item = &ExtensionFunction>;
    fn find_types(&self, name: &str) -> impl Iterator<Item = &ExtensionType>;
    fn find_variations(&self, name: &str) -> impl Iterator<Item = &ExtensionTypeVariation>;
}

impl SimpleExtensions for ExtensionLookup {
    fn find_uri(&self, anchor: u32) -> Option<pext::SimpleExtensionUri> {
        self.uris.get(&anchor).cloned()
    }

    fn find_function(&self, anchor: u32) -> Option<ExtensionFunction> {
        self.functions.get(&anchor).cloned()
    }

    fn find_type(&self, anchor: u32) -> Option<ExtensionType> {
        self.types.get(&anchor).cloned()
    }

    fn find_type_variation(&self, anchor: u32) -> Option<ExtensionTypeVariation> {
        self.type_variations.get(&anchor).cloned()
    }

    fn find_functions(&self, name: &str) -> impl Iterator<Item = &ExtensionFunction> {
        self.functions.values().filter(move |f| f.name == name)
    }

    fn find_types(&self, name: &str) -> impl Iterator<Item = &ExtensionType> {
        self.types.values().filter(move |f| f.name == name)
    }

    fn find_variations(&self, name: &str) -> impl Iterator<Item = &ExtensionTypeVariation> {
        self.type_variations
            .values()
            .filter(move |f| f.name == name)
    }
}

pub trait ErrorAccumulator {
    fn push(&mut self, e: TextifyError);
}

#[derive(Default, Debug, Clone)]
pub struct ErrorVec(pub Vec<TextifyError>);

impl ErrorAccumulator for ErrorVec {
    fn push(&mut self, e: TextifyError) {
        self.0.push(e);
    }
}

impl fmt::Display for ErrorVec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut first = true;
        for e in &self.0 {
            if first {
                first = false;
            } else {
                write!(f, "\n")?;
            }
            write!(f, "{}", e)?;
        }
        Ok(())
    }
}

pub struct ScopedContext<'a, Err: ErrorAccumulator, Ext: SimpleExtensions> {
    errors: &'a mut Err,
    options: &'a OutputOptions,
    extensions: &'a Ext,
}

impl<'a, Err: ErrorAccumulator, Ext: SimpleExtensions> ScopedContext<'a, Err, Ext> {
    pub fn new(options: &'a OutputOptions, errors: &'a mut Err, extensions: &'a Ext) -> Self {
        Self {
            options,
            errors,
            extensions,
        }
    }

    pub fn options(&self) -> &OutputOptions {
        self.options
    }

    pub fn extensions(&self) -> &Ext {
        self.extensions
    }

    pub fn push_error(&mut self, e: TextifyError) {
        self.errors.push(e);
    }
}

impl<'a, Err: ErrorAccumulator, Ext: SimpleExtensions> ScopedContext<'a, Err, Ext> {
    /// Handle a failure to textify a value. Textify errors are written as
    /// "!{name}" to the output (where "name" is the type name), and
    /// the error is pushed to the error accumulator.
    pub fn failure<W: fmt::Write>(&mut self, w: &mut W, e: TextifyError) -> Result<(), fmt::Error> {
        match e.error_type {
            TextifyErrorType::Fmt => return Err(fmt::Error),
            _ => {
                write!(w, "{}{}", ERROR_MARKER_START, e.message)?;
                if let Some(ref lookup) = e.lookup {
                    write!(w, ": {}", lookup)?;
                }
                write!(w, "{}", ERROR_MARKER_END)?;
                self.errors.push(e.clone());
            }
        }
        Ok(())
    }

    pub fn expect<W: fmt::Write, T: Textify>(&mut self, w: &mut W, t: &Option<T>) -> fmt::Result {
        match t {
            Some(t) => t.textify(self, w),
            None => self.failure(
                w,
                TextifyError::invalid(T::name(), NONSPECIFIC, "Required field missing, None found"),
            ),
        }
    }
}

pub struct MessageWriter<'a, Err: ErrorAccumulator, Ext: SimpleExtensions, T> {
    context: RefCell<ScopedContext<'a, Err, Ext>>,
    message: &'a T,
}

impl<'a, Err: ErrorAccumulator, Ext: SimpleExtensions, T: Textify + prost::Message>
    MessageWriter<'a, Err, Ext, T>
{
    pub fn ctx(&self) -> Result<RefMut<ScopedContext<'a, Err, Ext>>, fmt::Error> {
        self.context.try_borrow_mut().map_err(|_| fmt::Error)
    }
}

impl<'a, Err: ErrorAccumulator, Ext: SimpleExtensions, T: Textify + prost::Message> fmt::Display
    for MessageWriter<'a, Err, Ext, T>
{
    fn fmt<'b>(&self, f: &mut fmt::Formatter<'b>) -> fmt::Result {
        let mut ctx_ref = self.ctx()?;
        self.message.textify(ctx_ref.deref_mut(), f)
    }
}

#[derive(Debug, Clone)]
pub struct TextifyError {
    // The message this originated in
    pub message: &'static str,
    // The specific lookup that failed, if specifiable
    pub lookup: Option<Cow<'static, str>>,
    // The description of the error
    pub description: Cow<'static, str>,
    // The error type
    pub error_type: TextifyErrorType,
}

impl TextifyError {
    pub fn invalid(
        message: &'static str,
        specific: Option<impl Into<Cow<'static, str>>>,
        description: impl Into<Cow<'static, str>>,
    ) -> Self {
        Self {
            message,
            lookup: specific.map(|s| s.into()),
            description: description.into(),
            error_type: TextifyErrorType::InvalidValue,
        }
    }

    pub fn unimplemented(
        message: &'static str,
        specific: Option<impl Into<Cow<'static, str>>>,
        description: impl Into<Cow<'static, str>>,
    ) -> Self {
        Self {
            message,
            lookup: specific.map(|s| s.into()),
            description: description.into(),
            error_type: TextifyErrorType::Unimplemented,
        }
    }

    pub fn internal(
        message: &'static str,
        specific: Option<impl Into<Cow<'static, str>>>,
        description: impl Into<Cow<'static, str>>,
    ) -> Self {
        Self {
            message,
            lookup: specific.map(|s| s.into()),
            description: description.into(),
            error_type: TextifyErrorType::Internal,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum TextifyErrorType {
    InvalidValue,
    Unimplemented,
    Internal,
    Fmt,
}

impl fmt::Display for TextifyErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TextifyErrorType::InvalidValue => write!(f, "InvalidValue"),
            TextifyErrorType::Unimplemented => write!(f, "Unimplemented"),
            TextifyErrorType::Internal => write!(f, "Internal"),
            TextifyErrorType::Fmt => write!(f, "Fmt"),
        }
    }
}

impl From<fmt::Error> for TextifyError {
    fn from(_: fmt::Error) -> Self {
        Self {
            message: "fmt",
            lookup: None,
            description: Cow::Borrowed("fmt error"),
            error_type: TextifyErrorType::Fmt,
        }
    }
}

impl std::error::Error for TextifyError {}

impl fmt::Display for TextifyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} Error writing {}: {}",
            self.error_type, self.message, self.description
        )
    }
}

/// A trait for types that can be textified.
///
/// This trait is used to convert a Substrait type into a string representation.
pub trait Textify {
    fn textify<'a, 'b, Err: ErrorAccumulator, Ext: SimpleExtensions, W: fmt::Write>(
        &self,
        ctx: &'b mut ScopedContext<'a, Err, Ext>,
        w: &mut W,
    ) -> fmt::Result;

    // TODO: Prost can give this to us if substrait was generated with `enable_type_names`
    // <https://docs.rs/prost-build/latest/prost_build/struct.Config.html#method.enable_type_names>
    fn name() -> &'static str;
}

// ($dst:expr, $($arg:tt)*) => {
//     $dst.write_fmt($crate::format_args!($($arg)*))
// };

// macro_rules! textify {
//     ($ctx:expr, $w:expr, $($arg:tt)*) => {

//     };
// }
