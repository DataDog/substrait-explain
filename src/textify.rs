use std::collections::HashMap;
use std::fmt;

use substrait::proto::extensions as pext;

use pext::simple_extension_declaration::{
    ExtensionFunction, ExtensionType, ExtensionTypeVariation, MappingType,
};

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
    /// Error on unspecified fields
    pub strict: bool,
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
            strict: false,
            indent: "  ".to_string(),
            show_literal_binaries: false,
        }
    }
}

/// OutputContext holds the context for textifying a Substrait type.
#[derive(Default, Debug, Clone)]
pub struct OutputContext {
    // Maps from extension URI anchor to URI
    uris: HashMap<u32, pext::SimpleExtensionUri>,
    // Maps from function anchor to (extension URI anchor, function name)
    functions: HashMap<u32, ExtensionFunction>,
    // Maps from type anchor to (extension URI anchor, type name)
    types: HashMap<u32, ExtensionType>,
    // Maps from type variation anchor to (extension URI anchor, type variation name)
    type_variations: HashMap<u32, ExtensionTypeVariation>,

    options: OutputOptions,
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

    pub fn options(&self) -> &OutputOptions {
        &self.options
    }

    // pub fn fmt<T: Textify>(&self, t: &T) -> TextifyWriter<T> {
    //     TextifyWriter {
    //         ctx: self,
    //         value: &t,
    //     }
    // }
}

pub trait SimpleExtensions {
    fn find_uri(&self, anchor: u32) -> Option<pext::SimpleExtensionUri>;
    fn find_function(&self, anchor: u32) -> Option<ExtensionFunction>;
    fn find_type(&self, anchor: u32) -> Option<ExtensionType>;
    fn find_type_variation(&self, anchor: u32) -> Option<ExtensionTypeVariation>;
}

impl SimpleExtensions for OutputContext {
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
}

pub struct TextifyWriter<'a, T: Textify + 'a> {
    ctx: &'a mut OutputContext,
    value: &'a T,
}

impl<'a, T: Textify> fmt::Display for TextifyWriter<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.textify(&mut self.ctx.clone(), f)
    }
}

#[derive(Debug, Clone)]
pub enum TextifyError {
    /// An invalid value was encountered; could not be converted to a string.
    InvalidValue {
        // TODO: figure out the arguments here
        name: String,
        context: String,
    },
    Unimplemented(String),

    Internal(String),
}

impl std::error::Error for TextifyError {}

impl fmt::Display for TextifyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TextifyError::InvalidValue { name, context } => {
                write!(f, "Invalid value: {} ({})", name, context)
            }
            TextifyError::Unimplemented(s) => write!(f, "Unimplemented: {}", s),
            TextifyError::Internal(s) => write!(f, "Internal error: {}", s),
        }
    }
}

impl From<fmt::Error> for TextifyError {
    fn from(e: fmt::Error) -> Self {
        TextifyError::Internal(format!("fmt error: {}", e))
    }
}

/// A trait for types that can be textified.
///
/// This trait is used to convert a Substrait type into a string representation.
pub trait Textify {
    fn textify<W: fmt::Write>(
        &self,
        ctx: &mut OutputContext,
        w: &mut W,
    ) -> Result<(), TextifyError>;
}

// ($dst:expr, $($arg:tt)*) => {
//     $dst.write_fmt($crate::format_args!($($arg)*))
// };

// macro_rules! textify {
//     ($ctx:expr, $w:expr, $($arg:tt)*) => {

//     };
// }
