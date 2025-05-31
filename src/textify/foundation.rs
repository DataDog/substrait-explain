use std::borrow::Cow;
use std::cell::{RefCell, RefMut};
use std::fmt;
use std::ops::DerefMut;

use crate::extensions::ExtensionLookup;

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

pub trait IndentTracker {
    fn indent<W: fmt::Write>(&self, w: &mut W) -> fmt::Result;
    fn push(self) -> Self;
}

pub struct IndentStack<'a> {
    count: u32,
    indent: &'a str,
}

pub struct ScopedContext<'a, Err: ErrorAccumulator, Ext: ExtensionLookup> {
    errors: &'a mut Err,
    options: &'a OutputOptions,
    extensions: &'a Ext,
    indent: IndentStack<'a>,
}

impl<'a> IndentStack<'a> {
    pub fn new(indent: &'a str) -> Self {
        Self { count: 0, indent }
    }
}

impl<'a> IndentTracker for IndentStack<'a> {
    fn indent<W: fmt::Write>(&self, w: &mut W) -> fmt::Result {
        for _ in 0..self.count {
            w.write_str(self.indent)?;
        }
        Ok(())
    }

    fn push(mut self) -> Self {
        self.count += 1;
        self
    }
}

impl<'a, Err: ErrorAccumulator, Ext: ExtensionLookup> ScopedContext<'a, Err, Ext> {
    pub fn new(options: &'a OutputOptions, errors: &'a mut Err, extensions: &'a Ext) -> Self {
        Self {
            options,
            errors,
            extensions,
            indent: IndentStack::new(options.indent.as_str()),
        }
    }
}

pub struct MessageWriter<'a, Err: ErrorAccumulator, Ext: ExtensionLookup, T> {
    context: RefCell<ScopedContext<'a, Err, Ext>>,
    message: &'a T,
}

impl<'a, Err: ErrorAccumulator, Ext: ExtensionLookup, T: Textify + prost::Message>
    MessageWriter<'a, Err, Ext, T>
{
    pub fn ctx(&self) -> Result<RefMut<ScopedContext<'a, Err, Ext>>, fmt::Error> {
        self.context.try_borrow_mut().map_err(|_| fmt::Error)
    }
}

impl<'a, Err: ErrorAccumulator, Ext: ExtensionLookup, T: Textify + prost::Message> fmt::Display
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
    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &mut S, w: &mut W) -> fmt::Result;

    // TODO: Prost can give this to us if substrait was generated with `enable_type_names`
    // <https://docs.rs/prost-build/latest/prost_build/struct.Config.html#method.enable_type_names>
    fn name() -> &'static str;
}

pub trait Scope: Sized {
    type Errors: ErrorAccumulator;
    type Extensions: ExtensionLookup;
    type Indent: IndentTracker;

    fn push_indent(self) -> Self;

    fn options(&self) -> &OutputOptions;
    fn extensions(&self) -> &Self::Extensions;
    fn errors(&mut self) -> &mut Self::Errors;

    fn push_error(&mut self, e: TextifyError) {
        self.errors().push(e);
    }

    /// Handle a failure to textify a value. Textify errors are written as
    /// "!{name}" to the output (where "name" is the type name), and
    /// the error is pushed to the error accumulator.
    fn failure<W: fmt::Write>(&mut self, w: &mut W, e: TextifyError) -> Result<(), fmt::Error> {
        match e.error_type {
            TextifyErrorType::Fmt => return Err(fmt::Error),
            _ => {
                write!(w, "{}{}", ERROR_MARKER_START, e.message)?;
                if let Some(ref lookup) = e.lookup {
                    write!(w, ": {}", lookup)?;
                }
                write!(w, "{}", ERROR_MARKER_END)?;
                self.errors().push(e.clone());
            }
        }
        Ok(())
    }

    fn expect<W: fmt::Write, T: Textify>(&mut self, w: &mut W, t: &Option<T>) -> fmt::Result {
        match t {
            Some(t) => t.textify(self, w),
            None => self.failure(
                w,
                TextifyError::invalid(T::name(), NONSPECIFIC, "Required field missing, None found"),
            ),
        }
    }
}

impl<'a, Err: ErrorAccumulator, Ext: ExtensionLookup> Scope for ScopedContext<'a, Err, Ext> {
    type Errors = Err;
    type Extensions = Ext;
    type Indent = IndentStack<'a>;

    fn push_indent(mut self) -> Self {
        self.indent = self.indent.push();
        self
    }

    fn options(&self) -> &OutputOptions {
        self.options
    }

    fn extensions(&self) -> &Self::Extensions {
        self.extensions
    }

    fn errors(&mut self) -> &mut Self::Errors {
        self.errors
    }
}
