//! Foundation types for textifying a plan.

use std::borrow::Cow;
use std::fmt;
use std::rc::Rc;
use std::sync::mpsc;

use thiserror::Error;

use crate::extensions::simple::MissingReference;
use crate::extensions::{InsertError, SimpleExtensions};

pub const NONSPECIFIC: Option<&'static str> = None;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Visibility {
    /// Never show the information
    Never,
    /// Show the information if it is required for the output to be complete.
    Required,
    /// Show it always.
    Always,
}

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
    ///
    /// If `Required`, the anchor is shown for all simple extensions.
    pub show_simple_extension_anchors: Visibility,
    /// Instead of showing the emitted columns inline, show the emits directly.
    pub show_emit: bool,

    /// Show the types for columns in a read
    pub read_types: bool,
    /// Show the types for literals. If `Required`, the type is shown for anything other than
    /// `i64`, `fp64`, `boolean`, or `string`.
    pub literal_types: Visibility,
    /// Show the output types for functions
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
            show_simple_extension_anchors: Visibility::Required,
            literal_types: Visibility::Required,
            show_emit: false,
            read_types: false,
            fn_types: false,
            nullability: false,
            indent: "  ".to_string(),
            show_literal_binaries: false,
        }
    }
}

impl OutputOptions {
    /// A verbose output options that shows all the necessary information for
    /// reconstructing a plan.
    pub fn verbose() -> Self {
        Self {
            show_extension_uris: true,
            show_simple_extensions: true,
            show_simple_extension_anchors: Visibility::Always,
            literal_types: Visibility::Always,
            // Emits are not required for a complete plan - just not a precise one.
            show_emit: false,
            read_types: true,
            fn_types: true,
            nullability: true,
            indent: "  ".to_string(),
            show_literal_binaries: true,
        }
    }
}
pub trait ErrorAccumulator: Clone {
    fn push(&self, e: FormatError);
}

#[derive(Debug, Clone)]
pub struct ErrorQueue {
    sender: mpsc::Sender<FormatError>,
    receiver: Rc<mpsc::Receiver<FormatError>>,
}

impl Default for ErrorQueue {
    fn default() -> Self {
        let (sender, receiver) = mpsc::channel();
        Self {
            sender,
            receiver: Rc::new(receiver),
        }
    }
}

impl From<ErrorQueue> for Vec<FormatError> {
    fn from(v: ErrorQueue) -> Vec<FormatError> {
        v.receiver.try_iter().collect()
    }
}

impl ErrorAccumulator for ErrorQueue {
    fn push(&self, e: FormatError) {
        self.sender.send(e).unwrap();
    }
}

impl fmt::Display for ErrorQueue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut first = true;
        for e in self.receiver.try_iter() {
            if first {
                first = false;
            } else {
                writeln!(f)?;
            }
            write!(f, "{e}")?;
        }
        Ok(())
    }
}

impl ErrorQueue {
    pub fn errs(self) -> Result<(), ErrorList> {
        let errors: Vec<FormatError> = self.receiver.try_iter().collect();
        if errors.is_empty() {
            Ok(())
        } else {
            Err(ErrorList(errors))
        }
    }
}

// A list of errors, used to return errors from textify operations.
pub struct ErrorList(pub Vec<FormatError>);

impl ErrorList {
    pub fn first(&self) -> &FormatError {
        self.0
            .first()
            .expect("Expected at least one error in ErrorList")
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl fmt::Display for ErrorList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, e) in self.0.iter().enumerate() {
            if i > 0 {
                writeln!(f)?;
            }
            write!(f, "{e}")?;
        }
        Ok(())
    }
}

impl fmt::Debug for ErrorList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, e) in self.0.iter().enumerate() {
            if i == 0 {
                writeln!(f, "Errors:")?;
            }
            writeln!(f, "! {e:?}")?;
        }
        Ok(())
    }
}

impl<'e> IntoIterator for &'e ErrorQueue {
    type Item = FormatError;
    type IntoIter = std::sync::mpsc::TryIter<'e, FormatError>;

    fn into_iter(self) -> Self::IntoIter {
        self.receiver.try_iter()
    }
}

pub trait IndentTracker {
    // TODO: Use this and remove the allow(dead_code)
    #[allow(dead_code)]
    fn indent<W: fmt::Write>(&self, w: &mut W) -> fmt::Result;
    fn push(self) -> Self;
}

#[derive(Debug, Copy, Clone)]
pub struct IndentStack<'a> {
    count: u32,
    indent: &'a str,
}

impl<'a> fmt::Display for IndentStack<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for _ in 0..self.count {
            f.write_str(self.indent)?;
        }
        Ok(())
    }
}

#[derive(Debug, Copy, Clone)]
pub struct ScopedContext<'a, Err: ErrorAccumulator> {
    errors: &'a Err,
    options: &'a OutputOptions,
    extensions: &'a SimpleExtensions,
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

impl<'a, Err: ErrorAccumulator> ScopedContext<'a, Err> {
    pub fn new(
        options: &'a OutputOptions,
        errors: &'a Err,
        extensions: &'a SimpleExtensions,
    ) -> Self {
        Self {
            options,
            errors,
            extensions,
            indent: IndentStack::new(options.indent.as_str()),
        }
    }
}

/// Errors that can occur when formatting a plan.
#[derive(Error, Debug, Clone)]
pub enum FormatError {
    /// Error in adding extensions to the plan - e.g. duplicates, invalid URI
    /// references, etc.
    #[error("Error adding extension: {0}")]
    Insert(#[from] InsertError),
    /// Error in looking up an extension - e.g. missing URI, anchor, name, etc.
    #[error("Error finding extension: {0}")]
    Lookup(#[from] MissingReference),
    /// Error in formatting the plan - e.g. invalid value, unimplemented, etc.
    #[error("Error formatting output: {0}")]
    Format(#[from] PlanError),
}

impl FormatError {
    pub fn message(&self) -> &'static str {
        match self {
            FormatError::Lookup(MissingReference::MissingUri(_)) => "uri",
            FormatError::Lookup(MissingReference::MissingAnchor(k, _)) => k.name(),
            FormatError::Lookup(MissingReference::MissingName(k, _)) => k.name(),
            FormatError::Lookup(MissingReference::Mismatched(k, _, _)) => k.name(),
            FormatError::Lookup(MissingReference::DuplicateName(k, _)) => k.name(),
            FormatError::Format(m) => m.message,
            FormatError::Insert(InsertError::MissingMappingType) => "extension",
            FormatError::Insert(InsertError::DuplicateUriAnchor { .. }) => "uri",
            FormatError::Insert(InsertError::DuplicateAnchor { .. }) => "extension",
            FormatError::Insert(InsertError::MissingUri { .. }) => "uri",
            FormatError::Insert(InsertError::DuplicateAndMissingUri { .. }) => "uri",
        }
    }
}

#[derive(Debug, Clone)]
pub struct PlanError {
    // The message this originated in
    pub message: &'static str,
    // The specific lookup that failed, if specifiable
    pub lookup: Option<Cow<'static, str>>,
    // The description of the error
    pub description: Cow<'static, str>,
    // The error type
    pub error_type: FormatErrorType,
}

impl PlanError {
    pub fn invalid(
        message: &'static str,
        specific: Option<impl Into<Cow<'static, str>>>,
        description: impl Into<Cow<'static, str>>,
    ) -> Self {
        Self {
            message,
            lookup: specific.map(|s| s.into()),
            description: description.into(),
            error_type: FormatErrorType::InvalidValue,
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
            error_type: FormatErrorType::Unimplemented,
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
            error_type: FormatErrorType::Internal,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub enum FormatErrorType {
    InvalidValue,
    Unimplemented,
    Internal,
}

impl fmt::Display for FormatErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FormatErrorType::InvalidValue => write!(f, "InvalidValue"),
            FormatErrorType::Unimplemented => write!(f, "Unimplemented"),
            FormatErrorType::Internal => write!(f, "Internal"),
        }
    }
}

impl std::error::Error for PlanError {}

impl fmt::Display for PlanError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} Error writing {}: {}",
            self.error_type, self.message, self.description
        )
    }
}

#[derive(Debug, Copy, Clone)]
/// A token used to represent an error in the textified output.
pub struct ErrorToken(
    /// The kind of item this is in place of.
    pub &'static str,
);

impl fmt::Display for ErrorToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "!{{{}}}", self.0)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct MaybeToken<V: fmt::Display>(pub Result<V, ErrorToken>);

impl<V: fmt::Display> fmt::Display for MaybeToken<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            Ok(t) => t.fmt(f),
            Err(e) => e.fmt(f),
        }
    }
}

/// A trait for types that can be output in text format.
///
/// This trait is used to convert a Substrait type into a string representation
/// that can be written to a text writer.
pub trait Textify {
    fn textify<S: Scope, W: fmt::Write>(&self, ctx: &S, w: &mut W) -> fmt::Result;

    // TODO: Prost can give this to us if substrait was generated with `enable_type_names`
    // <https://docs.rs/prost-build/latest/prost_build/struct.Config.html#method.enable_type_names>
    fn name() -> &'static str;
}

/// Holds the context for outputting a plan.
///
/// This includes:
/// - Options (`&`[`OutputOptions`]) for formatting
/// - Errors (`&`[`ErrorAccumulator`]) for collecting errors
/// - Extensions (`&`[`SimpleExtensions`]) for looking up function and type
///   names
/// - Indent (`&`[`IndentTracker`]) for tracking the current indent level
///
/// The `Scope` trait is used to provide the context for textifying a plan.
pub trait Scope: Sized {
    /// The type of errors that can occur when textifying a plan.
    type Errors: ErrorAccumulator;
    type Indent: IndentTracker;

    /// Get the current indent level.
    fn indent(&self) -> impl fmt::Display;
    /// Push a new indent level.
    fn push_indent(&self) -> Self;

    /// Get the options for formatting the plan.
    fn options(&self) -> &OutputOptions;
    fn extensions(&self) -> &SimpleExtensions;
    fn errors(&self) -> &Self::Errors;

    fn push_error(&self, e: FormatError) {
        self.errors().push(e);
    }

    /// Handle a failure to textify a value. Textify errors are written as
    /// "!{name}" to the output (where "name" is the type name), and
    /// the error is pushed to the error accumulator.
    fn failure<E: Into<FormatError>>(&self, e: E) -> ErrorToken {
        let e = e.into();
        let token = ErrorToken(e.message());
        self.push_error(e);
        token
    }

    fn expect<'a, T: Textify>(&'a self, t: Option<&'a T>) -> MaybeToken<impl fmt::Display> {
        match t {
            Some(t) => MaybeToken(Ok(self.display(t))),
            None => {
                let err = PlanError::invalid(
                    T::name(),
                    // TODO: Make this an optional input
                    NONSPECIFIC,
                    "Required field missing, None found",
                );
                let err_token = self.failure(err);
                MaybeToken(Err(err_token))
            }
        }
    }

    fn expect_ok<'a, T: Textify, E: Into<FormatError>>(
        &'a self,
        result: Result<&'a T, E>,
    ) -> MaybeToken<impl fmt::Display + 'a> {
        MaybeToken(match result {
            Ok(t) => Ok(self.display(t)),
            Err(e) => Err(self.failure(e)),
        })
    }

    fn display<'a, T: Textify>(&'a self, value: &'a T) -> Displayable<'a, Self, T> {
        Displayable { scope: self, value }
    }

    /// Wrap an iterator over textifiable items into a displayable that will
    /// separate them with the given separator.
    ///
    /// # Example
    ///
    /// ```ignore
    /// let items = vec![1, 2, 3];
    /// let separated = self.separated(&items, ", ");
    /// ```
    fn separated<'a, T: Textify, I: IntoIterator<Item = &'a T> + Clone>(
        &'a self,
        items: I,
        separator: &'static str,
    ) -> Separated<'a, Self, T, I> {
        Separated {
            scope: self,
            items,
            separator,
        }
    }

    fn option<'a, T: Textify>(&'a self, value: Option<&'a T>) -> OptionalDisplayable<'a, Self, T> {
        OptionalDisplayable { scope: self, value }
    }

    fn optional<'a, T: Textify>(
        &'a self,
        value: &'a T,
        option: bool,
    ) -> OptionalDisplayable<'a, Self, T> {
        let value = if option { Some(value) } else { None };
        OptionalDisplayable { scope: self, value }
    }
}

#[derive(Clone)]
pub struct Separated<'a, S: Scope, T: Textify + 'a, I: IntoIterator<Item = &'a T> + Clone> {
    scope: &'a S,
    items: I,
    separator: &'static str,
}

impl<'a, S: Scope, T: Textify, I: IntoIterator<Item = &'a T> + Clone> fmt::Display
    for Separated<'a, S, T, I>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, item) in self.items.clone().into_iter().enumerate() {
            if i > 0 {
                f.write_str(self.separator)?;
            }
            item.textify(self.scope, f)?;
        }
        Ok(())
    }
}

impl<'a, S: Scope, T: Textify, I: IntoIterator<Item = &'a T> + Clone + fmt::Debug> fmt::Debug
    for Separated<'a, S, T, I>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Separated{{items: {:?}, separator: {:?}}}",
            self.items, self.separator
        )
    }
}

#[derive(Copy, Clone)]
pub struct Displayable<'a, S: Scope, T: Textify> {
    scope: &'a S,
    value: &'a T,
}

impl<'a, S: Scope, T: Textify + fmt::Debug> fmt::Debug for Displayable<'a, S, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Displayable({:?})", self.value)
    }
}

impl<'a, S: Scope, T: Textify> Displayable<'a, S, T> {
    pub fn new(scope: &'a S, value: &'a T) -> Self {
        Self { scope, value }
    }

    /// Display only if the option is true, otherwise, do not display anything.
    pub fn optional(self, option: bool) -> OptionalDisplayable<'a, S, T> {
        let value = if option { Some(self.value) } else { None };
        OptionalDisplayable {
            scope: self.scope,
            value,
        }
    }
}

impl<'a, S: Scope, T: Textify> fmt::Display for Displayable<'a, S, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.value.textify(self.scope, f)
    }
}

#[derive(Copy, Clone)]
pub struct OptionalDisplayable<'a, S: Scope, T: Textify> {
    scope: &'a S,
    value: Option<&'a T>,
}

impl<'a, S: Scope, T: Textify> fmt::Display for OptionalDisplayable<'a, S, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.value {
            Some(t) => t.textify(self.scope, f),
            None => Ok(()),
        }
    }
}

impl<'a, S: Scope, T: Textify + fmt::Debug> fmt::Debug for OptionalDisplayable<'a, S, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "OptionalDisplayable({:?})", self.value)
    }
}

impl<'a, Err: ErrorAccumulator> Scope for ScopedContext<'a, Err> {
    type Errors = Err;
    type Indent = IndentStack<'a>;

    fn indent(&self) -> impl fmt::Display {
        self.indent
    }

    fn push_indent(&self) -> Self {
        Self {
            indent: self.indent.push(),
            ..*self
        }
    }

    fn options(&self) -> &OutputOptions {
        self.options
    }

    fn errors(&self) -> &Self::Errors {
        self.errors
    }

    fn extensions(&self) -> &SimpleExtensions {
        self.extensions
    }
}
