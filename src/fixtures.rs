use crate::extensions::SimpleExtensions;
use crate::extensions::simple::ExtensionKind;
use crate::parser::{MessageParseError, ScopedParse};
use crate::textify::foundation::{ErrorAccumulator, ErrorList};
use crate::textify::{ErrorQueue, OutputOptions, Scope, ScopedContext, Textify};

pub struct TestContext {
    pub options: OutputOptions,
    pub extensions: SimpleExtensions,
}

impl Default for TestContext {
    fn default() -> Self {
        Self::new()
    }
}

impl TestContext {
    pub fn new() -> Self {
        Self {
            options: OutputOptions::default(),
            extensions: SimpleExtensions::new(),
        }
    }

    pub fn with_options(mut self, options: OutputOptions) -> Self {
        self.options = options;
        self
    }

    pub fn with_uri(mut self, anchor: u32, uri: &str) -> Self {
        self.extensions
            .add_extension_uri(uri.to_string(), anchor)
            .unwrap();
        self
    }

    pub fn with_function(mut self, uri: u32, anchor: u32, name: impl Into<String>) -> Self {
        assert!(self.extensions.find_uri(uri).is_ok());
        self.extensions
            .add_extension(ExtensionKind::Function, uri, anchor, name.into())
            .unwrap();
        self
    }

    pub fn with_type(mut self, uri: u32, anchor: u32, name: impl Into<String>) -> Self {
        assert!(self.extensions.find_uri(uri).is_ok());
        self.extensions
            .add_extension(ExtensionKind::Type, uri, anchor, name.into())
            .unwrap();
        self
    }

    pub fn with_type_variation(mut self, uri: u32, anchor: u32, name: impl Into<String>) -> Self {
        assert!(self.extensions.find_uri(uri).is_ok());
        self.extensions
            .add_extension(ExtensionKind::TypeVariation, uri, anchor, name.into())
            .unwrap();
        self
    }

    pub fn scope<'e, E: ErrorAccumulator>(&'e self, errors: &'e E) -> impl Scope + 'e {
        ScopedContext::new(&self.options, errors, &self.extensions)
    }

    pub fn textify<T: Textify>(&self, t: &T) -> (String, ErrorList) {
        let errors = ErrorQueue::default();
        let mut output = String::new();

        let scope = self.scope(&errors);
        t.textify(&scope, &mut output).unwrap();

        let evec = errors.into_iter().collect();
        (output, ErrorList(evec))
    }

    pub fn textify_no_errors<T: Textify>(&self, t: &T) -> String {
        let (s, errs) = self.textify(t);
        assert!(errs.is_empty(), "{} Errors: {}", errs.0.len(), errs.0[0]);
        s
    }

    pub fn parse<T: ScopedParse>(&self, input: &str) -> Result<T, MessageParseError> {
        T::parse(&self.extensions, input)
    }
}
