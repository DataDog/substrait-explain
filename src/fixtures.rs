//! Test fixtures for working with Substrait plans and substrait_explain

use substrait::proto;

use crate::extensions::simple::ExtensionKind;
use crate::extensions::{ExtensionRegistry, SimpleExtensions};
use crate::parser::common::test_support::ScopedParse;
use crate::textify::foundation::{ErrorAccumulator, ErrorList, ErrorQueue};
use crate::textify::{OutputOptions, Scope, ScopedContext, Textify};

pub struct TestContext {
    pub options: OutputOptions,
    pub extensions: SimpleExtensions,
    pub extension_registry: ExtensionRegistry,
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
            extension_registry: ExtensionRegistry::new(),
        }
    }

    pub fn with_options(mut self, options: OutputOptions) -> Self {
        self.options = options;
        self
    }

    pub fn with_urn(mut self, anchor: u32, urn: &str) -> Self {
        self.extensions
            .add_extension_urn(urn.to_string(), anchor)
            .unwrap();
        self
    }

    pub fn with_function(mut self, urn: u32, anchor: u32, name: impl Into<String>) -> Self {
        assert!(self.extensions.find_urn(urn).is_ok());
        self.extensions
            .add_extension(ExtensionKind::Function, urn, anchor, name.into())
            .unwrap();
        self
    }

    pub fn with_type(mut self, urn: u32, anchor: u32, name: impl Into<String>) -> Self {
        assert!(self.extensions.find_urn(urn).is_ok());
        self.extensions
            .add_extension(ExtensionKind::Type, urn, anchor, name.into())
            .unwrap();
        self
    }

    pub fn with_type_variation(mut self, urn: u32, anchor: u32, name: impl Into<String>) -> Self {
        assert!(self.extensions.find_urn(urn).is_ok());
        self.extensions
            .add_extension(ExtensionKind::TypeVariation, urn, anchor, name.into())
            .unwrap();
        self
    }

    pub fn scope<'e, E: ErrorAccumulator>(&'e self, errors: &'e E) -> impl Scope + 'e {
        ScopedContext::new(
            &self.options,
            errors,
            &self.extensions,
            &self.extension_registry,
        )
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
}

/// Parse a built-in type string (e.g. `"i64"`, `"string?"`) into a
/// `proto::Type`. Panics on invalid type names.
pub fn parse_type(s: &str) -> proto::Type {
    proto::Type::parse(&SimpleExtensions::default(), s)
        .unwrap_or_else(|e| panic!("failed to parse type '{s}': {e}"))
}
