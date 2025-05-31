#![cfg(test)]

use super::{ErrorVec, OutputOptions, ScopedContext, Textify};
use crate::extensions::{ExtensionLookup, SimpleExtensions};

pub struct TestContext {
    pub options: OutputOptions,
    pub extensions: ExtensionLookup,
}

impl TestContext {
    pub fn new() -> Self {
        Self {
            options: OutputOptions::default(),
            extensions: ExtensionLookup::new(),
        }
    }

    pub fn with_options(mut self, options: OutputOptions) -> Self {
        self.options = options;
        self
    }

    pub fn with_uri(mut self, anchor: u32, uri: &str) -> Self {
        self.extensions.add_extension_uri(uri.to_string(), anchor);
        self
    }

    pub fn with_function(mut self, uri: u32, anchor: u32, name: impl Into<String>) -> Self {
        assert!(self.extensions.find_uri(uri).is_some());
        self.extensions
            .add_extension_function(uri, anchor, name.into());
        self
    }

    pub fn with_type(mut self, uri: u32, anchor: u32, name: impl Into<String>) -> Self {
        assert!(self.extensions.find_uri(uri).is_some());
        self.extensions.add_extension_type(uri, anchor, name.into());
        self
    }

    pub fn with_type_variation(mut self, uri: u32, anchor: u32, name: impl Into<String>) -> Self {
        assert!(self.extensions.find_uri(uri).is_some());
        self.extensions
            .add_extension_type_variation(uri, anchor, name.into());
        self
    }

    pub fn textify<T: Textify>(&self, t: T) -> (String, ErrorVec) {
        let mut errors = ErrorVec::default();
        let mut scope = ScopedContext::new(&self.options, &mut errors, &self.extensions);
        let mut output = String::new();
        t.textify(&mut scope, &mut output).unwrap();
        (output, errors)
    }

    pub fn textify_no_errors<T: Textify>(&self, t: T) -> String {
        let (s, errs) = self.textify(t);
        assert!(errs.0.is_empty(), "{}", errs);
        s
    }
}
