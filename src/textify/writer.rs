//! Generic writer for any `Textify` value.
//!
//! This writer is fragment-oriented and does not reconstruct plan headers or
//! extension declaration blocks. Use [`crate::textify::plan::PlanWriter`] for
//! full-plan output.

use crate::extensions::{ExtensionRegistry, SimpleExtensions};
use crate::textify::foundation::{ErrorQueue, FormatError, OutputOptions, ScopedContext, Textify};

/// Generic writer for any value implementing [`Textify`].
#[derive(Debug, Clone)]
pub struct Writer<'a> {
    options: OutputOptions,
    extensions: &'a SimpleExtensions,
    extension_registry: ExtensionRegistry,
}

impl<'a> Writer<'a> {
    /// Create a writer with default output options and an empty extension registry.
    pub fn new(extensions: &'a SimpleExtensions) -> Self {
        Self {
            options: OutputOptions::default(),
            extensions,
            extension_registry: ExtensionRegistry::default(),
        }
    }

    /// Override output options used while writing values.
    pub fn with_options(mut self, options: &OutputOptions) -> Self {
        self.options = options.clone();
        self
    }

    /// Provide an extension registry used by extension-related textification.
    pub fn with_extension_registry(mut self, extension_registry: &ExtensionRegistry) -> Self {
        self.extension_registry = extension_registry.clone();
        self
    }

    /// Render a value and return accumulated formatting warnings.
    pub fn write<T: Textify>(&self, value: &T) -> (String, Vec<FormatError>) {
        let errors = ErrorQueue::default();
        let scope = ScopedContext::new(
            &self.options,
            &errors,
            self.extensions,
            &self.extension_registry,
        );

        let mut output = String::new();
        value
            .textify(&scope, &mut output)
            .expect("writing to String should not fail");

        let errors = errors.into_iter().collect();
        (output, errors)
    }
}
