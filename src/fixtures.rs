//! Test fixtures for working with Substrait plans and substrait_explain

use substrait::proto::expression::{FieldReference, Literal, ScalarFunction};
use substrait::proto::{Expression, Type};

use crate::extensions::simple::ExtensionKind;
use crate::extensions::{ExtensionRegistry, SimpleExtensions};
use crate::format;
use crate::parser::{MessageParseError, Parser, expressions, types};
use crate::textify::foundation::{ErrorAccumulator, ErrorList};
use crate::textify::plan::PlanWriter;
use crate::textify::{ErrorQueue, OutputOptions, Scope, ScopedContext, Textify};

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

    pub fn parse_type(&self, input: &str) -> Result<Type, MessageParseError> {
        types::parse_type(&self.extensions, input)
    }

    pub fn parse_literal(&self, input: &str) -> Result<Literal, MessageParseError> {
        expressions::parse_literal(&self.extensions, input)
    }

    pub fn parse_scalar_function(&self, input: &str) -> Result<ScalarFunction, MessageParseError> {
        expressions::parse_scalar_function(&self.extensions, input)
    }

    pub fn parse_expression(&self, input: &str) -> Result<Expression, MessageParseError> {
        expressions::parse_expression(&self.extensions, input)
    }

    pub fn parse_field_reference(&self, input: &str) -> Result<FieldReference, MessageParseError> {
        expressions::parse_field_reference(input)
    }
}

/// Roundtrip a plan and verify that the output is the same as the input, after
/// being parsed to a Substrait plan and then back to text.
pub fn roundtrip_plan(input: &str) {
    // Parse the plan using the simplified interface
    let plan = Parser::parse(input).unwrap_or_else(|e| {
        println!("Error parsing plan:\n{e}");
        panic!("{e}");
    });

    // Format the plan back to text using the simplified interface
    let (actual, errors) = format(&plan);

    // Check for formatting errors
    if !errors.is_empty() {
        println!("Formatting errors:");
        for error in errors {
            println!("  {error}");
        }
        panic!("Formatting errors occurred");
    }

    // Compare the output with the input, printing the difference.
    assert_eq!(
        actual.trim(),
        input.trim(),
        "Expected:\n---\n{}\n---\nActual:\n---\n{}\n---",
        input.trim(),
        actual.trim()
    );
}

/// Roundtrip a plan and verify that the output is the same as the input, after
/// being parsed to a Substrait plan and then back to text.
pub fn roundtrip_plan_with_verbose(input: &str, verbose_input: &str) {
    let default_registry = ExtensionRegistry::new();

    // Parse the simple plan
    let simple_plan = Parser::parse(input).unwrap_or_else(|e| {
        println!("Error parsing simple plan:\n{e}");
        panic!("{e}");
    });

    // Parse the verbose plan
    let verbose_plan = Parser::parse(verbose_input).unwrap_or_else(|e| {
        println!("Error parsing verbose plan:\n{e}");
        panic!("{e}");
    });

    // Test verbose output from both plans
    let verbose_options = OutputOptions::verbose();
    let (simple_verbose_writer, simple_verbose_errors) =
        PlanWriter::<ErrorQueue>::new(&verbose_options, &simple_plan, &default_registry);
    let simple_verbose_actual = format!("{simple_verbose_writer}");
    simple_verbose_errors
        .errs()
        .expect("Errors during simple -> verbose conversion");

    let (verbose_verbose_writer, verbose_verbose_errors) =
        PlanWriter::<ErrorQueue>::new(&verbose_options, &verbose_plan, &default_registry);
    let verbose_verbose_actual = format!("{verbose_verbose_writer}");
    verbose_verbose_errors
        .errs()
        .expect("Errors during verbose -> verbose conversion");

    assert_eq!(
        simple_verbose_actual.trim(),
        verbose_verbose_actual.trim(),
        "Expected verbose outputs to match:\n---\n{}\n---\n---\n{}\n---",
        simple_verbose_actual.trim(),
        verbose_verbose_actual.trim()
    );

    // Test simple output from both plans
    let simple_options = OutputOptions::default();
    let (simple_simple_writer, simple_simple_errors) =
        PlanWriter::<ErrorQueue>::new(&simple_options, &simple_plan, &default_registry);
    let simple_simple_actual = format!("{simple_simple_writer}");
    simple_simple_errors
        .errs()
        .expect("Errors during simple -> simple conversion");

    let (verbose_simple_writer, verbose_simple_errors) =
        PlanWriter::<ErrorQueue>::new(&simple_options, &verbose_plan, &default_registry);
    let verbose_simple_actual = format!("{verbose_simple_writer}");
    verbose_simple_errors
        .errs()
        .expect("Errors during verbose -> simple conversion");

    assert_eq!(
        simple_simple_actual.trim(),
        verbose_simple_actual.trim(),
        "Expected simple outputs to match:\n---\n{}\n---\n---\n{}\n---",
        simple_simple_actual.trim(),
        verbose_simple_actual.trim()
    );
}
