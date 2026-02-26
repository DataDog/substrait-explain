use substrait::proto::expression::{FieldReference, Literal, ScalarFunction};
use substrait::proto::{Expression, Type};
use substrait_explain::extensions::SimpleExtensions;
use substrait_explain::extensions::simple::ExtensionKind;
use substrait_explain::fixtures::TestContext;
use substrait_explain::parser::MessageParseError;
use substrait_explain::textify::{ErrorQueue, OutputOptions, Textify};

/// Helper function to parse and check for errors, panicking if either fails
fn must_parse<T, E: std::fmt::Display>(result: Result<T, E>, input: &str) -> T {
    let errors = ErrorQueue::default();
    let t = match result {
        Ok(t) => t,
        Err(e) => {
            println!("Error parsing {input}:\n{e}");
            panic!("{}", e);
        }
    };
    errors.errs().expect("Failure while parsing");
    t
}

fn roundtrip_parse<T, F>(ctx: &TestContext, input: &str, parse: F)
where
    T: Textify + std::fmt::Debug,
    F: FnOnce(&TestContext, &str) -> Result<T, MessageParseError>,
{
    let t = must_parse(parse(ctx, input), input);

    let actual = ctx.textify_no_errors(&t);
    assert_eq!(actual, input);
}

fn assert_roundtrip<T, F>(ctx: &TestContext, input: &str, parse: F)
where
    T: Textify + std::fmt::Debug,
    F: FnOnce(&TestContext, &str) -> Result<T, MessageParseError>,
{
    let t = must_parse(parse(ctx, input), input);

    let actual = ctx.textify_no_errors(&t);
    assert_eq!(actual, input);
}

fn roundtrip_with_simple_output<T, F>(
    extensions: SimpleExtensions,
    verbose: &str,
    simple: &str,
    parse: F,
) where
    T: Textify + std::fmt::Debug,
    F: Fn(&TestContext, &str) -> Result<T, MessageParseError>,
{
    let ctx1 = TestContext {
        options: OutputOptions::verbose(),
        extensions,
        extension_registry: Default::default(),
    };

    let t = must_parse(parse(&ctx1, verbose), verbose);

    let actual = ctx1.textify_no_errors(&t);
    assert_eq!(
        actual, verbose,
        "Expected verbose output: {verbose}, Actual: {actual}"
    );

    let ctx2 = TestContext {
        options: OutputOptions::default(),
        extensions: ctx1.extensions,
        extension_registry: Default::default(),
    };

    let actual = ctx2.textify_no_errors(&t);
    assert_eq!(
        actual, simple,
        "Expected simple output: {simple}, Actual: {actual}"
    );
}

#[test]
fn test_types() {
    let options = OutputOptions::verbose();
    let mut extensions = SimpleExtensions::default();
    extensions
        .add_extension_urn("some_source".to_string(), 4)
        .unwrap();
    extensions
        .add_extension(ExtensionKind::Type, 4, 12, "MyType".to_string())
        .unwrap();

    let ctx = TestContext {
        options,
        extensions,
        extension_registry: Default::default(),
    };

    assert_roundtrip::<Type, _>(&ctx, "i32", TestContext::parse_type);
    assert_roundtrip::<Type, _>(&ctx, "i32?", TestContext::parse_type);

    // Test new simple types added in issue #20
    assert_roundtrip::<Type, _>(&ctx, "binary", TestContext::parse_type);
    assert_roundtrip::<Type, _>(&ctx, "binary?", TestContext::parse_type);
    assert_roundtrip::<Type, _>(&ctx, "timestamp", TestContext::parse_type);
    assert_roundtrip::<Type, _>(&ctx, "timestamp?", TestContext::parse_type);
    assert_roundtrip::<Type, _>(&ctx, "timestamp_tz", TestContext::parse_type);
    assert_roundtrip::<Type, _>(&ctx, "timestamp_tz?", TestContext::parse_type);
    assert_roundtrip::<Type, _>(&ctx, "date", TestContext::parse_type);
    assert_roundtrip::<Type, _>(&ctx, "date?", TestContext::parse_type);
    assert_roundtrip::<Type, _>(&ctx, "time", TestContext::parse_type);
    assert_roundtrip::<Type, _>(&ctx, "time?", TestContext::parse_type);
    assert_roundtrip::<Type, _>(&ctx, "interval_year", TestContext::parse_type);
    assert_roundtrip::<Type, _>(&ctx, "interval_year?", TestContext::parse_type);
    assert_roundtrip::<Type, _>(&ctx, "uuid", TestContext::parse_type);
    assert_roundtrip::<Type, _>(&ctx, "uuid?", TestContext::parse_type);

    assert_roundtrip::<Type, _>(&ctx, "MyType#12", TestContext::parse_type);
    assert_roundtrip::<Type, _>(&ctx, "MyType#12?", TestContext::parse_type);
    assert_roundtrip::<Type, _>(&ctx, "MyType#12<i32>", TestContext::parse_type);
    assert_roundtrip::<Type, _>(&ctx, "MyType#12<i32?>", TestContext::parse_type);

    assert_roundtrip::<Type, _>(&ctx, "MyType#12<i32, string?>", TestContext::parse_type);
    assert_roundtrip::<Type, _>(&ctx, "MyType#12?<i32, string?>", TestContext::parse_type);
}

#[test]
fn test_expression() {
    let options = OutputOptions::default();
    let mut extensions = SimpleExtensions::default();
    extensions
        .add_extension_urn("some_source".to_string(), 4)
        .unwrap();
    extensions
        .add_extension(ExtensionKind::Function, 4, 12, "foo".to_string())
        .unwrap();
    extensions
        .add_extension(ExtensionKind::Function, 4, 14, "bar".to_string())
        .unwrap();

    let ctx = TestContext {
        options,
        extensions,
        extension_registry: Default::default(),
    };

    assert_roundtrip::<Literal, _>(&ctx, "12", TestContext::parse_literal);
    assert_roundtrip::<Literal, _>(&ctx, "12:i32", TestContext::parse_literal);
    roundtrip_parse::<FieldReference, _>(&ctx, "$1", TestContext::parse_field_reference);
    assert_roundtrip::<ScalarFunction, _>(&ctx, "foo()", TestContext::parse_scalar_function);
    assert_roundtrip::<Expression, _>(&ctx, "bar(12)", TestContext::parse_expression);
    assert_roundtrip::<Expression, _>(
        &ctx,
        "foo(bar(12:i16, 18), -4:i16)",
        TestContext::parse_expression,
    );
    assert_roundtrip::<Expression, _>(
        &ctx,
        "foo($2, bar($5, 18), -4:i16)",
        TestContext::parse_expression,
    );
}

#[test]
fn test_verbose_and_simple_output() {
    let mut extensions = SimpleExtensions::default();
    extensions
        .add_extension_urn("some_source".to_string(), 4)
        .unwrap();
    extensions
        .add_extension(ExtensionKind::Function, 4, 12, "foo".to_string())
        .unwrap();
    extensions
        .add_extension(ExtensionKind::Function, 4, 14, "bar".to_string())
        .unwrap();

    roundtrip_with_simple_output::<ScalarFunction, _>(
        extensions.clone(),
        "foo#12():i64",
        "foo()",
        TestContext::parse_scalar_function,
    );
    // Check type is included in verbose output
    roundtrip_with_simple_output::<ScalarFunction, _>(
        extensions.clone(),
        "foo#12(3:i32, 5:i64):i64",
        "foo(3:i32, 5)",
        TestContext::parse_scalar_function,
    );
    roundtrip_with_simple_output::<ScalarFunction, _>(
        extensions.clone(),
        "foo#12(bar#14(5:i64, $2)):i64",
        "foo(bar(5, $2))",
        TestContext::parse_scalar_function,
    );
}
