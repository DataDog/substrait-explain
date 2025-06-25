use substrait::proto::expression::{FieldReference, Literal, ScalarFunction};
use substrait::proto::{Expression, Type};
use substrait_explain::extensions::SimpleExtensions;
use substrait_explain::extensions::simple::ExtensionKind;
use substrait_explain::fixtures::TestContext;
use substrait_explain::parser::{Parse, ScopedParse};
use substrait_explain::textify::{ErrorQueue, OutputOptions, Textify};

/// Helper function to parse and check for errors, panicking if either fails
fn must_parse<T, E: std::fmt::Display>(result: Result<T, E>, input: &str) -> T {
    let errors = ErrorQueue::default();
    let t = match result {
        Ok(t) => t,
        Err(e) => {
            println!("Error parsing {}:\n{}", input, e);
            panic!("{}", e);
        }
    };
    errors.errs().expect("Failure while parsing");
    t
}

fn roundtrip_parse<T: Parse + Textify + std::fmt::Debug>(ctx: &TestContext, input: &str) {
    let t = must_parse(T::parse(input), input);

    let actual = ctx.textify_no_errors(&t);
    assert_eq!(actual, input);
}

fn assert_roundtrip<T: ScopedParse + Textify + std::fmt::Debug>(ctx: &TestContext, input: &str) {
    let t = must_parse(T::parse(&ctx.extensions, input), input);

    let actual = ctx.textify_no_errors(&t);
    assert_eq!(actual, input);
}

fn roundtrip_with_simple_output<T: ScopedParse + Textify + std::fmt::Debug>(
    extensions: SimpleExtensions,
    verbose: &str,
    simple: &str,
) {
    let ctx1 = TestContext {
        options: OutputOptions::verbose(),
        extensions,
    };

    let t = must_parse(T::parse(&ctx1.extensions, verbose), verbose);

    let actual = ctx1.textify_no_errors(&t);
    assert_eq!(
        actual, verbose,
        "Expected verbose output: {verbose}, Actual: {actual}"
    );

    let ctx2 = TestContext {
        options: OutputOptions::default(),
        extensions: ctx1.extensions,
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
        .add_extension_uri("some_source".to_string(), 4)
        .unwrap();
    extensions
        .add_extension(ExtensionKind::Type, 4, 12, "MyType".to_string())
        .unwrap();

    let ctx = TestContext {
        options,
        extensions,
    };

    assert_roundtrip::<Type>(&ctx, "i32");
    assert_roundtrip::<Type>(&ctx, "i32?");

    assert_roundtrip::<Type>(&ctx, "MyType#12");
    assert_roundtrip::<Type>(&ctx, "MyType#12?");
    assert_roundtrip::<Type>(&ctx, "MyType#12<i32>");
    assert_roundtrip::<Type>(&ctx, "MyType#12<i32?>");

    assert_roundtrip::<Type>(&ctx, "MyType#12<i32, string?>");
    assert_roundtrip::<Type>(&ctx, "MyType#12?<i32, string?>");
}

#[test]
fn test_expression() {
    let options = OutputOptions::default();
    let mut extensions = SimpleExtensions::default();
    extensions
        .add_extension_uri("some_source".to_string(), 4)
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
    };

    assert_roundtrip::<Literal>(&ctx, "12");
    assert_roundtrip::<Literal>(&ctx, "12:i32");
    roundtrip_parse::<FieldReference>(&ctx, "$1");
    assert_roundtrip::<ScalarFunction>(&ctx, "foo()");
    assert_roundtrip::<Expression>(&ctx, "bar(12)");
    assert_roundtrip::<Expression>(&ctx, "foo(bar(12:i16, 18), -4:i16)");
    assert_roundtrip::<Expression>(&ctx, "foo($2, bar($5, 18), -4:i16)");
}

#[test]
fn test_verbose_and_simple_output() {
    let mut extensions = SimpleExtensions::default();
    extensions
        .add_extension_uri("some_source".to_string(), 4)
        .unwrap();
    extensions
        .add_extension(ExtensionKind::Function, 4, 12, "foo".to_string())
        .unwrap();
    extensions
        .add_extension(ExtensionKind::Function, 4, 14, "bar".to_string())
        .unwrap();

    roundtrip_with_simple_output::<ScalarFunction>(extensions.clone(), "foo#12():i64", "foo()");
    // Check type is included in verbose output
    roundtrip_with_simple_output::<ScalarFunction>(
        extensions.clone(),
        "foo#12(3:i32, 5:i64):i64",
        "foo(3:i32, 5)",
    );
    roundtrip_with_simple_output::<ScalarFunction>(
        extensions.clone(),
        "foo#12(bar#14(5:i64, $2)):i64",
        "foo(bar(5, $2))",
    );
}
