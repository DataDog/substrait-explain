use substrait::proto::expression::{FieldReference, Literal, ScalarFunction};
use substrait::proto::{Expression, Type};
use substrait_explain::extensions::SimpleExtensions;
use substrait_explain::fixtures::TestContext;
use substrait_explain::parser::{Parse, ScopedParse};
use substrait_explain::textify::{ErrorQueue, OutputOptions, Textify};

fn roundtrip_parse<T: Parse + Textify + std::fmt::Debug>(ctx: &TestContext, input: &str) {
    let errors = ErrorQueue::default();

    let t = match T::parse(input) {
        Ok(t) => t,
        Err(e) => {
            println!("Error parsing {}:\n{}", input, e);
            panic!("{}", e);
        }
    };
    errors.errs().expect("Failure while parsing");

    let actual = ctx.textify_no_errors(t);
    assert_eq!(actual, input);
}

fn assert_roundtrip<T: ScopedParse + Textify + std::fmt::Debug>(ctx: &TestContext, input: &str) {
    let errors = ErrorQueue::default();

    let t = {
        let mut scope = ctx.scope(&errors);
        match T::parse(&mut scope, input) {
            Ok(t) => t,
            Err(e) => {
                println!("Error parsing {}:\n{}", input, e);
                panic!("{}", e);
            }
        }
    };
    errors.errs().expect("Failure while parsing");

    let actual = ctx.textify_no_errors(t);
    assert_eq!(actual, input);
}

#[test]
fn test_types() {
    let options = OutputOptions::default();
    let mut extensions = SimpleExtensions::default();
    extensions
        .add_extension_uri("some_source".to_string(), 4)
        .unwrap();
    extensions
        .add_extension_type(4, 12, "MyType".to_string())
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
        .add_extension_function(4, 12, "foo".to_string())
        .unwrap();
    extensions
        .add_extension_function(4, 14, "bar".to_string())
        .unwrap();

    let ctx = TestContext {
        options,
        extensions,
    };

    assert_roundtrip::<Literal>(&ctx, "12");
    assert_roundtrip::<Literal>(&ctx, "12:i32");
    roundtrip_parse::<FieldReference>(&ctx, "$1");
    assert_roundtrip::<ScalarFunction>(&ctx, "foo#12()");
    assert_roundtrip::<ScalarFunction>(&ctx, "foo#12():i64");
    assert_roundtrip::<Expression>(&ctx, "bar#14(12)");
    assert_roundtrip::<Expression>(&ctx, "foo#12(bar#14(12:i16, 18), -4:i16)");
    assert_roundtrip::<Expression>(&ctx, "foo#12($2, bar#14($5, 18), -4:i16)");
}
