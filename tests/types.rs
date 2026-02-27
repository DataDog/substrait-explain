use std::fmt::Debug;

use substrait::proto::expression::{FieldReference, Literal, ScalarFunction};
use substrait::proto::{Expression, Type};
use substrait_explain::extensions::SimpleExtensions;
use substrait_explain::extensions::simple::ExtensionKind;
use substrait_explain::parser::{ParseFragment, Parser};
use substrait_explain::textify::{OutputOptions, Textify, Writer};

fn must_parse<T, E: std::fmt::Display>(result: Result<T, E>, input: &str) -> T {
    match result {
        Ok(t) => t,
        Err(e) => {
            println!("Error parsing {input}:\n{e}");
            panic!("{e}");
        }
    }
}

fn assert_roundtrip<T>(extensions: &SimpleExtensions, input: &str)
where
    T: ParseFragment + Textify + Debug + PartialEq,
{
    let parser = Parser::new().with_simple_extensions(extensions.clone());
    let writer = Writer::new(extensions);

    let first = must_parse(parser.parse_fragment::<T>(input), input);
    let (text, errors) = writer.write(&first);
    assert!(errors.is_empty(), "Formatting errors: {errors:?}");
    let second = must_parse(parser.parse_fragment::<T>(&text), &text);
    assert_eq!(first, second);
}

#[test]
fn test_types() {
    let mut extensions = SimpleExtensions::default();
    extensions
        .add_extension_urn("some_source".to_string(), 4)
        .unwrap();
    extensions
        .add_extension(ExtensionKind::Type, 4, 12, "MyType".to_string())
        .unwrap();

    for input in [
        "i32",
        "i32?",
        "binary",
        "binary?",
        "timestamp",
        "timestamp?",
        "timestamp_tz",
        "timestamp_tz?",
        "date",
        "date?",
        "time",
        "time?",
        "interval_year",
        "interval_year?",
        "uuid",
        "uuid?",
        "MyType#12",
        "MyType#12?",
        "MyType#12<i32>",
        "MyType#12<i32?>",
        "MyType#12<i32, string?>",
        "MyType#12?<i32, string?>",
    ] {
        assert_roundtrip::<Type>(&extensions, input);
    }
}

#[test]
fn test_expression() {
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

    assert_roundtrip::<Literal>(&extensions, "12");
    assert_roundtrip::<Literal>(&extensions, "12:i32");
    assert_roundtrip::<FieldReference>(&extensions, "$1");
    assert_roundtrip::<ScalarFunction>(&extensions, "foo()");
    assert_roundtrip::<Expression>(&extensions, "bar(12)");
    assert_roundtrip::<Expression>(&extensions, "foo(bar(12:i16, 18), -4:i16)");
    assert_roundtrip::<Expression>(&extensions, "foo($2, bar($5, 18), -4:i16)");
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

    let parser = Parser::new().with_simple_extensions(extensions.clone());

    for (verbose_input, simple_output) in [
        ("foo#12():i64", "foo()"),
        ("foo#12(3:i32, 5:i64):i64", "foo(3:i32, 5)"),
        ("foo#12(bar#14(5:i64, $2)):i64", "foo(bar(5, $2))"),
    ] {
        let parsed: ScalarFunction =
            must_parse(parser.parse_fragment(verbose_input), verbose_input);

        let (verbose, verbose_errors) = Writer::new(&extensions)
            .with_options(&OutputOptions::verbose())
            .write(&parsed);
        assert!(
            verbose_errors.is_empty(),
            "Formatting errors: {verbose_errors:?}"
        );
        assert_eq!(verbose, verbose_input);

        let (simple, simple_errors) = Writer::new(&extensions).write(&parsed);
        assert!(
            simple_errors.is_empty(),
            "Formatting errors: {simple_errors:?}"
        );
        assert_eq!(simple, simple_output);
    }
}
