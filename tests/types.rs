use substrait::proto::Type;
use substrait_explain::extensions::SimpleExtensions;
use substrait_explain::fixtures::TestContext;
use substrait_explain::parser::{Parse, ScopedParse};
use substrait_explain::textify::{ErrorVec, OutputOptions, Textify};

fn assert_roundtrip<T: ScopedParse + Textify + std::fmt::Debug>(ctx: &TestContext, input: &str) {
    let mut errors = ErrorVec::default();

    let t = {
        let mut scope = ctx.scope(&mut errors);
        T::parse(&mut scope, input).unwrap()
    };
    assert!(errors.0.is_empty(), "{}", errors);

    let actual = ctx.textify_no_errors(t);
    assert_eq!(actual, input);
}

#[test]
fn test_type_explain() {
    let options = OutputOptions::default();
    let mut extensions = SimpleExtensions::default();
    extensions.add_extension_uri("some_source".to_string(), 4);
    extensions.add_extension_type(4, 12, "MyType".to_string());

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
