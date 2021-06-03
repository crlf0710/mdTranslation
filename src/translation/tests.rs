use super::translate;
use pulldown_cmark::html::push_html;
use pulldown_cmark::Parser;

#[test]
fn translation_1() -> () {
    let input = "Hello world!";
    let translation = r"> world

kitty
****
> Hello world!

Nice to meet you!";
    let expected_result = r"<p>Nice to meet you!</p>
";
    let mut result = String::new();
    push_html(
        &mut result,
        translate(
            Parser::new(input),
            Parser::new(translation),
            "en-US",
            Some("en-US"),
        )
        .unwrap(),
    );
    assert_eq!(expected_result, result);
}
