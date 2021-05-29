use super::write_markdown;
use crate::inline_group::{InlineGroupIteratorExt};
use pulldown_cmark::{CodeBlockKind, Event, Options, Parser, Tag};

const COMMONMARK_SPEC_TEXT: &'static str = include_str!("./third_party/CommonMark/spec.txt");

const COMMONMARK_SPEC_EXAMPLE_COUNT: usize = 649;

fn is_example_fence(tag: &Tag<'_>) -> bool {
    if let Tag::CodeBlock(CodeBlockKind::Fenced(fence_value)) = tag {
        &**fence_value == "example"
    } else {
        false
    }
}

fn collect_test_case<'a>(events: &mut impl Iterator<Item = Event<'a>>) -> Option<(String, String)> {
    let begin_tag = events.next().and_then(|e| {
        if let Event::Start(tag) = e {
            Some(tag)
        } else {
            None
        }
    })?;
    let text = events.next().and_then(|e| {
        if let Event::Text(text) = e {
            Some(text)
        } else {
            None
        }
    })?;
    let end_tag = events.next().and_then(|e| {
        if let Event::End(tag) = e {
            Some(tag)
        } else {
            None
        }
    })?;
    if !(is_example_fence(&begin_tag) && is_example_fence(&end_tag)) {
        return None;
    }
    let splitted_text = text.split("\n.\n").collect::<Vec<_>>();
    if splitted_text.len() != 2 {
        return None;
    }
    let input = splitted_text[0];
    let output = splitted_text[1].trim_end_matches('\n');
    Some((input.to_string(), output.to_string()))
}

fn test_roundtrip(original: &str, expected: &str) -> bool {
    let opts = Options::empty();
    let event_list = Parser::new_ext(original, opts).collect::<Vec<_>>();
    let mut regen_str = Vec::new();
    write_markdown(&mut regen_str, event_list.iter().cloned()).expect("Regeneration failure");
    let regen_str = core::str::from_utf8(&regen_str).expect("Should be utf-8");
    let event_list_2 = Parser::new_ext(&regen_str, opts).collect::<Vec<_>>();
    if event_list
        .iter()
        .cloned()
        .into_inline_groups_nonconservative()
        .eq(event_list_2.iter().cloned().into_inline_groups_nonconservative())
    {
        return true;
    }
    let event_count = event_list.iter().cloned().into_inline_groups_nonconservative().count();
    let event_count_2 = event_list_2.iter().cloned().into_inline_groups_nonconservative().count();
    let same_event_count = event_list
        .iter()
        .cloned()
        .into_inline_groups_nonconservative()
        .zip(event_list_2.iter().cloned().into_inline_groups_nonconservative())
        .take_while(|(e1, e2)| e1 == e2)
        .count();
    if event_count == same_event_count && event_count_2 == same_event_count {
        return true;
    }
    eprintln!(
        "Test fail: event [{}/{}] is {:?} vs {:?}",
        same_event_count,
        event_count,
        event_list
            .iter()
            .cloned()
            .into_inline_groups_nonconservative()
            .nth(same_event_count),
        event_list_2
            .iter()
            .cloned()
            .into_inline_groups_nonconservative()
            .nth(same_event_count),
    );
    eprintln!("Original input: \n{}", original);
    eprintln!("Regenerated markdown: \n{}", regen_str);
    eprintln!("Expected token list: \n{:?}", event_list);
    eprintln!("Actual token list: \n{:?}", event_list_2);
    eprintln!("Expected full output:\n{}", expected);
    return false;
}

#[test]
fn commonmark_spec_roundtrip() {
    let opts = Options::empty();
    let p = Parser::new_ext(COMMONMARK_SPEC_TEXT, opts);

    let mut testsuite = vec![];
    let mut p = p.peekable();
    while let Some(peeked_event) = p.peek() {
        if let Event::Start(tag) = peeked_event {
            if is_example_fence(tag) {
                // a new example, insert it into the testsuite.
                let new_test_case =
                    collect_test_case(&mut p).expect("Error parsing example text from spec.");
                testsuite.push(new_test_case);
                continue;
            }
        }
        let _ = p.next();
    }
    let mut success_count = 0usize;
    let testsuite_len = testsuite.len();
    for (original, expected) in testsuite {
        if test_roundtrip(&original, &expected) {
            success_count += 1;
        }
    }
    assert_eq!(COMMONMARK_SPEC_EXAMPLE_COUNT, testsuite_len);
    assert_eq!(COMMONMARK_SPEC_EXAMPLE_COUNT, success_count);
}
