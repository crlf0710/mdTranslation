use crate::inline_group::{InlineGroupEvent, InlineGroupIter};
use crate::roundtrip::write_markdown;
use pulldown_cmark::{Event, Tag};

fn process_event<'event>(
    event: InlineGroupEvent<'event>,
    default_lang: Option<&str>,
) -> Vec<Event<'event>> {
    match event {
        InlineGroupEvent::NonInline(_) => Vec::new(),
        InlineGroupEvent::InlineGroup(group) => {
            let mut result = Vec::new();
            result.push(Event::Start(Tag::BlockQuote));
            result.extend(group.iter().cloned());
            result.push(Event::End(Tag::BlockQuote));
            if let Some(default_lang) = default_lang {
                result.push(Event::Start(Tag::List(None)));
                result.push(Event::Start(Tag::Item));
                result.push(Event::Text(default_lang.to_string().into()));
                result.push(Event::End(Tag::Item));
                result.push(Event::End(Tag::List(None)));
            }
            result.push(Event::Start(Tag::Paragraph));
            result.extend(group);
            result.push(Event::End(Tag::Paragraph));
            result
        }
    }
}

pub fn extract<'event>(
    parser: pulldown_cmark::Parser<'event>,
    output: &mut dyn std::io::Write,
    default_lang: Option<&str>,
) -> std::io::Result<()> {
    let iter = InlineGroupIter::new(parser).flat_map(|e| process_event(e, default_lang));
    write_markdown(output, iter)?;
    Ok(())
}
