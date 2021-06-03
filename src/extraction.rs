use crate::roundtrip::write_markdown;
use crate::utils::VecSet;
use crate::utils::{InlineGroupEvent, InlineGroupIteratorExt};
use pulldown_cmark::{Event, Tag};

fn process_event<'event>(
    event_group: Vec<Event<'event>>,
    default_lang: Option<&str>,
) -> Vec<Event<'event>> {
    let mut result = Vec::new();
    result.push(Event::Start(Tag::BlockQuote));
    result.extend(event_group.iter().cloned());
    result.push(Event::End(Tag::BlockQuote));
    if let Some(default_lang) = default_lang {
        result.push(Event::Start(Tag::List(None)));
        result.push(Event::Start(Tag::Item));
        result.push(Event::Text(default_lang.to_string().into()));
        result.push(Event::End(Tag::Item));
        result.push(Event::End(Tag::List(None)));
    }
    result.push(Event::Start(Tag::Paragraph));
    result.extend(event_group);
    result.push(Event::End(Tag::Paragraph));
    result.push(Event::Rule);
    result
}

pub fn extract<'event>(
    input: impl Iterator<Item = Event<'event>>,
    output: &mut dyn std::io::Write,
    default_lang: Option<&str>,
) -> std::io::Result<()> {
    let iter = input
        .into_inline_groups()
        .flat_map(|e| match e {
            InlineGroupEvent::InlineGroup(e) => Some(e),
            InlineGroupEvent::NonInline(_) => None,
        })
        .collect::<VecSet<_>>()
        .into_iter()
        .flat_map(|e| process_event(e, default_lang));
    write_markdown(output, iter)?;
    Ok(())
}
