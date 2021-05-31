use std::iter::Peekable;

use pulldown_cmark::{CodeBlockKind, CowStr, Event, Tag};

use crate::utils::{InlineGroupEvent, InlineGroupIteratorExt};
use crate::utils::VecMap;

fn erase_cowstr_lifetime(c: &CowStr<'_>) -> CowStr<'static> {
    match c {
        CowStr::Boxed(b) => CowStr::Boxed(b.clone()),
        CowStr::Borrowed(s) => CowStr::Boxed(s.to_string().into()),
        CowStr::Inlined(i) => CowStr::Inlined(i.clone()),
    }
}

fn erase_codeblock_kind_lifetime(kind: &CodeBlockKind<'_>) -> CodeBlockKind<'static> {
    match kind {
        CodeBlockKind::Indented => CodeBlockKind::Indented,
        CodeBlockKind::Fenced(c) => CodeBlockKind::Fenced(erase_cowstr_lifetime(c)),
    }
}

fn erase_tag_lifetime(tag: &Tag<'_>) -> Tag<'static> {
    match tag {
        Tag::Paragraph => Tag::Paragraph,
        Tag::Heading(u) => Tag::Heading(*u),
        Tag::BlockQuote => Tag::BlockQuote,
        Tag::CodeBlock(k) => Tag::CodeBlock(erase_codeblock_kind_lifetime(k)),
        Tag::List(s) => Tag::List(*s),
        Tag::Item => Tag::Item,
        Tag::FootnoteDefinition(c) => Tag::FootnoteDefinition(erase_cowstr_lifetime(c)),
        Tag::Table(a) => Tag::Table(a.clone()),
        Tag::TableHead => Tag::TableHead,
        Tag::TableRow => Tag::TableRow,
        Tag::TableCell => Tag::TableCell,
        Tag::Emphasis => Tag::Emphasis,
        Tag::Strong => Tag::Strong,
        Tag::Strikethrough => Tag::Strikethrough,
        Tag::Link(t, c1, c2) => Tag::Link(*t, erase_cowstr_lifetime(c1), erase_cowstr_lifetime(c2)),
        Tag::Image(t, c1, c2) => {
            Tag::Image(*t, erase_cowstr_lifetime(c1), erase_cowstr_lifetime(c2))
        }
    }
}

fn erase_event_lifetime(event: &Event<'_>) -> Event<'static> {
    match event {
        Event::Start(t) => Event::Start(erase_tag_lifetime(t)),
        Event::End(t) => Event::End(erase_tag_lifetime(t)),
        Event::Text(c) => Event::Text(erase_cowstr_lifetime(c)),
        Event::Code(c) => Event::Code(erase_cowstr_lifetime(c)),
        Event::Html(c) => Event::Html(erase_cowstr_lifetime(c)),
        Event::FootnoteReference(c) => Event::FootnoteReference(erase_cowstr_lifetime(c)),
        Event::SoftBreak => Event::SoftBreak,
        Event::HardBreak => Event::HardBreak,
        Event::Rule => Event::Rule,
        Event::TaskListMarker(b) => Event::TaskListMarker(*b),
    }
}

fn erase_events_lifetime(events: &Vec<Event<'_>>) -> Vec<Event<'static>> {
    events.iter().map(erase_event_lifetime).collect()
}

#[derive(Default)]
struct TranslationData {
    data_map: VecMap<Vec<Event<'static>>, Vec<Event<'static>>>,
}

fn recognize_one_item<'event>(
    translation_data: &mut TranslationData,
    events: &mut Peekable<impl Iterator<Item = InlineGroupEvent<'event>>>,
    selected_lang: &str,
    use_default_lang: bool,
) -> std::io::Result<Option<()>> {
    if !matches!(
        events.peek(),
        Some(InlineGroupEvent::NonInline(Event::Start(Tag::BlockQuote)))
    ) {
        if matches!(events.peek(), None) {
            return Ok(None);
        } else {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                format!("{:?}", events.peek()),
            ));
        }
    }
    let mut item_contents = vec![];
    loop {
        item_contents.extend(events.next());
        if !matches!(
            events.peek(),
            Some(InlineGroupEvent::NonInline(Event::Start(Tag::BlockQuote)))
        ) {
            if matches!(events.peek(), None) {
                break;
            } else {
                continue;
            }
        } else {
            break;
        }
    }
    let mut remaining_contents = &item_contents[..];
    let original_text;
    if let [InlineGroupEvent::NonInline(Event::Start(Tag::BlockQuote)), InlineGroupEvent::InlineGroup(original_text_group), InlineGroupEvent::NonInline(Event::End(Tag::BlockQuote)), ..] =
        remaining_contents
    {
        original_text = erase_events_lifetime(original_text_group);
        remaining_contents = &remaining_contents[3..];
    } else if let [InlineGroupEvent::NonInline(Event::Start(Tag::BlockQuote)), InlineGroupEvent::NonInline(Event::Start(Tag::Paragraph)), InlineGroupEvent::InlineGroup(original_text_group), InlineGroupEvent::NonInline(Event::End(Tag::Paragraph)), InlineGroupEvent::NonInline(Event::End(Tag::BlockQuote)), ..] =
        remaining_contents
    {
        original_text = erase_events_lifetime(original_text_group);
        remaining_contents = &remaining_contents[5..];
    } else {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!("{:?}", remaining_contents),
        ));
    }
    while !remaining_contents.is_empty() {
        let (lang, translated_text);
        if let [InlineGroupEvent::NonInline(Event::Start(Tag::List(None))), InlineGroupEvent::NonInline(Event::Start(Tag::Item)), InlineGroupEvent::InlineGroup(lang_group), InlineGroupEvent::NonInline(Event::End(Tag::Item)), InlineGroupEvent::NonInline(Event::End(Tag::List(None))), InlineGroupEvent::NonInline(Event::Start(Tag::Paragraph)), InlineGroupEvent::InlineGroup(translated_group), InlineGroupEvent::NonInline(Event::End(Tag::Paragraph)), ..] =
            remaining_contents
        {
            lang = Some(lang_group.clone());
            translated_text = erase_events_lifetime(translated_group);
            remaining_contents = &remaining_contents[8..];
        } else if let [InlineGroupEvent::NonInline(Event::Start(Tag::Paragraph)), InlineGroupEvent::InlineGroup(translated_group), InlineGroupEvent::NonInline(Event::End(Tag::Paragraph)), ..] =
            remaining_contents
        {
            lang = None;
            translated_text = erase_events_lifetime(translated_group);
            remaining_contents = &remaining_contents[3..];
        } else {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                format!("{:?}", remaining_contents),
            ));
        }

        if let Some(lang) = lang {
            if let [Event::Text(lang)] = &lang[..] {
                if *selected_lang != **lang {
                    continue;
                }
            } else {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::InvalidInput,
                    format!("Invalid lang: {:?}", lang),
                ));
            }
        } else {
            if !use_default_lang {
                continue;
            }
        }

        if translation_data.data_map.contains(&original_text) {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                format!("Duplicate entry: {:?}", original_text),
            ));
        } else {
            translation_data
                .data_map
                .insert(original_text.clone(), translated_text);
        }
    }
    Ok(Some(()))
}

impl TranslationData {
    fn load<'ttext>(
        events: impl Iterator<Item = Event<'ttext>>,
        default_lang: Option<&str>,
        lang: &str,
    ) -> std::io::Result<Self> {
        let mut events = events.into_inline_groups().peekable();
        let use_default_lang = default_lang == Some(lang);
        let mut translation_data = TranslationData::default();
        while let Some(()) =
            recognize_one_item(&mut translation_data, &mut events, lang, use_default_lang)?
        {
        }
        Ok(translation_data)
    }

    fn process_inline_group_event<'event>(
        &self,
        e: InlineGroupEvent<'event>,
    ) -> Vec<Event<'event>> {
        match e {
            InlineGroupEvent::NonInline(e) => vec![e],
            InlineGroupEvent::InlineGroup(g) => {
                if let Some(v) = self.data_map.get(&g) {
                    v.clone()
                } else {
                    g
                }
            }
        }
    }
}

pub fn translate<'event>(
    input: impl Iterator<Item = Event<'event>>,
    translation: impl Iterator<Item = Event<'event>>,
    lang: &str,
    default_lang: Option<&str>,
) -> std::io::Result<impl Iterator<Item = Event<'event>>> {
    let t = TranslationData::load(translation, default_lang, lang)?;
    let iter = input
        .into_inline_groups()
        .flat_map(move |e| t.process_inline_group_event(e));
    Ok(iter)
}

#[cfg(test)]
mod tests;
