use pulldown_cmark::{Event, Tag};

mod inline_group;
mod nested_inline_group;
mod vec_map;
mod vec_set;
mod puncttable;

pub(crate) use inline_group::{InlineGroupIteratorExt, InlineGroupEvent};
pub(crate) use nested_inline_group::{NestedInlineGroupIteratorExt, NestedInlineGroupEvent};
pub(crate) use vec_map::VecMap;
pub(crate) use vec_set::VecSet;

pub(crate) fn is_whitespace (ch: char) -> bool {
    ch.is_whitespace()
}
pub(crate) use puncttable::is_punctuation;

pub(crate) fn normalize_inlines(inlines: &mut Vec<Event<'_>>, conservative: bool) {
    let mut idx = 0;
    while let Some(first_item) = inlines.get(idx) {
        if let Event::Text(first_text) = first_item {
            if !conservative || !first_text.ends_with('\n') {
                let mut idx_end = idx + 1;
                let mut replace_text = None;
                while let Some(Event::Text(t)) = inlines.get(idx_end) {
                    if conservative && t.ends_with('\n') {
                        break;
                    }
                    replace_text.get_or_insert_with(|| first_text.to_string());
                    replace_text.as_mut().unwrap().extend(t.chars());
                    idx_end += 1;
                }
                if let Some(replace_text) = replace_text {
                    inlines.splice(idx..idx_end, Some(Event::Text(replace_text.into())));
                }
            }
        }
        idx += 1;
    }
}

pub(crate) fn is_block_event<'event>(event: &pulldown_cmark::Event<'event>, context: &Vec<Tag<'event>>) -> bool {
    match event {
        Event::Start(tag) | Event::End(tag) => {
            let tag: &pulldown_cmark::Tag = tag;
            match tag {
                Tag::Paragraph
                | Tag::Heading(_)
                | Tag::BlockQuote
                | Tag::CodeBlock(_)
                | Tag::List(_)
                | Tag::Item
                | Tag::Table(_)
                | Tag::TableHead
                | Tag::TableRow
                | Tag::TableCell => true,
                Tag::FootnoteDefinition(_) => {
                    // FIXME: verify if it is correct to regard it as block-level.
                    // since it is actually out-of-band.
                    true
                }
                Tag::Emphasis
                | Tag::Strong
                | Tag::Strikethrough
                | Tag::Link(_, _, _)
                | Tag::Image(_, _, _) => false,
            }
        }
        Event::Html(text) => {
            context.is_empty() || text.ends_with("\n")
        }
        Event::Rule => true,
        Event::Text(_)
        | Event::Code(_)
        | Event::FootnoteReference(_)
        | Event::SoftBreak
        | Event::HardBreak
        | Event::TaskListMarker(_) => false,
    }
}

