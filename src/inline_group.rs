//! This provides markdown event iterator adapters that group inline events together.

use std::usize;

pub enum InlineGroupEvent<'event> {
    NonInline(pulldown_cmark::Event<'event>),
    InlineGroup(Vec<pulldown_cmark::Event<'event>>),
}

fn span_union(
    span1: core::ops::Range<usize>,
    span2: core::ops::Range<usize>,
) -> core::ops::Range<usize> {
    span1.start.min(span2.start)..span1.end.max(span2.end)
}

fn is_block_event<'event>(event: &pulldown_cmark::Event<'event>) -> bool {
    use pulldown_cmark::{Event, Tag};
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
        Event::Html(text) => text.ends_with("\n"),
        Event::Rule => true,
        Event::Text(_)
        | Event::Code(_)
        | Event::FootnoteReference(_)
        | Event::SoftBreak
        | Event::HardBreak
        | Event::TaskListMarker(_) => false,
    }
}

struct InlineGroupEvents<'event> {
    inner_iter: core::iter::Peekable<pulldown_cmark::OffsetIter<'event>>,
    allow_unpaired_block_events: bool,
    noninline_event_stack: Vec<pulldown_cmark::Tag<'event>>,
}

impl<'event> InlineGroupEvents<'event> {
    pub(crate) fn next_inline_group_event(
        &mut self,
    ) -> Option<(InlineGroupEvent<'event>, core::ops::Range<usize>)> {
        let (first_event, first_event_span) = match self.inner_iter.next() {
            Some(e) => e,
            None => {
                if !self.allow_unpaired_block_events && !self.noninline_event_stack.is_empty() {
                    panic!("Unpaired markdown block events found");
                }
                return None;
            }
        };
        if is_block_event(&first_event) {
            match &first_event {
                pulldown_cmark::Event::Start(tag) => {
                    self.noninline_event_stack.push(tag.clone());
                }
                pulldown_cmark::Event::End(tag) => {
                    if self.noninline_event_stack.last() == Some(tag) {
                        self.noninline_event_stack.pop();
                    } else if !self.allow_unpaired_block_events {
                        panic!("Unpaired markdown block events found");
                    }
                }
                _ => {}
            }
            Some((InlineGroupEvent::NonInline(first_event), first_event_span))
        } else {
            let mut inlines = vec![first_event];
            let mut sum_span = first_event_span;
            while let Some((cur_event, cur_span)) = self.inner_iter.peek() {
                if is_block_event(&cur_event) {
                    break;
                }
                sum_span = span_union(sum_span, cur_span.clone());
                inlines.extend(self.inner_iter.next().map(|x| x.0));
            }
            Some((InlineGroupEvent::InlineGroup(inlines), sum_span))
        }
    }
}

pub struct InlineGroupIter<'event> {
    events: InlineGroupEvents<'event>,
}

impl<'event> InlineGroupIter<'event> {
    pub fn new(parser: pulldown_cmark::Parser<'event>) -> Self {
        let events = InlineGroupEvents {
            inner_iter: parser.into_offset_iter().peekable(),
            allow_unpaired_block_events: false,
            noninline_event_stack: Default::default(),
        };
        InlineGroupIter { events }
    }
}

impl<'event> Iterator for InlineGroupIter<'event> {
    type Item = InlineGroupEvent<'event>;
    fn next(&mut self) -> Option<InlineGroupEvent<'event>> {
        self.events.next_inline_group_event().map(|x| x.0)
    }
}
