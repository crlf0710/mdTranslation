//! This provides markdown event iterator adapters that group inline events together.

use crate::utils::{is_block_event, normalize_inlines};

#[derive(PartialEq, Debug)]
pub enum InlineGroupEvent<'event> {
    NonInline(pulldown_cmark::Event<'event>),
    InlineGroup(Vec<pulldown_cmark::Event<'event>>),
}

struct InlineGroupEvents<'event, Iter>
where
    Iter: Iterator<Item = pulldown_cmark::Event<'event>>,
{
    inner_iter: core::iter::Peekable<Iter>,
    conservative: bool,
    allow_unpaired_block_events: bool,
    noninline_event_stack: Vec<pulldown_cmark::Tag<'event>>,
}

impl<'event, Iter> InlineGroupEvents<'event, Iter>
where
    Iter: Iterator<Item = pulldown_cmark::Event<'event>>,
{
    pub(crate) fn next_inline_group_event(&mut self) -> Option<InlineGroupEvent<'event>> {
        let first_event;
        loop {
            match self.inner_iter.next() {
                Some(e) => {
                    if let pulldown_cmark::Event::Html(s) = &e {
                        if !self.conservative
                            && &**s == crate::roundtrip::SPECIAL_SEPARATOR_WITH_EOL
                        {
                            continue;
                        }
                    }
                    first_event = e;
                    break;
                }
                None => {
                    if !self.allow_unpaired_block_events && !self.noninline_event_stack.is_empty() {
                        panic!("Unpaired markdown block events found");
                    }
                    return None;
                }
            };
        }
        if is_block_event(&first_event, &self.noninline_event_stack) {
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
            Some(InlineGroupEvent::NonInline(first_event))
        } else {
            let mut inlines = vec![first_event];
            while let Some(cur_event) = self.inner_iter.peek() {
                if is_block_event(&cur_event, &self.noninline_event_stack) {
                    break;
                }
                inlines.extend(self.inner_iter.next());
            }
            normalize_inlines(&mut inlines, self.conservative);
            Some(InlineGroupEvent::InlineGroup(inlines))
        }
    }
}

pub struct InlineGroupIter<'event, Iter>
where
    Iter: Iterator<Item = pulldown_cmark::Event<'event>>,
{
    events: InlineGroupEvents<'event, Iter>,
}

impl<'event, Iter> InlineGroupIter<'event, Iter>
where
    Iter: Iterator<Item = pulldown_cmark::Event<'event>>,
{
    pub fn new(parser: Iter) -> Self {
        let events = InlineGroupEvents {
            inner_iter: parser.peekable(),
            conservative: true,
            allow_unpaired_block_events: false,
            noninline_event_stack: Default::default(),
        };
        InlineGroupIter { events }
    }
    pub fn set_conservative(&mut self, conservative: bool) {
        self.events.conservative = conservative;
    }
}

impl<'event, Iter> Iterator for InlineGroupIter<'event, Iter>
where
    Iter: Iterator<Item = pulldown_cmark::Event<'event>>,
{
    type Item = InlineGroupEvent<'event>;
    fn next(&mut self) -> Option<InlineGroupEvent<'event>> {
        self.events.next_inline_group_event()
    }
}

pub(crate) trait InlineGroupIteratorExt<'event>:
    Iterator<Item = pulldown_cmark::Event<'event>>
{
    fn into_inline_groups(self) -> InlineGroupIter<'event, Self>
    where
        Self: Sized;
    fn into_inline_groups_nonconservative(self) -> InlineGroupIter<'event, Self>
    where
        Self: Sized;
}

impl<'event, T> InlineGroupIteratorExt<'event> for T
where
    T: Iterator<Item = pulldown_cmark::Event<'event>>,
{
    fn into_inline_groups(self) -> InlineGroupIter<'event, Self> {
        InlineGroupIter::new(self)
    }
    fn into_inline_groups_nonconservative(self) -> InlineGroupIter<'event, Self> {
        let mut iter = InlineGroupIter::new(self);
        iter.set_conservative(false);
        iter
    }
}
