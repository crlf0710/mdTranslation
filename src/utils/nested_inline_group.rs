#[derive(PartialEq, Debug)]
pub enum NestedInlineGroupEvent<'event> {
    Inline(pulldown_cmark::Event<'event>),
    Group(
        pulldown_cmark::Tag<'event>,
        Vec<NestedInlineGroupEvent<'event>>,
    ),
}

struct NestedInlineGroupEvents<'event, Iter>
where
    Iter: Iterator<Item = pulldown_cmark::Event<'event>>,
{
    inner_iter: core::iter::Peekable<Iter>,
    allow_unpaired_inline_events: bool,
}

impl<'event, Iter> NestedInlineGroupEvents<'event, Iter>
where
    Iter: Iterator<Item = pulldown_cmark::Event<'event>>,
{
    pub(crate) fn next_nested_inline_group_event(
        &mut self,
    ) -> Option<NestedInlineGroupEvent<'event>> {
        let mut stack: Vec<(
            pulldown_cmark::Tag<'event>,
            Vec<NestedInlineGroupEvent<'event>>,
        )> = Vec::new();
        'first_event: loop {
            let first = self.inner_iter.next()?;
            match first {
                pulldown_cmark::Event::Start(tag) => {
                    stack.push((tag, vec![]));
                    break;
                }
                pulldown_cmark::Event::End(_) => {
                    if !self.allow_unpaired_inline_events {
                        panic!("Unpaired markdown inline events found");
                    }
                    continue 'first_event;
                }
                _ => {
                    return Some(NestedInlineGroupEvent::Inline(first));
                }
            };
        }
        'rest_event: while let Some(next) = self.inner_iter.peek() {
            match next {
                pulldown_cmark::Event::Start(tag) => {
                    stack.push((tag.clone(), vec![]));
                    self.inner_iter.next();
                }
                pulldown_cmark::Event::End(tag) => {
                    let tag = tag.clone();
                    self.inner_iter.next();
                    let (last_tag, _) = stack.last().unwrap();
                    if *last_tag == tag {
                        let (last_tag, last_items) = stack.pop().unwrap();
                        let group_event = NestedInlineGroupEvent::Group(last_tag, last_items);
                        if let Some((_, outer_items)) = stack.last_mut() {
                            outer_items.push(group_event);
                            continue 'rest_event;
                        } else {
                            return Some(group_event);
                        }
                    } else {
                        if !self.allow_unpaired_inline_events {
                            panic!("Unpaired markdown inline events found");
                        }
                        continue 'rest_event;
                    }
                }
                _ => {
                    let event = self.inner_iter.next().unwrap();
                    let inline_event = NestedInlineGroupEvent::Inline(event);
                    let (_, outer_items) = stack.last_mut().unwrap();
                    outer_items.push(inline_event);
                    continue 'rest_event;
                }
            }
        }
        if !self.allow_unpaired_inline_events {
            panic!("Unpaired markdown inline events found");
        }
        'finishing_event: loop {
            let (last_tag, last_items) = stack.pop().unwrap();
            let group_event = NestedInlineGroupEvent::Group(last_tag, last_items);
            if let Some((_, outer_items)) = stack.last_mut() {
                outer_items.push(group_event);
                continue 'finishing_event;
            } else {
                return Some(group_event);
            }
        }
    }
}

pub struct NestedInlineGroupIter<'event, Iter>
where
    Iter: Iterator<Item = pulldown_cmark::Event<'event>>,
{
    events: NestedInlineGroupEvents<'event, Iter>,
}

impl<'event, Iter> NestedInlineGroupIter<'event, Iter>
where
    Iter: Iterator<Item = pulldown_cmark::Event<'event>>,
{
    pub fn new(parser: Iter) -> Self {
        let events = NestedInlineGroupEvents {
            inner_iter: parser.peekable(),
            allow_unpaired_inline_events: false,
        };
        NestedInlineGroupIter { events }
    }
}

impl<'event, Iter> Iterator for NestedInlineGroupIter<'event, Iter>
where
    Iter: Iterator<Item = pulldown_cmark::Event<'event>>,
{
    type Item = NestedInlineGroupEvent<'event>;
    fn next(&mut self) -> Option<NestedInlineGroupEvent<'event>> {
        self.events.next_nested_inline_group_event()
    }
}

pub(crate) trait NestedInlineGroupIteratorExt<'event>:
    Iterator<Item = pulldown_cmark::Event<'event>>
{
    fn into_nested_inline_groups(self) -> NestedInlineGroupIter<'event, Self>
    where
        Self: Sized;
}

impl<'event, T> NestedInlineGroupIteratorExt<'event> for T
where
    T: Iterator<Item = pulldown_cmark::Event<'event>>,
{
    fn into_nested_inline_groups(self) -> NestedInlineGroupIter<'event, Self> {
        NestedInlineGroupIter::new(self)
    }
}
