use crate::inline_group::{InlineGroupEvent, InlineGroupIteratorExt};
use pulldown_cmark::escape::{StrWrite, WriteWrapper};
use pulldown_cmark::{Event, Tag};
use std::io::{self, Write};
use std::mem;
use std::rc::Rc;
use std::{cell::Cell, collections::VecDeque};

/// Iterate over an `Iterator` of `Event`s, generate HTML for each `Event`, and
/// push it to a `String`.
#[allow(unused)]
pub(crate) fn push_markdown<'a, I>(s: &mut String, iter: I)
where
    I: Iterator<Item = Event<'a>>,
{
    MarkdownWriter::new(iter, s).run().unwrap();
}

/// Iterate over an `Iterator` of `Event`s, generate Markdown for each `Event`, and
/// write it out to a writable stream.
///
/// **Note**: using this function with an unbuffered writer like a file or socket
/// will result in poor performance. Wrap these in a
/// [`BufWriter`](https://doc.rust-lang.org/std/io/struct.BufWriter.html) to
/// prevent unnecessary slowdowns.
pub(crate) fn write_markdown<'a, I, W>(writer: W, iter: I) -> io::Result<()>
where
    I: Iterator<Item = Event<'a>>,
    W: Write,
{
    MarkdownWriter::new(iter, WriteWrapper(writer)).run()
}

struct MarkdownWriter<'a, I, W> {
    /// Iterator supplying events.
    iter: I,

    /// Writer to write to.
    writer: W,

    phantom: core::marker::PhantomData<&'a ()>,
}

impl<'a, I, W> MarkdownWriter<'a, I, W>
where
    I: Iterator<Item = Event<'a>>,
    W: StrWrite,
{
    fn new(iter: I, writer: W) -> Self {
        MarkdownWriter {
            iter,
            writer,
            phantom: core::marker::PhantomData,
        }
    }

    fn run(mut self) -> io::Result<()> {
        let events = self.iter.into_inline_groups();
        // split events into groups and normalize.
        let traverse_groups = split_events_into_traverse_groups(events);

        // traverse groups and output
        let mut remaining_groups = &traverse_groups[..];
        let mut context = vec![];
        while !remaining_groups.is_empty() {
            match remaining_groups {
                [InlineOrBlockTraverseGroup::Descending(desc), tail @ ..] => {
                    let context_len = context.len();
                    let mut desc_counter = 0;
                    for tag in desc.iter() {
                        if context_len <= desc_counter {
                            break;
                        }
                        if !context
                            .get(context_len - desc_counter - 1)
                            .map(|t: &AnnotatedTag<'_>| t.match_pair(tag))
                            .unwrap_or(false)
                        {
                            continue;
                        }
                        desc_counter += 1;
                    }
                    if desc_counter < desc.len() {
                        eprintln!("Requesting finishing blocks in the order {:?}, while actually finishing blocks {:?} on stack", 
                            desc, &context[context_len - desc_counter..]);
                        eprintln!("Full context is: {:?}", context);
                    }
                    let (remaining, removal) = context.split_at(context_len - desc_counter);
                    process_block_removal(&mut self.writer, remaining, removal)?;
                    if let [InlineOrBlockTraverseGroup::Ascending(asc), ..] = tail {
                        process_tag_transition(&mut self.writer, remaining, removal, asc)?;
                    }
                    context.drain(context_len - desc_counter..);
                    remaining_groups = tail;
                }
                [InlineOrBlockTraverseGroup::Ascending(asc), tail @ ..] => {
                    process_block_incoming(&mut self.writer, &context, asc)?;
                    context.extend(asc.iter().cloned());
                    remaining_groups = tail;
                }
                [InlineOrBlockTraverseGroup::BlockSingleton(block), tail @ ..] => {
                    process_block_singleton(&mut self.writer, &context, block)?;
                    if let [InlineOrBlockTraverseGroup::BlockSingleton(next_block), ..] = tail {
                        process_block_transition(&mut self.writer, &context, block, next_block)?;
                    } else if let [InlineOrBlockTraverseGroup::Ascending(asc), ..] = tail {
                        process_block_transition2(&mut self.writer, &context, block, asc)?;
                    }
                    remaining_groups = tail;
                }
                [InlineOrBlockTraverseGroup::InlineGroup(inlines), tail @ ..] => {
                    process_inlines_group(&mut self.writer, &context, inlines)?;
                    remaining_groups = tail;
                }
                [InlineOrBlockTraverseGroup::EarlyStop, ..] => {
                    break;
                }
                [] => {
                    unreachable!()
                }
            }
        }
        Ok(())
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum ListStyleKind {
    Style1,
    Style2,
}

impl ListStyleKind {
    fn flip(self) -> Self {
        match self {
            Self::Style1 => Self::Style2,
            Self::Style2 => Self::Style1,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum ListStyle {
    Unordered(ListStyleKind),
    Ordered(ListStyleKind, u64),
}

#[derive(Copy, Clone, PartialEq, Debug)]
enum HeadingStyle {
    ATX,
    ATXWithClosing,
    Setext,
}

#[derive(Clone, Debug)]
enum AnnotatedTag<'event> {
    Root {
        next_descendent_list_style_kind: Rc<Cell<ListStyleKind>>,
    },
    List {
        self_style: Rc<Cell<ListStyle>>,
        next_descendent_list_style_kind: Rc<Cell<ListStyleKind>>,
    },
    Item {
        is_thin: Cell<bool>,
    },
    ThinPara,
    Heading {
        level: u32,
        self_style: Rc<Cell<HeadingStyle>>,
    },
    Other(Tag<'event>),
}

impl<'event> AnnotatedTag<'event> {
    fn match_pair(&self, rhs: &Self) -> bool {
        match (self, rhs) {
            (AnnotatedTag::Root { .. }, AnnotatedTag::Root { .. }) => true,
            (
                AnnotatedTag::List { self_style, .. },
                AnnotatedTag::List {
                    self_style: rhs_style,
                    ..
                },
            ) => match (self_style.get(), rhs_style.get()) {
                (ListStyle::Ordered(..), ListStyle::Ordered(..)) => true,
                (ListStyle::Unordered(..), ListStyle::Unordered(..)) => true,
                _ => false,
            },
            (AnnotatedTag::Item { .. }, AnnotatedTag::Item { .. }) => true,
            (AnnotatedTag::ThinPara { .. }, AnnotatedTag::ThinPara { .. }) => true,
            (
                AnnotatedTag::Heading { level: level1, .. },
                AnnotatedTag::Heading { level: level2, .. },
            ) => *level1 == *level2,
            (AnnotatedTag::Other(self_tag), AnnotatedTag::Other(other_tag)) => {
                self_tag == other_tag
            }
            _ => false,
        }
    }
}

impl<'event> From<Tag<'event>> for AnnotatedTag<'event> {
    fn from(v: Tag<'event>) -> Self {
        match v {
            Tag::List(style) => {
                let liststyle = match style {
                    None => ListStyle::Unordered(ListStyleKind::Style1),
                    Some(ordinal) => ListStyle::Ordered(ListStyleKind::Style1, ordinal),
                };
                AnnotatedTag::List {
                    self_style: Rc::new(Cell::new(liststyle)),
                    next_descendent_list_style_kind: Rc::new(Cell::new(ListStyleKind::Style1)),
                }
            }
            Tag::Item => AnnotatedTag::Item {
                is_thin: Cell::new(false),
            },
            Tag::Heading(level) => AnnotatedTag::Heading {
                level,
                self_style: Rc::new(Cell::new(HeadingStyle::ATX)),
            },
            _ => AnnotatedTag::Other(v),
        }
    }
}

enum InlineOrBlockTraverseGroup<'event> {
    Ascending(Vec<AnnotatedTag<'event>>),
    Descending(Vec<AnnotatedTag<'event>>),
    BlockSingleton(Event<'event>),
    InlineGroup(Vec<Event<'event>>),
    EarlyStop,
}

fn split_events_into_traverse_groups<'event>(
    events: impl Iterator<Item = InlineGroupEvent<'event>>,
) -> Vec<InlineOrBlockTraverseGroup<'event>> {
    let mut events = events.peekable();
    let mut traverse_groups = vec![];
    while let Some(item) = events.next() {
        match item {
            InlineGroupEvent::InlineGroup(g) => {
                traverse_groups.push(InlineOrBlockTraverseGroup::InlineGroup(g));
            }
            InlineGroupEvent::NonInline(e) => match e {
                Event::Start(first_tag) => {
                    let mut tags = vec![first_tag.into()];
                    while let Some(InlineGroupEvent::NonInline(Event::Start(cur_tag))) =
                        events.peek()
                    {
                        tags.push(cur_tag.clone().into());
                        events.next();
                    }
                    traverse_groups.push(InlineOrBlockTraverseGroup::Ascending(tags));
                }
                Event::End(first_tag) => {
                    let mut tags = vec![first_tag.into()];
                    while let Some(InlineGroupEvent::NonInline(Event::End(cur_tag))) = events.peek()
                    {
                        tags.push(cur_tag.clone().into());
                        events.next();
                    }
                    traverse_groups.push(InlineOrBlockTraverseGroup::Descending(tags));
                }
                _ => traverse_groups.push(InlineOrBlockTraverseGroup::BlockSingleton(e)),
            },
        }
    }
    normalize_traverse_groups(&mut traverse_groups);
    traverse_groups
}

fn is_early_stop_skippable<'event>(tag: &AnnotatedTag<'event>) -> bool {
    match tag {
        AnnotatedTag::Root {..} => true,
        AnnotatedTag::Heading { self_style, ..} => {
            self_style.get() != HeadingStyle::ATX
        }
        AnnotatedTag::List {..} => true,
        AnnotatedTag::ThinPara => true,
        AnnotatedTag::Item {..} => true,
        AnnotatedTag::Other(tag) => {
            match tag {
                Tag::Paragraph => true,
                Tag::CodeBlock(kind) => {
                    use pulldown_cmark::CodeBlockKind;
                    match kind {
                        CodeBlockKind::Indented => true,
                        CodeBlockKind::Fenced(_) => false,
                    }
                }
                _ => false,
            }
        }
    }
}

fn normalize_traverse_groups<'event>(
    traverse_groups: &mut Vec<InlineOrBlockTraverseGroup<'event>>,
) {
    let root = AnnotatedTag::Root {
        next_descendent_list_style_kind: Rc::new(Cell::new(ListStyleKind::Style1)),
    };
    // insert fictional "root" tag to the beginning and the end.
    if let Some(InlineOrBlockTraverseGroup::Ascending(asc)) = traverse_groups.first_mut() {
        asc.insert(0, root.clone());
    } else {
        traverse_groups.insert(0, InlineOrBlockTraverseGroup::Ascending(vec![root.clone()]));
    }
    if let Some(InlineOrBlockTraverseGroup::Descending(desc)) = traverse_groups.last_mut() {
        desc.push(root);
    } else {
        traverse_groups.push(InlineOrBlockTraverseGroup::Descending(vec![root]));
    }

    // check whether needs early stop here.
    let mut early_stop = false;
    if let [.., InlineOrBlockTraverseGroup::InlineGroup(group), InlineOrBlockTraverseGroup::Descending(desc)] =
        &traverse_groups[..]
    {
        if let Some(Event::Text(s)) = group.last() {
            if !s.ends_with('\n') && desc.iter().all(is_early_stop_skippable) {
                early_stop = true;
            }
        }
    }

    for idx in 0..traverse_groups.len() {
        if let [InlineOrBlockTraverseGroup::Ascending(asc), InlineOrBlockTraverseGroup::InlineGroup(group), InlineOrBlockTraverseGroup::Descending(_desc), ..] =
            &mut traverse_groups[idx..]
        {
            if let [.., AnnotatedTag::Other(Tag::CodeBlock(_))] = &asc[..] {
                if let [.., Event::Text(s)] = &mut group[..] {
                    if s.ends_with('\n') {
                        let mut str = s.to_string();
                        str.pop();
                        *s = str.into();
                    }
                }
            } else if let [.., AnnotatedTag::Heading{self_style, ..}] = &asc[..] {
                if let [Event::Text(s)] = &mut group[..] {
                    if s.is_empty() {
                        self_style.set(HeadingStyle::ATXWithClosing);
                    }
                } else if group.contains(&Event::SoftBreak) {
                    self_style.set(HeadingStyle::Setext);
                }
            }
        }
    }

    if early_stop {
        traverse_groups.insert(
            traverse_groups.len() - 1,
            InlineOrBlockTraverseGroup::EarlyStop,
        );
    }
    // merge sibling html blocks since they're actually one single block.
    // NOTE: this is not the proper way.
    for idx in 0.. {
        if let Some(group) = traverse_groups.get(idx) {
            if let InlineOrBlockTraverseGroup::BlockSingleton(Event::Html(_)) = group {
                let count = traverse_groups[idx + 1..]
                    .iter()
                    .take_while(|x| {
                        matches!(
                            x,
                            InlineOrBlockTraverseGroup::BlockSingleton(Event::Html(_))
                        )
                    })
                    .count();
                if count > 0 {
                    let content: String = traverse_groups[idx..idx + 1 + count]
                        .iter()
                        .flat_map(|x| {
                            if let InlineOrBlockTraverseGroup::BlockSingleton(Event::Html(s)) = x {
                                s.chars()
                            } else {
                                unreachable!()
                            }
                        })
                        .collect();
                    traverse_groups.splice(
                        idx..idx + 1 + count,
                        Some(InlineOrBlockTraverseGroup::BlockSingleton(Event::Html(
                            content.into(),
                        ))),
                    );
                }
            } else if let InlineOrBlockTraverseGroup::InlineGroup(_) = group {
                let mut thin = false;
                if idx > 0 {
                    if let Some(InlineOrBlockTraverseGroup::Ascending(asc)) =
                        traverse_groups.get(idx - 1)
                    {
                        if let Some(AnnotatedTag::Item { .. }) = asc.last() {
                            thin = true;
                        }
                    }
                }
                match traverse_groups.get(idx + 1) {
                    Some(InlineOrBlockTraverseGroup::BlockSingleton(_))
                    | Some(InlineOrBlockTraverseGroup::Ascending(_)) => {
                        thin = true;
                    }
                    Some(InlineOrBlockTraverseGroup::Descending(desc)) => {
                        if let Some(AnnotatedTag::Item { .. }) = desc.first() {
                            thin = true;
                        }
                    }
                    _ => {}
                }
                if thin {
                    let thin_para = AnnotatedTag::ThinPara;
                    let mut offset = 0;
                    if idx == 0 {
                        traverse_groups.insert(
                            idx + offset,
                            InlineOrBlockTraverseGroup::Ascending(vec![thin_para.clone()]),
                        );
                        offset = 1;
                    } else if let Some(InlineOrBlockTraverseGroup::Ascending(asc)) =
                        traverse_groups.get_mut(idx + offset - 1)
                    {
                        asc.push(thin_para.clone());
                    } else {
                        traverse_groups.insert(
                            idx + offset,
                            InlineOrBlockTraverseGroup::Ascending(vec![thin_para.clone()]),
                        );
                        offset += 1;
                    }
                    mark_item_tag_as_thin(&traverse_groups[..idx + offset]);
                    if let Some(InlineOrBlockTraverseGroup::Descending(desc)) =
                        traverse_groups.get_mut(idx + offset + 1)
                    {
                        desc.insert(0, thin_para);
                    } else {
                        traverse_groups.insert(
                            idx + offset + 1,
                            InlineOrBlockTraverseGroup::Descending(vec![thin_para]),
                        );
                    }
                }
            }
        } else {
            break;
        }
    }
}

fn mark_item_tag_as_thin<'event>(traverse_groups: &[InlineOrBlockTraverseGroup<'event>]) {
    let mut tag_queue = VecDeque::new();
    for group in traverse_groups.iter().rev() {
        match group {
            InlineOrBlockTraverseGroup::BlockSingleton(_)
            | InlineOrBlockTraverseGroup::InlineGroup(_)
            | InlineOrBlockTraverseGroup::EarlyStop => {}
            InlineOrBlockTraverseGroup::Descending(desc) => {
                let mut take_queue = mem::take(&mut tag_queue);
                tag_queue.extend(desc.iter().cloned());
                tag_queue.append(&mut take_queue);
            }
            InlineOrBlockTraverseGroup::Ascending(asc) => {
                'process_tag: for (idx, tag) in asc.iter().enumerate().rev() {
                    while let Some(last_in_queue) = tag_queue.back() {
                        let matched = tag.match_pair(last_in_queue);
                        tag_queue.pop_back();
                        if matched {
                            continue 'process_tag;
                        }
                    }
                    match tag {
                        AnnotatedTag::ThinPara => {
                            continue;
                        }
                        AnnotatedTag::Item { is_thin } => {
                            is_thin.set(true);
                            return;
                        }
                        _ => {
                            eprintln!("Thin para in non-item context: {:?}", &asc[..idx]);
                        }
                    }
                }
            }
        }
    }
}

fn find_active_list_style<'event>(
    tags_in_effect: &[&[AnnotatedTag<'event>]],
) -> Option<Rc<Cell<ListStyle>>> {
    for tag_list in tags_in_effect.iter().rev() {
        for tag in tag_list.iter().rev() {
            match tag {
                AnnotatedTag::List { self_style, .. } => {
                    return Some(self_style.clone());
                }
                AnnotatedTag::Root { .. }
                | AnnotatedTag::Item { .. }
                | AnnotatedTag::ThinPara
                | AnnotatedTag::Heading { .. }
                | AnnotatedTag::Other(_) => {}
            }
        }
    }
    None
}

fn find_next_descendent_list_style_kind<'event>(
    tags_in_effect: &[&[AnnotatedTag<'event>]],
) -> Option<Rc<Cell<ListStyleKind>>> {
    for tag_list in tags_in_effect.iter().rev() {
        for tag in tag_list.iter().rev() {
            match tag {
                AnnotatedTag::Root {
                    next_descendent_list_style_kind,
                } => {
                    return Some(next_descendent_list_style_kind.clone());
                }
                AnnotatedTag::List {
                    next_descendent_list_style_kind,
                    ..
                } => {
                    return Some(next_descendent_list_style_kind.clone());
                }
                AnnotatedTag::Item { .. }
                | AnnotatedTag::ThinPara
                | AnnotatedTag::Heading { .. }
                | AnnotatedTag::Other(_) => {}
            }
        }
    }
    None
}

fn renew_nonnesting_sequence_line_start<'event>(
    writer: &mut dyn StrWrite,
    tags_in_effect: &[&[AnnotatedTag<'event>]],
) -> io::Result<()> {
    use pulldown_cmark::CodeBlockKind;

    for (outer_idx, tag_list) in tags_in_effect.iter().enumerate() {
        for (idx, tag) in tag_list.iter().enumerate() {
            match tag {
                AnnotatedTag::Root { .. } => {
                    // do nothing
                }
                AnnotatedTag::List { .. } => {
                    // do nothing
                }
                AnnotatedTag::Item { .. } => {
                    let mut context_segments = vec![];
                    context_segments.extend(&tags_in_effect[0..outer_idx]);
                    context_segments.push(&tags_in_effect[outer_idx][0..idx]);
                    let list_style = find_active_list_style(&context_segments);
                    if let Some(list_style) = list_style {
                        let style = list_style.get();
                        match style {
                            ListStyle::Ordered(_, ordinal) => {
                                let str = format!("{}  ", ordinal)
                                    .chars()
                                    .map(|_| ' ')
                                    .collect::<String>();
                                writer.write_str(&str)?;
                            }
                            ListStyle::Unordered(_) => writer.write_str("  ")?,
                        }
                    } else {
                        eprintln!(
                            "item encountered but list context not found: {:?}",
                            &context_segments
                        );
                    }
                }
                AnnotatedTag::ThinPara => {
                    // do nothing
                }
                AnnotatedTag::Heading { .. } => {
                    // do nothing
                }
                AnnotatedTag::Other(Tag::List(_))
                | AnnotatedTag::Other(Tag::Item)
                | AnnotatedTag::Other(Tag::Heading(_)) => {
                    unreachable!()
                }
                AnnotatedTag::Other(Tag::Paragraph) => {
                    // do nothing
                }
                AnnotatedTag::Other(Tag::BlockQuote) => {
                    writer.write_str("> ")?;
                }
                AnnotatedTag::Other(Tag::CodeBlock(CodeBlockKind::Indented)) => {
                    writer.write_str("    ")?;
                }
                AnnotatedTag::Other(Tag::CodeBlock(CodeBlockKind::Fenced(_))) => {
                    // do nothing
                }
                _ => {
                    unreachable!()
                }
            }
        }
    }
    Ok(())
}

fn process_block_incoming<'event>(
    writer: &mut dyn StrWrite,
    remaining: &[AnnotatedTag<'event>],
    incoming: &[AnnotatedTag<'event>],
) -> io::Result<()> {
    use pulldown_cmark::CodeBlockKind;

    for (idx, tag) in incoming.iter().enumerate() {
        match tag {
            AnnotatedTag::Root { .. } => {
                // do nothing
            }
            AnnotatedTag::List { self_style, .. } => {
                let active_list_style_kind =
                    find_next_descendent_list_style_kind(&[remaining, &incoming[..idx + 0]]);
                if let Some(active_list_style_kind) = active_list_style_kind {
                    let mut style = self_style.get();
                    match &mut style {
                        ListStyle::Ordered(style_kind, _) => {
                            *style_kind = active_list_style_kind.get();
                        }
                        ListStyle::Unordered(style_kind) => {
                            *style_kind = active_list_style_kind.get();
                        }
                    }
                } else {
                    eprintln!(
                        "list encountered but outer list context not found: {:?}",
                        &[remaining, &incoming[..idx]]
                    );
                }
                // the actual label will be written by AnnotatedTag::Item.
            }
            AnnotatedTag::Item { .. } => {
                let list_style = find_active_list_style(&[remaining, &incoming[..idx + 0]]);
                if let Some(list_style) = list_style {
                    let style = list_style.get();
                    match style {
                        ListStyle::Ordered(list_style_kind, ordinal) => match list_style_kind {
                            ListStyleKind::Style1 => {
                                write!(writer, "{}. ", ordinal)?;
                            }
                            ListStyleKind::Style2 => {
                                write!(writer, "{}) ", ordinal)?;
                            }
                        },
                        ListStyle::Unordered(list_style_kind) => match list_style_kind {
                            ListStyleKind::Style1 => {
                                writer.write_str("* ")?;
                            }
                            ListStyleKind::Style2 => {
                                writer.write_str("- ")?;
                            }
                        },
                    }
                } else {
                    eprintln!(
                        "item encountered but list context not found: {:?}",
                        &[remaining, &incoming[..idx]]
                    );
                }
            }
            AnnotatedTag::ThinPara => {
                // do nothing
            }
            AnnotatedTag::Heading { level, self_style } => {
                match self_style.get() {
                    HeadingStyle::ATX | HeadingStyle::ATXWithClosing => {
                        let level_str = &"#######"[..(*level) as usize];
                        writer.write_str(level_str)?;
                        writer.write_str(" ")?;
                    }
                    _ => {},
                }
            }
            AnnotatedTag::Other(Tag::List(_))
            | AnnotatedTag::Other(Tag::Item)
            | AnnotatedTag::Other(Tag::Heading(_)) => {
                unreachable!()
            }
            AnnotatedTag::Other(Tag::Paragraph) => {
                // do nothing
            }
            AnnotatedTag::Other(Tag::BlockQuote) => {
                writer.write_str("> ")?;
            }
            AnnotatedTag::Other(Tag::CodeBlock(CodeBlockKind::Indented)) => {
                writer.write_str("    ")?;
            }
            AnnotatedTag::Other(Tag::CodeBlock(CodeBlockKind::Fenced(str))) => {
                writer.write_str("````")?;
                writer.write_str(str)?;
                writer.write_str("\n")?;
                renew_nonnesting_sequence_line_start(writer, &[remaining, &incoming[..idx + 1]])?;
            }
            _ => {
                eprintln!(
                    "unhandled enter nesting tag {:?}, within context: {:?}",
                    tag,
                    &[remaining, &incoming[..idx]]
                );
            }
        }
    }

    Ok(())
}

fn process_block_removal<'event>(
    writer: &mut dyn StrWrite,
    remaining: &[AnnotatedTag<'event>],
    removal: &[AnnotatedTag<'event>],
) -> io::Result<()> {
    use pulldown_cmark::CodeBlockKind;

    let mut need_newline = false;

    for (idx, tag) in removal.iter().enumerate().rev() {
        match tag {
            AnnotatedTag::Root { .. } => {
                // do nothing
            }
            AnnotatedTag::List { .. } => {
                // do nothing
            }
            AnnotatedTag::Item { .. } => {
                let list_style = find_active_list_style(&[remaining, &removal[..idx + 0]]);
                if let Some(list_style) = list_style {
                    let style = list_style.get();
                    match style {
                        ListStyle::Ordered(list_style_kind, ordinal) => {
                            list_style.set(ListStyle::Ordered(
                                list_style_kind,
                                ordinal.saturating_add(1),
                            ));
                        }
                        ListStyle::Unordered(_) => {}
                    }
                } else {
                    eprintln!(
                        "item finishing but list context not found: {:?}",
                        &[remaining, &removal[..idx]]
                    );
                }
                need_newline = true;
            }
            AnnotatedTag::ThinPara => {
                need_newline = true;
            }
            AnnotatedTag::Heading { level, self_style } => {
                match self_style.get() {
                    HeadingStyle::ATX => {},
                    HeadingStyle::ATXWithClosing => {
                        let level_str = &"#######"[..(*level) as usize];
                        writer.write_str(" ")?;
                        writer.write_str(level_str)?;
                    },
                    HeadingStyle::Setext => {
                        writer.write_str("\n")?;
                        renew_nonnesting_sequence_line_start(writer, &[remaining, &removal[..idx]])?;
                        if *level == 1 {
                            writer.write_str("=========")?;
                        } else {
                            writer.write_str("---------")?;
                        }
                    }
                }
                need_newline = true;
            }
            AnnotatedTag::Other(Tag::Paragraph) | AnnotatedTag::Other(Tag::BlockQuote) => {
                need_newline = true;
            }
            AnnotatedTag::Other(Tag::List(_))
            | AnnotatedTag::Other(Tag::Item)
            | AnnotatedTag::Other(Tag::Heading(_)) => {
                unreachable!()
            }
            AnnotatedTag::Other(Tag::CodeBlock(CodeBlockKind::Indented)) => {
                need_newline = true;
            }
            AnnotatedTag::Other(Tag::CodeBlock(CodeBlockKind::Fenced(_))) => {
                writer.write_str("\n")?;
                renew_nonnesting_sequence_line_start(writer, &[remaining, &removal[..idx]])?;
                //UNNECESSARY: need_newline = false;
                writer.write_str("````")?;
                need_newline = true;
            }
            _ => {
                eprintln!(
                    "unhandled exit nesting event {:?}, within context: {:?}",
                    tag,
                    &[remaining, &removal[..idx]]
                );
            }
        }
    }
    if need_newline {
        writer.write_str("\n")?;
        renew_nonnesting_sequence_line_start(writer, &[remaining])?;
    }
    Ok(())
}

fn process_tag_transition<'event>(
    writer: &mut dyn StrWrite,
    remaining: &[AnnotatedTag<'event>],
    removal: &[AnnotatedTag<'event>],
    incoming: &[AnnotatedTag<'event>],
) -> io::Result<()> {
    use pulldown_cmark::CodeBlockKind;
    enum TransitionStrategy {
        DoNothing,
        FlipListStyle,
        ExtraNewlineAndRenew,
    }
    let mut strategy = None;
    match (removal, incoming) {
        ([], []) => {
            strategy = Some(TransitionStrategy::DoNothing);
        }
        ([AnnotatedTag::Other(Tag::Paragraph), ..], [AnnotatedTag::Other(Tag::Paragraph), ..])
        | (
            [AnnotatedTag::Other(Tag::Paragraph), ..],
            [AnnotatedTag::Other(Tag::CodeBlock(CodeBlockKind::Indented)), ..],
        )
        | (
            [AnnotatedTag::Other(Tag::BlockQuote), ..],
            [AnnotatedTag::Other(Tag::BlockQuote), ..],
        )
        | ([AnnotatedTag::Other(Tag::BlockQuote), ..], [AnnotatedTag::Other(Tag::Paragraph), ..])
        | ([AnnotatedTag::Other(Tag::Paragraph), ..], [AnnotatedTag::List { .. }, ..])
        | ([AnnotatedTag::List { .. }, ..], [AnnotatedTag::Other(Tag::Paragraph), ..]) => {
            strategy = Some(TransitionStrategy::ExtraNewlineAndRenew);
        }
        ([AnnotatedTag::List { .. }, ..], [AnnotatedTag::List { .. }, ..]) => {
            strategy = Some(TransitionStrategy::FlipListStyle)
        }
        ([AnnotatedTag::Other(Tag::Paragraph), ..], [AnnotatedTag::Heading { .. }, ..])
        | (
            [AnnotatedTag::Other(Tag::Paragraph), ..],
            [AnnotatedTag::Other(Tag::CodeBlock(CodeBlockKind::Fenced(_))), ..],
        )
        | ([AnnotatedTag::Other(Tag::Paragraph), ..], [AnnotatedTag::Other(Tag::BlockQuote), ..])
        | ([AnnotatedTag::Heading { .. }, ..], [AnnotatedTag::Other(Tag::Paragraph), ..])
        | ([AnnotatedTag::Heading { .. }, ..], [AnnotatedTag::Heading { .. }, ..])
        | ([AnnotatedTag::Heading { .. }, ..], [AnnotatedTag::Other(Tag::CodeBlock(_)), ..])
        | ([AnnotatedTag::Heading { .. }, ..], [AnnotatedTag::Other(Tag::BlockQuote), ..])
        | (
            [AnnotatedTag::Other(Tag::CodeBlock(_)), ..],
            [AnnotatedTag::Other(Tag::Paragraph), ..],
        )
        | ([AnnotatedTag::Other(Tag::CodeBlock(_)), ..], [AnnotatedTag::Heading { .. }, ..])
        | ([AnnotatedTag::Other(Tag::CodeBlock(_)), ..], [AnnotatedTag::List { .. }, ..])
        | (
            [AnnotatedTag::Other(Tag::CodeBlock(_)), ..],
            [AnnotatedTag::Other(Tag::BlockQuote), ..],
        )
        | ([AnnotatedTag::Other(Tag::BlockQuote), ..], [AnnotatedTag::Heading { .. }, ..])
        | (
            [AnnotatedTag::Other(Tag::BlockQuote), ..],
            [AnnotatedTag::Other(Tag::CodeBlock(_)), ..],
        )
        | ([AnnotatedTag::Other(Tag::BlockQuote), ..], [AnnotatedTag::List { .. }, ..]) => {
            strategy = Some(TransitionStrategy::DoNothing);
        }
        (
            [AnnotatedTag::Item { is_thin: is_thin1 }, ..],
            [AnnotatedTag::Item { is_thin: is_thin2 }, ..],
        ) => {
            if !is_thin1.get() && !is_thin2.get() {
                strategy = Some(TransitionStrategy::ExtraNewlineAndRenew);
            } else {
                strategy = Some(TransitionStrategy::DoNothing);
            }
        }
        _ => {}
    }

    let strategy = match strategy {
        None => {
            eprintln!(
                "unhandled transition between event, context = {:?}, removing = {:?}, adding = {:?}",
                remaining,
                removal,
                incoming);
            TransitionStrategy::DoNothing
        }
        Some(s) => s,
    };
    match strategy {
        TransitionStrategy::DoNothing => {
            // do nothing
        }
        TransitionStrategy::FlipListStyle => {
            let next_list_style_kind = find_next_descendent_list_style_kind(&[remaining]);
            if let Some(next_list_style_kind) = next_list_style_kind {
                let style = next_list_style_kind.get().flip();
                next_list_style_kind.set(style);
            } else {
                eprintln!(
                    "sibling lists encountered but list context not found: {:?}",
                    &[remaining]
                );
            }
        }
        TransitionStrategy::ExtraNewlineAndRenew => {
            writer.write_str("\n")?;
            renew_nonnesting_sequence_line_start(writer, &[remaining])?;
        }
    }
    Ok(())
}

fn process_block_singleton<'event>(
    writer: &mut dyn StrWrite,
    context: &[AnnotatedTag<'event>],
    block: &Event<'event>,
) -> io::Result<()> {
    match block {
        Event::Rule => {
            if let Some(AnnotatedTag::Item { .. }) = context.last() {
                writer.write_str("---")?;
            } else {
                writer.write_str("***")?;
            }
        }
        Event::Html(s) => {
            writer.write_str(s)?;
        }
        _ => {
            eprintln!("unsupported block: {:?}", block);
        }
    }
    Ok(())
}

fn process_block_transition<'event>(
    writer: &mut dyn StrWrite,
    remaining: &[AnnotatedTag<'event>],
    _removal: &Event<'event>,
    _incoming: &Event<'event>,
) -> io::Result<()> {
    writer.write_str("\n")?;
    renew_nonnesting_sequence_line_start(writer, &[remaining])?;
    Ok(())
}

fn process_block_transition2<'event>(
    writer: &mut dyn StrWrite,
    remaining: &[AnnotatedTag<'event>],
    _removal: &Event<'event>,
    _incoming: &[AnnotatedTag<'event>],
) -> io::Result<()> {
    writer.write_str("\n")?;
    renew_nonnesting_sequence_line_start(writer, &[remaining])?;
    Ok(())
}

fn process_inlines_group<'event>(
    writer: &mut dyn StrWrite,
    context: &[AnnotatedTag<'event>],
    inlines: &Vec<Event<'event>>,
) -> io::Result<()> {
    //resolve_emphasis_and_strong(inlines);
    //Self::process_definition_and_references(writer, context, sequence, &resolution)?;
    output_inline_contents(writer, &[context], &inlines[..])?;
    // eprintln!("FIXME: Output inlines");

    Ok(())
}

fn output_inline_contents<'event>(
    writer: &mut dyn StrWrite,
    context: &[&[AnnotatedTag<'event>]],
    inlines: &[Event<'event>],
) -> io::Result<()> {
    let containing_tag = context
        .iter()
        .rev()
        .flat_map(|x| x.iter().rev())
        .cloned()
        .next();
    for inline in inlines {
        match inline {
            Event::Text(s) => {
                let need_escape =
                    !matches!(containing_tag, Some(AnnotatedTag::Other(Tag::CodeBlock(_))));
                if need_escape {
                    let escaped = escape_text(s);
                    writer.write_str(&escaped)?;
                } else {
                    writer.write_str(s)?;
                    if s.ends_with('\n') {
                        renew_nonnesting_sequence_line_start(writer, context)?;
                    }
                }
            }
            Event::Html(s) => writer.write_str(s)?,
            Event::SoftBreak => {
                writer.write_str("\n")?;
                renew_nonnesting_sequence_line_start(writer, context)?;
            }
            Event::HardBreak => {
                writer.write_str("\\\n")?;
                renew_nonnesting_sequence_line_start(writer, context)?;
            }
            Event::Code(s) => {
                writer.write_str("`")?;
                writer.write_str(s)?;
                writer.write_str("`")?;
            }
            Event::Start(Tag::Emphasis) | Event::End(Tag::Emphasis) => {
                writer.write_str("*")?;
            }
            Event::Start(Tag::Strong) | Event::End(Tag::Strong) => {
                writer.write_str("**")?;
            }
            _ => {
                eprintln!("unsupported inline: {:?}", inline);
            }
        }
    }
    Ok(())
}

fn escape_text<'event>(input: &pulldown_cmark::CowStr<'event>) -> pulldown_cmark::CowStr<'event> {
    let mut rewrite_str = None;
    let input_str = &*input;
    for (offset, ch) in input_str.char_indices() {
        if ch.is_ascii_punctuation() {
            let rewrite_str = rewrite_str.get_or_insert_with(|| input_str[..offset].to_string());
            rewrite_str.push('\\');
            rewrite_str.push(ch);
        } else if ch.is_ascii_control() {
            let rewrite_str = rewrite_str.get_or_insert_with(|| input_str[..offset].to_string());
            let str = format!("&#{};", ch as usize);
            *rewrite_str += &str;
        } else {
            if let Some(rewrite_str) = rewrite_str.as_mut() {
                rewrite_str.push(ch);
            }
        }
    }
    if let Some(str) = rewrite_str {
        str.into()
    } else {
        input.clone()
    }
}

#[cfg(test)]
mod tests;
