use crate::inline_group::{InlineGroupEvent, InlineGroupIteratorExt};
use pulldown_cmark::escape::{StrWrite, WriteWrapper};
use pulldown_cmark::{Event, Tag};
use std::cell::Cell;
use std::io::{self, Write};
use std::rc::Rc;

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
                        process_block_transition(&mut self.writer, remaining, removal, asc)?;
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
                    remaining_groups = tail;
                }
                [InlineOrBlockTraverseGroup::InlineGroup(inlines), tail @ ..] => {
                    process_inlines_group(&mut self.writer, &context, inlines)?;
                    remaining_groups = tail;
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

#[derive(Clone, Debug)]
enum AnnotatedTag<'event> {
    Root {
        next_descendent_list_style_kind: Rc<Cell<ListStyleKind>>,
    },
    List {
        self_style: Rc<Cell<ListStyle>>,
        next_descendent_list_style_kind: Rc<Cell<ListStyleKind>>,
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
            _ => AnnotatedTag::Other(v),
        }
    }
}

enum InlineOrBlockTraverseGroup<'event> {
    Ascending(Vec<AnnotatedTag<'event>>),
    Descending(Vec<AnnotatedTag<'event>>),
    BlockSingleton(Event<'event>),
    InlineGroup(Vec<Event<'event>>),
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

fn normalize_traverse_groups<'event>(
    traverse_groups: &mut Vec<InlineOrBlockTraverseGroup<'event>>,
) {
    let root = AnnotatedTag::Root {
        next_descendent_list_style_kind: Rc::new(Cell::new(ListStyleKind::Style1)),
    };
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
                AnnotatedTag::Root { .. } | AnnotatedTag::Other(_) => {}
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
                AnnotatedTag::Other(_) => {}
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
                AnnotatedTag::Other(Tag::List(_)) => {
                    unreachable!()
                }
                AnnotatedTag::Other(Tag::Paragraph) | AnnotatedTag::Other(Tag::Heading(_)) => {
                    // do nothing
                }
                AnnotatedTag::Other(Tag::BlockQuote) => {
                    writer.write_str("> ")?;
                }
                AnnotatedTag::Other(Tag::CodeBlock(CodeBlockKind::Indented)) => {
                    writer.write_str("    ")?;
                }
                AnnotatedTag::Other(Tag::CodeBlock(CodeBlockKind::Fenced(str))) => {
                    // do nothing
                }
                AnnotatedTag::Other(Tag::Item) => {
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
                // the actual label will be written by Tag::Item.
            }
            AnnotatedTag::Other(Tag::List(_)) => {
                unreachable!()
            }
            AnnotatedTag::Other(Tag::Paragraph) => {
                // do nothing
            }
            AnnotatedTag::Other(Tag::Heading(level)) => {
                let level_str = &"#######"[..(*level) as usize];
                writer.write_str(level_str)?;
                writer.write_str(" ")?;
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
            AnnotatedTag::Other(Tag::Item) => {
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
            AnnotatedTag::Other(Tag::Paragraph)
            | AnnotatedTag::Other(Tag::Heading(_))
            | AnnotatedTag::Other(Tag::BlockQuote) => {
                need_newline = true;
            }
            AnnotatedTag::Other(Tag::Item) => {
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
            AnnotatedTag::Other(Tag::List(_)) => {
                unreachable!()
            }
            AnnotatedTag::Other(Tag::CodeBlock(CodeBlockKind::Indented)) => {
                need_newline = true;
            }
            AnnotatedTag::Other(Tag::CodeBlock(CodeBlockKind::Fenced(_))) => {
                if need_newline {
                    writer.write_str("\n")?;
                    renew_nonnesting_sequence_line_start(writer, &[remaining, &removal[..idx]])?;
                    need_newline = false;
                }
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

fn process_block_transition<'event>(
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
        | ([AnnotatedTag::List { .. }, ..], [AnnotatedTag::Other(Tag::Paragraph), ..]) => {
            strategy = Some(TransitionStrategy::ExtraNewlineAndRenew);
        }
        ([AnnotatedTag::List { .. }, ..], [AnnotatedTag::List { .. }, ..]) => {
            strategy = Some(TransitionStrategy::FlipListStyle)
        }
        ([AnnotatedTag::Other(Tag::Paragraph), ..], [AnnotatedTag::Other(Tag::Heading(_)), ..])
        | (
            [AnnotatedTag::Other(Tag::Paragraph), ..],
            [AnnotatedTag::Other(Tag::CodeBlock(CodeBlockKind::Fenced(_))), ..],
        )
        | ([AnnotatedTag::Other(Tag::Paragraph), ..], [AnnotatedTag::List { .. }, ..])
        | ([AnnotatedTag::Other(Tag::Paragraph), ..], [AnnotatedTag::Other(Tag::BlockQuote), ..])
        | ([AnnotatedTag::Other(Tag::Heading(_)), ..], [AnnotatedTag::Other(Tag::Paragraph), ..])
        | (
            [AnnotatedTag::Other(Tag::Heading(_)), ..],
            [AnnotatedTag::Other(Tag::Heading(_)), ..],
        )
        | (
            [AnnotatedTag::Other(Tag::Heading(_)), ..],
            [AnnotatedTag::Other(Tag::CodeBlock(_)), ..],
        )
        | (
            [AnnotatedTag::Other(Tag::Heading(_)), ..],
            [AnnotatedTag::Other(Tag::BlockQuote), ..],
        )
        | (
            [AnnotatedTag::Other(Tag::CodeBlock(_)), ..],
            [AnnotatedTag::Other(Tag::Paragraph), ..],
        )
        | (
            [AnnotatedTag::Other(Tag::CodeBlock(_)), ..],
            [AnnotatedTag::Other(Tag::Heading(_)), ..],
        )
        | ([AnnotatedTag::Other(Tag::CodeBlock(_)), ..], [AnnotatedTag::List { .. }, ..])
        | (
            [AnnotatedTag::Other(Tag::CodeBlock(_)), ..],
            [AnnotatedTag::Other(Tag::BlockQuote), ..],
        )
        | (
            [AnnotatedTag::Other(Tag::BlockQuote), ..],
            [AnnotatedTag::Other(Tag::Heading(_)), ..],
        )
        | (
            [AnnotatedTag::Other(Tag::BlockQuote), ..],
            [AnnotatedTag::Other(Tag::CodeBlock(_)), ..],
        )
        | ([AnnotatedTag::Other(Tag::BlockQuote), ..], [AnnotatedTag::List { .. }, ..])
        | ([AnnotatedTag::Other(Tag::Item), ..], [AnnotatedTag::Other(Tag::Item), ..]) => {
            strategy = Some(TransitionStrategy::DoNothing);
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
            if let Some(AnnotatedTag::Other(Tag::Item)) = context.last() {
                writer.write_str("---")?;
            } else {
                writer.write_str("***")?;
            }
        }
        _ => {
            eprintln!("unsupported block: {:?}", block);
        }
    }
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
    for inline in inlines {
        match inline {
            Event::Text(s) => writer.write_str(s)?,
            Event::Html(s) => writer.write_str(s)?,
            Event::SoftBreak => {
                writer.write_str("\n")?;
                renew_nonnesting_sequence_line_start(writer, context)?;
            }
            Event::HardBreak => {
                writer.write_str("\\\n")?;
                renew_nonnesting_sequence_line_start(writer, context)?;
            }
            _ => {
                eprintln!("unsupported inline: {:?}", inline);
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests;
