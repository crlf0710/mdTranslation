use pulldown_cmark::{Event, Tag};

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

pub(crate) fn is_block_event<'event>(event: &pulldown_cmark::Event<'event>) -> bool {
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

pub(crate) struct VecMap<K, V>(Vec<(K, V)>);

impl<K: PartialEq, V> VecMap<K, V> {
    pub(crate) fn get(&self, k: &K) -> Option<&V> {
        for (existing_k, existing_v) in self.0.iter() {
            if existing_k == k {
                return Some(existing_v);
            }
        }
        None
    }

    pub(crate) fn get_mut(&mut self, k: &K) -> Option<&mut V> {
        for (existing_k, existing_v) in self.0.iter_mut() {
            if existing_k == k {
                return Some(existing_v);
            }
        }
        None
    }

    pub(crate) fn contains(&self, k: &K) -> bool {
        self.get(k).is_some()
    }
    pub(crate) fn insert(&mut self, k: K, v: V) {
        if let Some(existing_v) = self.get_mut(&k) {
            *existing_v = v;
        } else {
            self.0.push((k, v));
        }
    }
}

impl<K, V> IntoIterator for VecMap<K, V> {
    type Item = (K, V);
    type IntoIter = std::vec::IntoIter<(K, V)>;
    fn into_iter(self) -> std::vec::IntoIter<(K, V)> {
        self.0.into_iter()
    }
}

impl<K: PartialEq, V> std::iter::FromIterator<(K, V)> for VecMap<K, V> {
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
        let mut result = VecMap::default();
        for (k, v) in iter {
            result.insert(k, v);
        }
        result
    }
}

impl<K, V> Default for VecMap<K, V> {
    fn default() -> Self {
        VecMap(Default::default())
    }
}

pub(crate) struct VecSet<T>(VecMap<T, ()>);

impl<T> Default for VecSet<T> {
    fn default() -> Self {
        VecSet(Default::default())
    }
}

impl<T> IntoIterator for VecSet<T> {
    type Item = T;
    type IntoIter = VecSetIntoIter<T>;
    fn into_iter(self) -> VecSetIntoIter<T> {
        VecSetIntoIter {
            inner: self.0.into_iter(),
        }
    }
}

impl<T: PartialEq> std::iter::FromIterator<T> for VecSet<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        VecSet(VecMap::from_iter(iter.into_iter().map(|x| (x, ()))))
    }
}

pub(crate) struct VecSetIntoIter<T> {
    inner: std::vec::IntoIter<(T, ())>,
}

impl<T> Iterator for VecSetIntoIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<T> {
        self.inner.next().map(|x| x.0)
    }
}
