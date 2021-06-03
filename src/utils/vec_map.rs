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
