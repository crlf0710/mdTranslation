use super::vec_map::VecMap;
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
