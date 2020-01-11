use std::collections::HashMap;

/// A map that tracks insertion order.
#[derive(Debug)]
pub struct OrderedMap<K, V>
where
    K: Eq + std::hash::Hash,
{
    key_order: Vec<K>,
    map: HashMap<K, V>,
}

impl<K: Eq + std::hash::Hash + Clone, V> OrderedMap<K, V> {
    pub fn len(&self) -> usize {
        self.map.len()
    }
    pub fn insert(&mut self, k: K, v: V) {
        assert!(self.map.insert(k.clone(), v).is_none());
        self.key_order.push(k);
    }
    pub fn get(&self, k: &K) -> Option<&V> {
        self.map.get(k)
    }
    pub fn values<'a>(&'a self) -> Vec<&'a V> {
        self.key_order.iter().map(|k| &self.map[k]).collect()
    }
}

impl<K: Eq + std::hash::Hash + Clone, V> std::iter::FromIterator<(K, V)> for OrderedMap<K, V> {
    fn from_iter<I: IntoIterator<Item = (K, V)>>(iter: I) -> Self {
        let mut s = Self {
            key_order: Vec::new(),
            map: HashMap::new(),
        };
        for (k, v) in iter {
            s.insert(k, v);
        }
        s
    }
}

impl<K: Eq + std::hash::Hash, V> std::ops::Index<&K> for OrderedMap<K, V> {
    type Output = V;

    fn index(&self, key: &K) -> &Self::Output {
        &self.map[key]
    }
}
