pub trait Assoc<K, V>
    where K: PartialEq, V: Clone
{
    fn assoc(self, key: &K) -> Option<V>;
}

impl<'a, K, V, T> Assoc<K, V> for T
where
    K: PartialEq + 'a,
    V: Clone + 'a,
    T: IntoIterator<Item=&'a (K, V)>
{
    fn assoc(self, key: &K) -> Option<V> {
        self.into_iter()
            .find(|(t, _)| t == key)
            .map(|(_, u)| u)
            .cloned()
    }
}
