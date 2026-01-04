use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use std::ops::Index;

pub trait DisplayResolved {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result;

    fn resolved<'a>(&'a self, interner: &'a Interner<String>) -> DisplayResolvedWrapper<'a, Self>
    where
        Self: Sized,
    {
        DisplayResolvedWrapper {
            inner: self,
            interner,
        }
    }
}

pub struct DisplayResolvedWrapper<'a, T: ?Sized> {
    inner: &'a T,
    interner: &'a Interner<String>,
}

impl<'a, T: DisplayResolved + ?Sized> fmt::Display for DisplayResolvedWrapper<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.inner.fmt_with(f, self.interner)
    }
}

#[derive(Debug, Default, Clone)]
pub struct Interner<T> {
    map: HashMap<T, usize>,
    vec: Vec<T>,
}

impl<T: Clone + Eq + Hash> Interner<T> {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            vec: Vec::new(),
        }
    }

    pub fn intern(&mut self, item: T) -> usize {
        *self.map.entry(item).or_insert_with_key(|key| {
            let index = self.vec.len();
            self.vec.push(key.clone());
            index
        })
    }

    pub fn resolve(&self, idx: usize) -> &T {
        &self.vec[idx]
    }

    pub fn len(&self) -> usize {
        self.vec.len()
    }
}

impl<T: Eq + Hash> Index<&T> for Interner<T> {
    type Output = usize;

    fn index(&self, item: &T) -> &Self::Output {
        &self.map[item]
    }
}
