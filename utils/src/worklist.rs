use std::collections::{HashSet, VecDeque};
use std::hash::Hash;

#[derive(Debug)]
pub struct Worklist<T> {
    queue: VecDeque<T>,
    set: HashSet<T>,
}

impl<T: Copy + Eq + Hash> Worklist<T> {
    pub fn new() -> Self {
        Self {
            queue: VecDeque::new(),
            set: HashSet::new(),
        }
    }

    pub fn pop(&mut self) -> Option<T> {
        self.queue.pop_front().inspect(|item| {
            self.set.remove(item);
        })
    }

    pub fn push(&mut self, item: T) {
        if self.set.insert(item) {
            self.queue.push_back(item);
        }
    }
}

impl<T: Copy + Eq + Hash> Extend<T> for Worklist<T> {
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        for item in iter {
            self.push(item);
        }
    }
}

impl<T: Copy + Eq + Hash> Default for Worklist<T> {
    fn default() -> Self {
        Self::new()
    }
}
