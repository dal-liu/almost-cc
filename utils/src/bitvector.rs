#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BitVector {
    vec: Vec<u64>,
    len: usize,
}

impl BitVector {
    const BITWORD_SIZE: usize = 64;

    pub fn new(len: usize) -> Self {
        let num_words = (len + Self::BITWORD_SIZE - 1) / Self::BITWORD_SIZE;
        Self {
            vec: vec![0; num_words],
            len,
        }
    }

    pub fn test(&self, idx: usize) -> bool {
        assert!(idx < self.len);
        let word_index = idx / Self::BITWORD_SIZE;
        let bit_index = idx % Self::BITWORD_SIZE;
        (self.vec[word_index] & 1u64 << bit_index) != 0
    }

    pub fn union(&mut self, other: &Self) {
        assert_eq!(self.vec.len(), other.vec.len());
        for (a, b) in self.vec.iter_mut().zip(&other.vec) {
            *a |= *b;
        }
    }

    pub fn clear(&mut self) {
        for word in &mut self.vec {
            *word = 0;
        }
    }

    pub fn difference(&mut self, other: &Self) {
        assert_eq!(self.vec.len(), other.vec.len());
        for (a, b) in self.vec.iter_mut().zip(&other.vec) {
            *a &= !*b;
        }
    }

    pub fn iter(&self) -> BitVectorIterator<'_> {
        BitVectorIterator {
            vec: &self.vec,
            word_index: 0,
            bit_offset: 0,
            current_word: if self.vec.is_empty() { 0 } else { self.vec[0] },
        }
    }

    pub fn set(&mut self, idx: usize) {
        assert!(idx < self.len);
        let word_index = idx / Self::BITWORD_SIZE;
        let bit_index = idx % Self::BITWORD_SIZE;
        self.vec[word_index] |= 1u64 << bit_index;
    }

    pub fn reset(&mut self, idx: usize) {
        assert!(idx < self.len);
        let word_index = idx / Self::BITWORD_SIZE;
        let bit_index = idx % Self::BITWORD_SIZE;
        self.vec[word_index] &= !(1u64 << bit_index);
    }

    pub fn any(&self) -> bool {
        self.vec.iter().any(|&word| word != 0)
    }

    pub fn set_from(&mut self, iter: impl Iterator<Item = usize>) {
        for index in iter {
            self.set(index);
        }
    }

    pub fn count(&self) -> usize {
        self.vec.iter().map(|word| word.count_ones() as usize).sum()
    }

    pub fn intersection(&mut self, other: &Self) {
        assert_eq!(self.vec.len(), other.vec.len());
        for (a, b) in self.vec.iter_mut().zip(&other.vec) {
            *a &= *b;
        }
    }

    pub fn reset_from(&mut self, iter: impl Iterator<Item = usize>) {
        for index in iter {
            self.reset(index);
        }
    }
}

impl<'a> IntoIterator for &'a BitVector {
    type Item = usize;
    type IntoIter = BitVectorIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

#[derive(Debug)]
pub struct BitVectorIterator<'a> {
    vec: &'a [u64],
    word_index: usize,
    bit_offset: usize,
    current_word: u64,
}

impl<'a> Iterator for BitVectorIterator<'a> {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.current_word == 0 {
                self.word_index += 1;
                if self.word_index >= self.vec.len() {
                    return None;
                }

                self.current_word = self.vec[self.word_index];
                self.bit_offset = self.word_index * 64;
                continue;
            }

            let bit_position = self.current_word.trailing_zeros() as usize;
            self.current_word &= self.current_word - 1;
            return Some(bit_position + self.bit_offset);
        }
    }
}
