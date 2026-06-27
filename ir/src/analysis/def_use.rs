use std::collections::HashMap;
use std::iter;

use ir::*;

pub struct DefUseChain {
    pub users: HashMap<SymbolId, Vec<InstId>>,
}

impl DefUseChain {
    pub fn new(func: &Function) -> Self {
        let mut users: HashMap<SymbolId, Vec<InstId>> = HashMap::new();

        for (i, block) in func.basic_blocks.iter().enumerate() {
            for (j, inst) in block
                .instructions
                .iter()
                .chain(iter::once(&block.terminator))
                .enumerate()
            {
                for &use_ in inst.uses().filter_map(|val| match val {
                    Value::Variable(var) => Some(var),
                    _ => None,
                }) {
                    users.entry(use_).or_default().push(InstId(i, j));
                }
            }
        }

        Self { users }
    }

    pub fn users(&self, def: SymbolId) -> impl Iterator<Item = InstId> {
        self.users.get(&def).into_iter().flatten().copied()
    }
}
