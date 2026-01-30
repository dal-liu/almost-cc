use std::collections::HashMap;
use std::iter;

use ir::*;

pub struct DefUseChain {
    users: HashMap<SymbolId, Vec<Instruction>>,
}

impl DefUseChain {
    pub fn new(func: &Function) -> Self {
        let mut users: HashMap<SymbolId, Vec<Instruction>> = HashMap::new();

        for block in &func.basic_blocks {
            for inst in block
                .instructions
                .iter()
                .chain(iter::once(&block.terminator))
            {
                for &use_ in inst.uses().filter_map(|val| match val {
                    Value::Variable(var) => Some(var),
                    _ => None,
                }) {
                    users.entry(use_).or_default().push(inst.clone());
                }
            }
        }

        Self { users }
    }

    pub fn users(&self, inst: &Instruction) -> impl Iterator<Item = &Instruction> {
        inst.defs()
            .into_iter()
            .flat_map(|def| self.users.get(def).into_iter().flatten())
    }
}
