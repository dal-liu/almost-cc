use std::collections::HashSet;
use std::fmt;
use std::iter;

use l3::*;
use utils::{DisplayResolved, Interner};

use crate::analysis::ReachingDefResult;

#[derive(Debug)]
pub struct DefUseChain<'a, 'b> {
    interner: &'a Interner<Instruction>,
    users: Vec<HashSet<&'b Instruction>>,
}

impl<'a, 'b> DefUseChain<'a, 'b> {
    pub fn new(func: &'b Function, reaching_def: &'a ReachingDefResult) -> Self {
        let interner = &reaching_def.interner;
        let num_insts = interner.len();
        let mut users = vec![HashSet::new(); num_insts];

        for (i, block) in func.basic_blocks.iter().enumerate() {
            for (j, inst) in block.instructions.iter().enumerate() {
                for use_ in inst.uses() {
                    for def_id in &reaching_def.in_[i][j] {
                        if interner.resolve(def_id).defs() == Some(use_) {
                            users[def_id].insert(inst);
                        }
                    }
                }
            }
        }

        Self { interner, users }
    }

    pub fn is_only_user(&self, inst: &Instruction, user: &Instruction) -> bool {
        let users = &self.users[self.interner[inst]];
        users.len() == 1 && users.iter().next() == Some(&user)
    }
}

impl DisplayResolved for DefUseChain<'_, '_> {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        let mut lines: Vec<String> = self
            .users
            .iter()
            .enumerate()
            .map(|(i, users)| {
                let mut line: Vec<String> =
                    iter::once(self.interner.resolve(i).resolved(interner).to_string())
                        .chain(users.iter().map(|user| user.resolved(interner).to_string()))
                        .collect();
                line.sort();
                format!("{}", line.join(", "))
            })
            .collect();
        lines.sort();
        writeln!(f, "{}", lines.join("\n"))
    }
}
