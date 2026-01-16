use std::fmt;
use std::iter;

use l3::*;
use utils::{BitVector, DisplayResolved, Interner};

use crate::analysis::reaching_def::ReachingDefResult;

#[derive(Debug)]
pub struct DefUseChain<'a> {
    interner: &'a Interner<Instruction>,
    users: Vec<BitVector>,
}

impl<'a> DefUseChain<'a> {
    pub fn new(func: &Function, reaching_def: &'a ReachingDefResult) -> Self {
        let interner = &reaching_def.interner;
        let num_insts = interner.len();
        let mut users = vec![BitVector::new(num_insts); num_insts];

        for (i, block) in func.basic_blocks.iter().enumerate() {
            for (j, inst) in block.instructions.iter().enumerate() {
                for use_ in inst.uses() {
                    for def_id in &reaching_def.in_[i][j] {
                        if interner
                            .resolve(def_id)
                            .defs()
                            .is_some_and(|def| def == use_)
                        {
                            users[def_id].set(interner[inst]);
                        }
                    }
                }
            }
        }

        Self { interner, users }
    }

    pub fn is_only_user(&self, inst: &Instruction, user: &Instruction) -> bool {
        let users = &self.users[self.interner[inst]];
        users.count() == 1
            && users
                .iter()
                .next()
                .is_some_and(|idx| self.interner.resolve(idx) == user)
    }
}

impl DisplayResolved for DefUseChain<'_> {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        let mut lines: Vec<String> =
            self.users
                .iter()
                .enumerate()
                .map(|(i, users)| {
                    let mut line: Vec<String> =
                        iter::once(self.interner.resolve(i).resolved(interner).to_string())
                            .chain(users.iter().map(|idx| {
                                self.interner.resolve(idx).resolved(interner).to_string()
                            }))
                            .collect();
                    line.sort();
                    format!("{}", line.join(", "))
                })
                .collect();
        lines.sort();
        writeln!(f, "{}", lines.join("\n"))
    }
}
