use std::collections::{HashMap, HashSet};
use std::fmt;
use std::iter;

use l3::*;
use utils::{DisplayResolved, Interner};

use crate::analysis::reaching_def::ReachingDefResult;

#[derive(Debug)]
pub struct DefUseChain {
    pub users: HashMap<InstId, HashSet<InstId>>,
}

impl DefUseChain {
    pub fn new(func: &Function, reaching_def: &ReachingDefResult) -> Self {
        let mut users: HashMap<InstId, HashSet<InstId>> = HashMap::new();

        for (i, block) in func.basic_blocks.iter().enumerate() {
            for (j, inst) in block.instructions.iter().enumerate() {
                for use_ in inst.uses() {
                    for &def_id in reaching_def.in_[i][j]
                        .iter()
                        .map(|idx| reaching_def.interner.resolve(idx))
                    {
                        if def_id.0 == func.basic_blocks.len() {
                            if func.params[def_id.1] == use_ {
                                users.entry(def_id).or_default().insert(InstId(i, j));
                            }
                        } else {
                            if func
                                .instruction(def_id)
                                .expect("inst id should be valid")
                                .defs()
                                .is_some_and(|def| def == use_)
                            {
                                users.entry(def_id).or_default().insert(InstId(i, j));
                            }
                        }
                    }
                }
            }
        }

        Self { users }
    }

    #[allow(dead_code)]
    pub fn display<'a>(&'a self, func: &'a Function) -> DefUseChainDisplay<'a> {
        DefUseChainDisplay {
            func,
            def_use: self,
        }
    }
}

#[derive(Debug)]
pub struct DefUseChainDisplay<'a> {
    func: &'a Function,
    def_use: &'a DefUseChain,
}

impl<'a> DisplayResolved for DefUseChainDisplay<'a> {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        let mut lines: Vec<String> = self
            .def_use
            .users
            .iter()
            .map(|(&def_id, users)| {
                let def_str = if def_id.0 == self.func.basic_blocks.len() {
                    format!("%{}", self.func.params[def_id.1].resolved(interner))
                } else {
                    self.func
                        .instruction(def_id)
                        .expect("inst id should be valid")
                        .resolved(interner)
                        .to_string()
                };
                let mut line: Vec<String> = iter::once(def_str)
                    .chain(users.iter().map(|&user_id| {
                        self.func
                            .instruction(user_id)
                            .expect("inst id should be valid")
                            .resolved(interner)
                            .to_string()
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
