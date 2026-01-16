use std::collections::HashMap;
use std::fmt;

use ir::*;
use utils::{BitVector, DisplayResolved, Interner};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operand {
    Argument,
    Local(Instruction),
}

impl DisplayResolved for Operand {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        match self {
            Self::Argument => write!(f, "arg"),
            Self::Local(inst) => write!(f, "{}", inst.resolved(interner)),
        }
    }
}

#[derive(Debug)]
pub struct UseDefChain {
    pub interner: Interner<Operand>,
    pub operands: Vec<Vec<BitVector>>,
}

impl UseDefChain {
    pub fn new(func: &Function) -> Self {
        let (interner, def_table) = func
            .params
            .iter()
            .map(|param| (param.var, Operand::Argument))
            .chain(
                func.basic_blocks
                    .iter()
                    .flat_map(|block| &block.instructions)
                    .filter_map(|inst| {
                        inst.defs()
                            .and_then(|def| Some((def, Operand::Local(inst.clone()))))
                    }),
            )
            .fold(
                (Interner::new(), HashMap::new()),
                |(mut interner, mut def_table), (def, op)| {
                    def_table.insert(def, interner.intern(op));
                    (interner, def_table)
                },
            );

        let mut operands: Vec<Vec<BitVector>> = func
            .basic_blocks
            .iter()
            .map(|block| vec![BitVector::new(interner.len()); block.instructions.len() + 1])
            .collect();
        for (i, block) in func.basic_blocks.iter().enumerate() {
            for (j, inst) in block.instructions.iter().enumerate() {
                for use_ in inst.uses() {
                    operands[i][j].set(def_table[&use_]);
                }
            }
            for use_ in block.terminator.uses() {
                operands[i][block.instructions.len()].set(def_table[&use_]);
            }
        }

        Self { interner, operands }
    }
}

impl DisplayResolved for UseDefChain {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        let lines: Vec<String> = self
            .operands
            .iter()
            .flatten()
            .map(|bitvec| {
                let line: Vec<String> = bitvec
                    .iter()
                    .map(|idx| self.interner.resolve(idx).resolved(interner).to_string())
                    .collect();
                format!("{}", line.join(", "))
            })
            .collect();
        writeln!(f, "{}", lines.join("\n"))
    }
}
