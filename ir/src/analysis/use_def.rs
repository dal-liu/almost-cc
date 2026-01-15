use std::collections::HashMap;

use ir::*;
use utils::{BitVector, Interner};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operand<'a> {
    Argument,
    Local(&'a Instruction),
}

#[derive(Debug)]
pub struct UseDefChain<'a> {
    interner: Interner<Operand<'a>>,
    operands: Vec<Vec<BitVector>>,
}

impl<'a> UseDefChain<'a> {
    pub fn new(func: &'a Function) -> Self {
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
                            .and_then(|def| Some((def, Operand::Local(inst))))
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
            .map(|block| vec![BitVector::new(interner.len()); block.instructions.len()])
            .collect();
        for (i, block) in func.basic_blocks.iter().enumerate() {
            for (j, inst) in block.instructions.iter().enumerate() {
                for use_ in inst.uses() {
                    operands[i][j].set(def_table[&use_]);
                }
            }
        }

        Self { interner, operands }
    }
}
