use std::collections::HashMap;

use ir::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operand {
    Argument,
    Constant,
    Local(InstId),
}

#[derive(Debug)]
pub struct UseDefChain {
    def_table: HashMap<SymbolId, Operand>,
}

impl UseDefChain {
    pub fn new(func: &Function) -> Self {
        let def_table = func
            .params
            .iter()
            .map(|param| (param.var, Operand::Argument))
            .chain(
                func.basic_blocks
                    .iter()
                    .enumerate()
                    .flat_map(|(i, block)| {
                        block
                            .instructions
                            .iter()
                            .enumerate()
                            .map(move |(j, inst)| (inst, InstId(i, j)))
                    })
                    .filter_map(|(inst, id)| inst.defs().map(|&def| (def, Operand::Local(id)))),
            )
            .collect();
        Self { def_table }
    }

    pub fn operands(&self, inst: &Instruction) -> impl Iterator<Item = &Operand> {
        inst.uses().map(|use_| match use_ {
            Value::Variable(var) => &self.def_table[var],
            Value::Function(_) | Value::Number(_) => &Operand::Constant,
        })
    }
}
