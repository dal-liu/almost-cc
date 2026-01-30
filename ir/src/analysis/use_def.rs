use std::collections::HashMap;
use std::fmt;

use ir::*;
use utils::{DisplayResolved, Interner};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Operand {
    Argument,
    Constant,
    Local(Instruction),
}

impl DisplayResolved for Operand {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        match self {
            Self::Argument => write!(f, "arg"),
            Self::Constant => write!(f, "const"),
            Self::Local(inst) => write!(f, "{}", inst.resolved(interner)),
        }
    }
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
                    .flat_map(|block| &block.instructions)
                    .filter_map(|inst| {
                        inst.defs()
                            .and_then(|&def| Some((def, Operand::Local(inst.clone()))))
                    }),
            )
            .fold(HashMap::new(), |mut def_table, (def, op)| {
                def_table.insert(def, op);
                def_table
            });
        Self { def_table }
    }

    pub fn operands(&self, inst: &Instruction) -> impl Iterator<Item = &Operand> {
        inst.uses().map(|use_| match use_ {
            Value::Variable(var) => &self.def_table[var],
            Value::Function(_) | Value::Number(_) => &Operand::Constant,
        })
    }
}
