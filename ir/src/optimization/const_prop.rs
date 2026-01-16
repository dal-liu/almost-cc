use ir::*;

use crate::analysis::use_def::{Operand, UseDefChain};

pub fn propagate_constants(func: &mut Function) -> bool {
    let use_def = UseDefChain::new(func);
    let mut modified = false;

    for (i, block) in func.basic_blocks.iter_mut().enumerate() {
        let num_insts = block.instructions.len();
        for (j, mut inst) in
            std::mem::replace(&mut block.instructions, Vec::with_capacity(num_insts))
                .into_iter()
                .enumerate()
        {
            for (use_, op) in inst
                .uses()
                .collect::<Vec<SymbolId>>()
                .into_iter()
                .zip(&use_def.operands[i][j])
            {
                if let Operand::Local(def_inst) = use_def.interner.resolve(op) {
                    match def_inst {
                        Instruction::Assign { src, .. } if matches!(src, Value::Number(_)) => {
                            inst.replace_use(use_, src);
                            modified = true;
                        }
                        _ => (),
                    }
                }
            }

            block.instructions.push(inst);
        }

        let term = &mut block.terminator;
        for (use_, op) in term
            .uses()
            .collect::<Vec<SymbolId>>()
            .into_iter()
            .zip(&use_def.operands[i][num_insts])
        {
            if let Operand::Local(def_inst) = use_def.interner.resolve(op) {
                match def_inst {
                    Instruction::Assign { src, .. } if matches!(src, Value::Number(_)) => {
                        term.replace_use(use_, src);
                        modified = true;
                    }
                    _ => (),
                }
            }
        }
    }

    modified
}
