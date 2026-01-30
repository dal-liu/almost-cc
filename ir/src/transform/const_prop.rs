use std::iter;

use ir::*;

use crate::analysis::use_def::{Operand, UseDefChain};

pub fn constant_propagation(func: &mut Function) -> bool {
    let use_def = UseDefChain::new(func);
    let mut modified = false;

    for block in &mut func.basic_blocks {
        for inst in block
            .instructions
            .iter_mut()
            .chain(iter::once(&mut block.terminator))
        {
            let operands: Vec<&Operand> = use_def.operands(inst).collect();

            for (use_, op) in inst.uses_mut().into_iter().zip(operands) {
                if let Operand::Local(def_inst) = op {
                    match def_inst {
                        Instruction::Assign { src, .. } if matches!(src, Value::Number(_)) => {
                            *use_ = src.clone();
                            modified = true;
                        }
                        Instruction::PhiNode { vals, .. } => {
                            let mut it = vals.iter().map(|val| &val.val);
                            if let Some(Value::Number(first)) = it.next() {
                                if it.all(|val| matches!(val, Value::Number(num) if num == first)) {
                                    *use_ = Value::Number(*first);
                                    modified = true;
                                }
                            };
                        }
                        _ => (),
                    }
                }
            }
        }
    }

    modified
}
