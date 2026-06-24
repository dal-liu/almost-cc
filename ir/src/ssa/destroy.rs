use std::collections::HashMap;

use ir::*;

pub fn destroy_ssa_form(prog: &mut Program) {
    for func in &mut prog.functions {
        let num_blocks = func.basic_blocks.len();
        let label_to_block: HashMap<SymbolId, BlockId> = func
            .basic_blocks
            .iter()
            .enumerate()
            .map(|(i, block)| (block.label, BlockId(i)))
            .collect();
        let mut new_instructions = vec![vec![]; num_blocks];

        for block in &func.basic_blocks {
            for (&dst, vals) in block.instructions.iter().filter_map(|inst| match inst {
                Instruction::PhiNode { dst, vals } => Some((dst, vals)),
                _ => None,
            }) {
                for phi_val in vals {
                    new_instructions[label_to_block[&phi_val.label].0].push(Instruction::Assign {
                        dst,
                        src: phi_val.val.clone(),
                    });
                }
            }
        }

        for (block, new_insts) in func.basic_blocks.iter_mut().zip(new_instructions) {
            block
                .instructions
                .retain(|inst| !matches!(inst, Instruction::PhiNode { .. }));
            block.instructions.extend(new_insts);
        }
    }
}
