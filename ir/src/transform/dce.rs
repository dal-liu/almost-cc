use ir::*;

use crate::analysis::def_use::DefUseChain;

pub fn deadcode_elimination(func: &mut Function) -> bool {
    let def_use = DefUseChain::new(func);
    let mut modified = false;

    for block in &mut func.basic_blocks {
        block.instructions.retain(|inst| {
            if !matches!(inst, Instruction::Define { .. })
                && inst.defs().is_some()
                && def_use.users(inst).next().is_none()
            {
                modified = true;
                false
            } else {
                true
            }
        });
    }

    modified
}
