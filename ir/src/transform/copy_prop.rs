use std::collections::{BTreeMap, HashSet};
use std::mem;

use ir::*;

use crate::analysis::def_use::DefUseChain;

pub fn run_copy_prop_pass(func: &mut Function) -> bool {
    let def_use = DefUseChain::new(func);
    let mut to_delete = HashSet::new();
    let mut to_replace = BTreeMap::new();
    let mut modified = false;

    for (i, block) in func.basic_blocks.iter().enumerate() {
        for (j, inst) in block.instructions.iter().enumerate() {
            match inst {
                Instruction::Assign {
                    dst,
                    src: Value::Variable(var),
                } => {
                    to_delete.insert(InstId(i, j));
                    to_replace.insert(*dst, *var);
                    modified = true;
                }

                Instruction::PhiNode { dst, vals } => {
                    if vals.len() == 1
                        && let &Value::Variable(var) = &vals[0].val
                    {
                        to_delete.insert(InstId(i, j));
                        to_replace.insert(*dst, var);
                        modified = true;
                    }
                }

                _ => (),
            }
        }
    }

    for (orig, repl) in to_replace {
        for &user_id in def_use.users.get(&orig).into_iter().flatten() {
            for use_ in func
                .instruction_mut(user_id)
                .expect("user id should be valid instruction")
                .uses_mut()
            {
                if matches!(use_, &mut Value::Variable(var) if var == orig) {
                    *use_ = Value::Variable(repl);
                }
            }
        }
    }

    for (i, block) in func.basic_blocks.iter_mut().enumerate() {
        let num_insts = block.instructions.len();

        for (j, inst) in mem::replace(&mut block.instructions, Vec::with_capacity(num_insts))
            .into_iter()
            .enumerate()
        {
            if !to_delete.contains(&InstId(i, j)) {
                block.instructions.push(inst);
            }
        }
    }

    modified
}
