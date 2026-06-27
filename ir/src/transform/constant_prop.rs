use std::collections::HashSet;
use std::mem;

use ir::*;
use utils::worklist::Worklist;

use crate::analysis::def_use::DefUseChain;

pub fn run_constant_prop_pass(func: &mut Function) -> bool {
    let def_use = DefUseChain::new(func);
    let mut to_delete = HashSet::new();
    let mut modified = false;

    let mut worklist: Worklist<InstId> = func
        .basic_blocks
        .iter()
        .enumerate()
        .flat_map(|(i, block)| (0..block.instructions.len()).map(move |j| InstId(i, j)))
        .collect();

    while let Some(inst_id) = worklist.pop() {
        match func
            .instruction(inst_id)
            .expect("inst id should be valid instruction")
        {
            Instruction::Assign { dst, src } => {
                let &Value::Number(num) = src else {
                    continue;
                };

                let dst = *dst;
                for &user_id in def_use.users.get(&dst).into_iter().flatten() {
                    for use_ in func
                        .instruction_mut(user_id)
                        .expect("user id should be valid instruction")
                        .uses_mut()
                    {
                        if matches!(use_, &mut Value::Variable(var) if var == dst) {
                            *use_ = Value::Number(num);
                        }
                    }

                    worklist.push(user_id);
                }

                modified |= to_delete.insert(inst_id);
            }

            Instruction::PhiNode { dst, vals } => {
                let is_constant = |vals: &[PhiValue]| {
                    let mut iter = vals.iter().map(|phi_val| &phi_val.val);
                    let Some(&Value::Number(first_num)) = iter.next() else {
                        return None;
                    };
                    iter.all(|val| matches!(val, &Value::Number(num) if num == first_num))
                        .then_some(first_num)
                };

                if let Some(num) = is_constant(vals) {
                    *func
                        .instruction_mut(inst_id)
                        .expect("inst id should be valid instruction") = Instruction::Assign {
                        dst: *dst,
                        src: Value::Number(num),
                    };
                    modified = true;
                }
            }

            _ => (),
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
