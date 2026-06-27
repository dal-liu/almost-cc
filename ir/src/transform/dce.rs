use std::collections::HashSet;
use std::mem;

use ir::*;
use utils::worklist::Worklist;

use crate::analysis::def_use::DefUseChain;
use crate::analysis::use_def::{Operand, UseDefChain};

pub fn run_dce_pass(func: &mut Function) -> bool {
    let use_def = UseDefChain::new(func);
    let mut def_use = DefUseChain::new(func);
    let mut to_delete = HashSet::new();
    let mut modified = false;

    let mut worklist: Worklist<SymbolId> = func
        .basic_blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .filter_map(|inst| inst.defs())
        .copied()
        .collect();

    while let Some(var) = worklist.pop() {
        if def_use.users(var).next().is_some() {
            continue;
        }

        let Some(def_inst) = use_def.def_table.get(&var).and_then(|op| match op {
            Operand::Local(inst_id)
                if func.instruction(*inst_id).is_some_and(|inst| {
                    !matches!(
                        inst,
                        Instruction::Define { .. }
                            | Instruction::Call { .. }
                            | Instruction::CallResult { .. }
                    )
                }) =>
            {
                Some(*inst_id)
            }
            _ => None,
        }) else {
            continue;
        };

        for var in func
            .instruction(def_inst)
            .unwrap()
            .uses()
            .filter_map(|use_| match use_ {
                Value::Variable(var) => Some(*var),
                _ => None,
            })
        {
            if let Some(users) = def_use.users.get_mut(&var) {
                users.retain(|&inst_id| inst_id != def_inst);
            };

            worklist.push(var);
        }

        modified |= to_delete.insert(def_inst);
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
