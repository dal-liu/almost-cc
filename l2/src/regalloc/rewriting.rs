use std::collections::HashSet;

use l2::*;
use utils::Interner;

use crate::analysis::{compute_liveness, compute_loops};
use crate::regalloc::coloring::{ColoringResult, color_graph};
use crate::regalloc::interference::build_interference;
use crate::regalloc::spilling::spill;

pub fn allocate_registers(func: &mut Function, interner: &mut Interner<String>) {
    let prefix = "S";
    let mut suffix = 0;
    let mut prev_spilled = HashSet::new();

    loop {
        let liveness = compute_liveness(func);
        let mut interference = build_interference(func, &liveness);
        let loops = compute_loops(func);
        let coloring = color_graph(func, &liveness, &mut interference, &loops, &prev_spilled);

        if coloring.spill_nodes.is_empty() {
            rewrite_program(func, &coloring);
            break;
        }

        for var in &coloring.spill_nodes {
            let spilled = spill(func, var, prefix, &mut suffix, interner);
            prev_spilled.extend(spilled.into_iter());
        }
    }
}

fn rewrite_program(func: &mut Function, coloring: &ColoringResult) {
    func.basic_blocks
        .iter_mut()
        .flat_map(|block| &mut block.instructions)
        .for_each(|inst| {
            inst.defs()
                .into_iter()
                .chain(inst.uses())
                .filter(|val| matches!(val, Value::Variable(_)))
                .for_each(|var| {
                    inst.replace_value(&var, &coloring.color[&var]);
                })
        });
}
