use std::collections::HashSet;

use l2::*;
use utils::Interner;

use crate::analysis::dominators::DominatorTree;
use crate::analysis::liveness::compute_liveness;
use crate::analysis::loops::LoopForest;
use crate::regalloc::coloring::{ColoringResult, color_graph};
use crate::regalloc::interference::InterferenceGraph;
use crate::regalloc::spilling::spill;

pub fn allocate_registers(func: &mut Function, interner: &mut Interner<String>) {
    let prefix = "S";
    let mut suffix = 0;
    let mut prev_spilled = HashSet::new();

    loop {
        let liveness = compute_liveness(func);
        let mut interference = InterferenceGraph::new(func, &liveness);
        let dominators = DominatorTree::new(func);
        let loops = LoopForest::new(func, &dominators);
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
    for block in &mut func.basic_blocks {
        for inst in &mut block.instructions {
            let vars: Vec<Value> = inst
                .defs()
                .chain(inst.uses())
                .filter(|val| matches!(val, Value::Variable(_)))
                .collect();
            for var in vars {
                inst.replace_value(&var, &coloring.color[&var]);
            }
        }
    }
}
