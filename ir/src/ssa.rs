use std::collections::BTreeMap;

use ir::*;
use utils::BitVector;

use crate::analysis::{DominanceFrontier, DominatorTree};

pub fn construct_ssa_form(func: &mut Function) {
    let dom_tree = DominatorTree::new(func);
    let dom_frontier = DominanceFrontier::new(func, &dom_tree);

    let num_blocks = func.basic_blocks.len();
    let mut def_blocks = BTreeMap::new();
    for (i, block) in func.basic_blocks.iter().enumerate() {
        for def in block.instructions.iter().filter_map(|inst| inst.defs()) {
            def_blocks
                .entry(def)
                .or_insert(BitVector::new(num_blocks))
                .set(i);
        }
    }

    let mut iter_count = 0;
    let mut worklist = BitVector::new(num_blocks);
    let mut work = vec![0; num_blocks];
    let mut has_already = vec![0; num_blocks];

    for (var, blocks) in &def_blocks {
        iter_count += 1;
        for node in blocks {
            work[node] = iter_count;
            worklist.set(node);
        }

        while let Some(u) = worklist.iter().next() {
            worklist.reset(u);

            for v in &dom_frontier.frontier[u] {
                if has_already[v] < iter_count {
                    let phi = Instruction::PhiNode {
                        dst: *var,
                        vals: blocks
                            .iter()
                            .map(|node| PhiValue {
                                val: Value::Variable(*var),
                                label: *dom_frontier.interner.resolve(node),
                            })
                            .collect(),
                    };
                    func.basic_blocks[v].instructions.insert(0, phi);

                    has_already[v] = iter_count;

                    if work[v] < iter_count {
                        work[v] = iter_count;
                        worklist.set(v);
                    }
                }
            }
        }
    }
}
