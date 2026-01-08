use std::collections::BTreeMap;

use ir::*;
use utils::{BitVector, Interner};

use crate::analysis::{DominanceFrontier, DominatorTree};

pub fn construct_ssa_form(prog: &mut Program) {
    for func in &mut prog.functions {
        let dom_tree = DominatorTree::new(func);
        let dom_front = DominanceFrontier::new(func, &dom_tree);

        let mut def_blocks = BTreeMap::new();
        let mut set_def = |def, idx| {
            def_blocks
                .entry(def)
                .or_insert(BitVector::new(func.basic_blocks.len()))
                .set(idx);
        };

        for param in &func.params {
            set_def(param.var, 0);
        }

        for (i, block) in func.basic_blocks.iter().enumerate() {
            for def in block.instructions.iter().filter_map(|inst| inst.defs()) {
                set_def(def, i);
            }
        }

        place_phi_nodes(func, &dom_front, &def_blocks);
        // rename_variables(func, &mut prog.interner, &dom_tree, &def_blocks);
    }
}

fn place_phi_nodes(
    func: &mut Function,
    dom_front: &DominanceFrontier,
    def_blocks: &BTreeMap<SymbolId, BitVector>,
) {
    let num_blocks = func.basic_blocks.len();
    let interner = dom_front.interner;

    let mut iter_count = 0;
    let mut worklist = BitVector::new(num_blocks);
    let mut work = vec![0; num_blocks];
    let mut has_already = vec![0; num_blocks];

    for (&dst, blocks) in def_blocks {
        iter_count += 1;

        for node in blocks {
            work[node] = iter_count;
            worklist.set(node);
        }

        while let Some(u) = worklist.iter().next() {
            worklist.reset(u);

            for v in &dom_front.frontier[u] {
                if has_already[v] < iter_count {
                    let vals: Vec<PhiValue> = func
                        .cfg
                        .predecessors(*interner.resolve(v))
                        .map(|pred| PhiValue {
                            val: Value::Variable(dst),
                            label: pred,
                        })
                        .collect();

                    if vals.len() >= 2 {
                        func.basic_blocks[v]
                            .instructions
                            .insert(0, Instruction::PhiNode { dst, vals });
                    }

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

fn rename_variables(
    func: &mut Function,
    interner: &mut Interner<String>,
    dom_tree: &DominatorTree,
    def_blocks: &BTreeMap<SymbolId, BitVector>,
) {
    todo!()
}
