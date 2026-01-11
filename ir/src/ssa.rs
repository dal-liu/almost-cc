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
        rename_variables(func, &mut prog.interner, &dom_tree, &def_blocks);
    }
}

fn place_phi_nodes(
    func: &mut Function,
    dom_front: &DominanceFrontier,
    def_blocks: &BTreeMap<SymbolId, BitVector>,
) {
    let num_blocks = func.basic_blocks.len();
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
                    let vals: Vec<PhiValue> = func.cfg.predecessors[v]
                        .iter()
                        .map(|pred| PhiValue {
                            val: Value::Variable(dst),
                            label: func.basic_blocks[pred.0].label,
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
    string_interner: &mut Interner<String>,
    dom_tree: &DominatorTree,
    def_blocks: &BTreeMap<SymbolId, BitVector>,
) {
    let var_id_interner = func
        .params
        .iter()
        .map(|param| &param.var)
        .chain(def_blocks.keys())
        .fold(Interner::new(), |mut interner, &def| {
            interner.intern(def);
            interner
        });

    let num_vars = var_id_interner.len();
    let mut counter = vec![0; num_vars];
    let mut stack = vec![vec![]; num_vars];

    for param in &mut func.params {
        let var = &mut param.var;
        let idx = var_id_interner[var];
        let i = counter[idx];
        *var = SymbolId(string_interner.intern(format!("{}{}", string_interner.resolve(idx), i)));
        stack[idx].push(i);
        counter[idx] += 1;
    }

    search(
        func,
        string_interner,
        dom_tree,
        &var_id_interner,
        &mut counter,
        &mut stack,
        BlockId(0),
    );
}

fn search(
    func: &mut Function,
    string_interner: &mut Interner<String>,
    dom_tree: &DominatorTree,
    var_id_interner: &Interner<SymbolId>,
    counter: &mut Vec<u32>,
    stack: &mut Vec<Vec<u32>>,
    node: BlockId,
) {
    let mut old_lhs = Vec::new();
    let mut new_var = |idx, suffix| {
        SymbolId(string_interner.intern(format!("{}{}", string_interner.resolve(idx), suffix)))
    };

    for inst in func.basic_blocks[node.0].instructions.iter_mut() {
        for use_ in inst.uses().collect::<Vec<SymbolId>>() {
            let idx = var_id_interner[&use_];
            let i = *stack[idx].last().expect("stack should not be empty");
            inst.replace_use(use_, new_var(idx, i));
        }

        if let Some(def) = inst.defs() {
            let idx = var_id_interner[&def];
            let i = counter[idx];
            inst.replace_def(new_var(idx, i));
            stack[idx].push(i);
            counter[idx] += 1;
            old_lhs.push(idx);
        }
    }

    for succ in &func.cfg.successors[node.0] {
        // TODO: this
    }

    for child in dom_tree.children(node) {
        search(
            func,
            string_interner,
            dom_tree,
            var_id_interner,
            counter,
            stack,
            child,
        );
    }

    for &idx in &old_lhs {
        stack[idx].pop();
    }
}
