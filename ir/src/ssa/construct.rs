use ir::*;
use utils::{BitVector, Interner};

use crate::analysis::{DominanceFrontier, DominatorTree};

pub fn construct_ssa_form(
    func: &mut Function,
    string_interner: &mut Interner<String>,
    dom_tree: &DominatorTree,
    dom_front: &DominanceFrontier,
) {
    let var_id_interner = func
        .params
        .iter()
        .map(|param| param.var)
        .chain(
            func.basic_blocks
                .iter()
                .flat_map(|block| block.instructions.iter().filter_map(|inst| inst.defs())),
        )
        .fold(Interner::new(), |mut interner, def| {
            interner.intern(def);
            interner
        });

    let mut def_blocks = vec![BitVector::new(func.basic_blocks.len()); var_id_interner.len()];
    for param in &func.params {
        def_blocks[var_id_interner[&param.var]].set(0);
    }
    for (i, block) in func.basic_blocks.iter().enumerate() {
        for def in block.instructions.iter().filter_map(|inst| inst.defs()) {
            def_blocks[var_id_interner[&def]].set(i);
        }
    }

    place_phi_nodes(func, &dom_front, &var_id_interner, &def_blocks);
    rename_variables(func, string_interner, &dom_tree, &var_id_interner);
}

fn place_phi_nodes(
    func: &mut Function,
    dom_front: &DominanceFrontier,
    var_id_interner: &Interner<SymbolId>,
    def_blocks: &[BitVector],
) {
    let num_blocks = func.basic_blocks.len();
    let mut iter_count = 0;
    let mut worklist = BitVector::new(num_blocks);
    let mut work = vec![0; num_blocks];
    let mut has_already = vec![0; num_blocks];

    for (i, blocks) in def_blocks.iter().enumerate() {
        let dst = *var_id_interner.resolve(i);
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
    var_id_interner: &Interner<SymbolId>,
) {
    let num_vars = var_id_interner.len();
    let mut counter = vec![0; num_vars];
    let mut stack = vec![vec![]; num_vars];

    for param in &mut func.params {
        let var = &mut param.var;
        let v = var_id_interner[var];
        let i = counter[v];
        *var = SymbolId(string_interner.intern(format!("{}{}", string_interner.resolve(var.0), i)));
        stack[v].push(i);
        counter[v] += 1;
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
    let block = &mut func.basic_blocks[node.0];
    let mut old_lhs = Vec::new();

    let mut new_var = |old_var: SymbolId, i: u32| {
        SymbolId(string_interner.intern(format!("{}{}", string_interner.resolve(old_var.0), i)))
    };

    for inst in block.instructions.iter_mut() {
        if !matches!(inst, Instruction::PhiNode { .. }) {
            for use_ in inst.uses().collect::<Vec<SymbolId>>() {
                let v = var_id_interner[&use_];
                let i = *stack[v].last().expect("stack should not be empty");
                inst.replace_use(use_, new_var(use_, i));
            }
        }

        if let Some(def) = inst.defs() {
            let v = var_id_interner[&def];
            let i = counter[v];
            inst.replace_def(new_var(def, i));
            stack[v].push(i);
            counter[v] += 1;
            old_lhs.push(v);
        }
    }

    let term = &mut block.terminator;
    if let Some(use_) = term.uses() {
        let v = var_id_interner[&use_];
        let i = *stack[v].last().expect("stack should not be empty");
        term.replace_use(new_var(use_, i));
    }

    let label = block.label;
    for succ in &func.cfg.successors[node.0] {
        for inst in func.basic_blocks[succ.0].instructions.iter_mut() {
            match inst {
                Instruction::PhiNode { vals, .. } => {
                    let val = vals
                        .iter_mut()
                        .find(|val| val.label == label)
                        .expect("phi node should have value from predecessor");
                    if let Value::Variable(var) = &mut val.val {
                        let v = var_id_interner[var];
                        let i = *stack[v].last().expect("stack should not be empty");
                        *var = new_var(*var, i);
                    }
                }
                _ => (),
            }
        }
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

    for v in old_lhs {
        stack[v].pop();
    }
}
