use ir::*;
use utils::interner::Interner;

pub fn split_critical_edges(prog: &mut Program) {
    for func in &mut prog.functions {
        let prefix = longest_label(func, &prog.interner).to_owned() + "_";
        let critical_edges: Vec<(BlockId, BlockId)> = critical_edges(func).collect();

        for (suffix, &(u, v)) in critical_edges.iter().enumerate() {
            let new_label = SymbolId(prog.interner.intern(format!("{}{}", prefix, suffix)));
            let old_label = func.basic_blocks[v.0].label;

            if let Instruction::BranchCondition {
                true_label,
                false_label,
                ..
            } = &mut func.basic_blocks[u.0].terminator
            {
                let label_to_replace = if *true_label == old_label {
                    true_label
                } else {
                    false_label
                };
                *label_to_replace = new_label;
            } else {
                unreachable!("critical edge source block should have 2 destinations");
            }

            func.basic_blocks.push(BasicBlock {
                label: new_label,
                instructions: Vec::new(),
                terminator: Instruction::Branch(old_label),
            });
        }

        func.cfg = ControlFlowGraph::new(&func.basic_blocks);
    }
}

fn longest_label<'a>(func: &Function, interner: &'a Interner<String>) -> &'a str {
    func.basic_blocks
        .iter()
        .map(|block| interner.resolve(block.label.0))
        .fold(None, |longest, label| match longest {
            None => Some(label),
            Some(longest) if label.len() > longest.len() => Some(label),
            _ => longest,
        })
        .map_or("", |longest| longest)
}

fn critical_edges(func: &Function) -> impl Iterator<Item = (BlockId, BlockId)> {
    let cfg = &func.cfg;
    (0..func.basic_blocks.len())
        .filter(|&u| cfg.successors[u].len() > 1)
        .flat_map(move |u| {
            cfg.successors[u]
                .iter()
                .filter_map(move |&v| (cfg.predecessors[v.0].len() > 1).then_some((BlockId(u), v)))
        })
}
