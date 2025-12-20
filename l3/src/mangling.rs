use l3::*;

pub fn mangle_labels(prog: &mut Program) -> (String, u32) {
    let mut prefix = prog
        .functions
        .iter()
        .flat_map(|func| &func.basic_blocks)
        .flat_map(|block| &block.instructions)
        .fold(None, |prefix, inst| match inst {
            Instruction::Label(label)
            | Instruction::Branch(label)
            | Instruction::BranchCond { label, .. } => {
                let label = prog.interner.resolve(label.0);
                match prefix {
                    None => Some(label.clone()),
                    Some(prefix) if label.len() > prefix.len() => Some(label.clone()),
                    _ => prefix,
                }
            }
            _ => prefix,
        })
        .unwrap_or("".to_owned());
    prefix.push_str("_global_");

    let suffix = prog
        .functions
        .iter_mut()
        .flat_map(|func| &mut func.basic_blocks)
        .flat_map(|block| &mut block.instructions)
        .fold(0, |suffix, inst| match inst {
            Instruction::Label(label)
            | Instruction::Branch(label)
            | Instruction::BranchCond { label, .. } => {
                *label = SymbolId(prog.interner.intern(format!("{}{}", prefix, suffix)));
                suffix + 1
            }
            _ => suffix,
        });

    (prefix, suffix)
}
