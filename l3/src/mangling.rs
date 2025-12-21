use std::collections::HashMap;

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

    let mut suffix = 0;
    for func in &mut prog.functions {
        let mut label_to_mangled = HashMap::new();

        for block in &mut func.basic_blocks {
            for inst in &mut block.instructions {
                match inst {
                    Instruction::Label(label)
                    | Instruction::Branch(label)
                    | Instruction::BranchCond { label, .. } => {
                        let is_new = !label_to_mangled.contains_key(label);
                        *label = *label_to_mangled.entry(*label).or_insert_with(|| {
                            SymbolId(prog.interner.intern(format!("{}{}", prefix, suffix)))
                        });
                        suffix += is_new as u32;
                    }
                    _ => (),
                }
            }
        }
    }

    (prefix, suffix)
}
