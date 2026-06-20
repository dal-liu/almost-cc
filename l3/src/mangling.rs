use std::collections::HashMap;

use l3::*;

pub fn mangle_labels(prog: &mut Program) -> (String, u32) {
    let prefix = format!("{}{}", longest_label(prog), "_global_");
    let mut suffix = 0;

    for func in &mut prog.functions {
        let mut label_to_mangled = HashMap::new();

        for block in &mut func.basic_blocks {
            for inst in &mut block.instructions {
                match inst {
                    Instruction::Label(label)
                    | Instruction::Branch(label)
                    | Instruction::BranchCondition { label, .. } => {
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

fn longest_label(prog: &Program) -> &str {
    prog.functions
        .iter()
        .flat_map(|func| &func.basic_blocks)
        .flat_map(|block| &block.instructions)
        .fold(None, |longest, inst| match inst {
            Instruction::Label(label)
            | Instruction::Branch(label)
            | Instruction::BranchCondition { label, .. } => {
                let label = prog.interner.resolve(label.0);
                match longest {
                    None => Some(label),
                    Some(longest) if label.len() > longest.len() => Some(label),
                    _ => longest,
                }
            }
            _ => longest,
        })
        .map_or("", |longest| longest)
}
