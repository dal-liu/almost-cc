use std::mem;

use l2::*;
use utils::Interner;

pub fn spill(
    func: &mut Function,
    var: &Value,
    prefix: &str,
    suffix: &mut i32,
    interner: &mut Interner<String>,
) -> Vec<Value> {
    let mut modified = false;
    let mut spill_vars = Vec::new();
    let offset = func.locals * 8;

    for block in &mut func.basic_blocks {
        let num_insts = block.instructions.len();
        for inst in mem::replace(&mut block.instructions, Vec::with_capacity(num_insts)) {
            let spill_use = inst.uses().any(|use_| &use_ == var);
            let spill_def = inst.defs().any(|def| &def == var);

            let spill_var = if spill_use || spill_def {
                let new_var =
                    Value::Variable(SymbolId(interner.intern(format!("{}{}", prefix, suffix))));
                *suffix += 1;
                modified = true;
                spill_vars.push(new_var);
                Some(new_var)
            } else {
                None
            };

            if spill_use {
                if let Some(new_var) = spill_var {
                    block.instructions.push(Instruction::Load {
                        dst: new_var,
                        src: Value::Register(Register::RSP),
                        offset,
                    });
                }
            }

            if spill_use || spill_def {
                let mut new_inst = inst;
                if let Some(ref new_var) = spill_var {
                    new_inst.replace_value(var, new_var);
                }
                block.instructions.push(new_inst);
            } else {
                block.instructions.push(inst);
            }

            if spill_def {
                if let Some(new_var) = spill_var {
                    block.instructions.push(Instruction::Store {
                        dst: Value::Register(Register::RSP),
                        offset,
                        src: new_var,
                    });
                }
            }
        }
    }

    if modified {
        func.locals += 1;
    }

    spill_vars
}
