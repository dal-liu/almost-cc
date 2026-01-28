use ir::*;

use crate::analysis::use_def::{Operand, UseDefChain};

type StmtId = usize;

pub fn constant_propagation(func: &mut Function) -> bool {
    let use_def = UseDefChain::new(func);
    let mut modified = false;

    for (i, block) in func.basic_blocks.iter_mut().enumerate() {
        let block_id = BlockId(i);
        for (j, inst) in block.instructions.iter_mut().enumerate() {
            modified |= try_propagate(inst, &use_def, block_id, j);
        }
        modified |= try_propagate(
            &mut block.terminator,
            &use_def,
            block_id,
            block.instructions.len(),
        );
    }

    modified
}

fn try_propagate(
    stmt: &mut impl Statement,
    use_def: &UseDefChain,
    block_id: BlockId,
    stmt_id: StmtId,
) -> bool {
    let mut modified = false;

    for (use_, op) in stmt
        .uses()
        .collect::<Vec<SymbolId>>()
        .into_iter()
        .zip(&use_def.operands[block_id.0][stmt_id])
    {
        if let Operand::Local(def_inst) = use_def.interner.resolve(op) {
            match def_inst {
                Instruction::Assign { src, .. } if matches!(src, Value::Number(_)) => {
                    stmt.replace_use(use_, src);
                    modified = true;
                }
                Instruction::PhiNode { vals, .. } => {
                    let mut it = vals.iter().map(|val| &val.val);
                    if let Some(Value::Number(num)) = it.next() {
                        if it.all(|val| matches!(val, Value::Number(n) if n == num)) {
                            stmt.replace_use(use_, &Value::Number(*num));
                            modified = true;
                        }
                    };
                }
                _ => (),
            }
        }
    }

    modified
}
