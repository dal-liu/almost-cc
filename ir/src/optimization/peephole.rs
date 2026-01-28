use std::mem;

use ir::*;

pub fn run_peephole_passes(func: &mut Function) -> bool {
    let mut modified = false;
    modified |= constant_folding(func);
    modified |= algebraic_simplification(func);
    modified
}

fn constant_folding(func: &mut Function) -> bool {
    use BinaryOp::*;

    let mut modified = false;

    for block in &mut func.basic_blocks {
        let num_insts = block.instructions.len();
        for old_inst in mem::replace(&mut block.instructions, Vec::with_capacity(num_insts)) {
            match &old_inst {
                Instruction::Binary { dst, lhs, op, rhs } => match (lhs, rhs) {
                    (Value::Number(lhs), Value::Number(rhs)) => {
                        let res = match op {
                            Add => lhs + rhs,
                            Sub => lhs - rhs,
                            Mul => lhs * rhs,
                            BitAnd => lhs & rhs,
                            Shl => lhs << rhs,
                            Shr => lhs >> rhs,
                            Lt => (lhs < rhs) as i64,
                            Le => (lhs <= rhs) as i64,
                            Eq => (lhs == rhs) as i64,
                            Ge => (lhs >= rhs) as i64,
                            Gt => (lhs > rhs) as i64,
                        };
                        block.instructions.push(Instruction::Assign {
                            dst: *dst,
                            src: Value::Number(res),
                        });
                        modified = true
                    }
                    _ => block.instructions.push(old_inst),
                },
                _ => block.instructions.push(old_inst),
            };
        }
    }

    modified
}

fn algebraic_simplification(func: &mut Function) -> bool {
    false
}
