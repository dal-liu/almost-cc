use ir::*;

pub fn constant_folding(func: &mut Function) -> bool {
    let mut modified = false;

    for block in &mut func.basic_blocks {
        for inst in &mut block.instructions {
            if let Instruction::Binary { dst, lhs, op, rhs } = inst {
                if let (Value::Number(lhs), Value::Number(rhs)) = (lhs, rhs) {
                    let lhs = *lhs;
                    let rhs = *rhs;

                    let res = match op {
                        BinaryOp::Add => lhs + rhs,
                        BinaryOp::Sub => lhs - rhs,
                        BinaryOp::Mul => lhs * rhs,
                        BinaryOp::BitAnd => lhs & rhs,
                        BinaryOp::Shl => lhs << rhs,
                        BinaryOp::Shr => lhs >> rhs,
                        BinaryOp::Lt => (lhs < rhs) as i64,
                        BinaryOp::Le => (lhs <= rhs) as i64,
                        BinaryOp::Eq => (lhs == rhs) as i64,
                        BinaryOp::Ge => (lhs >= rhs) as i64,
                        BinaryOp::Gt => (lhs > rhs) as i64,
                    };

                    *inst = Instruction::Assign {
                        dst: *dst,
                        src: Value::Number(res),
                    };
                    modified = true
                }
            }
        }
    }

    modified
}
