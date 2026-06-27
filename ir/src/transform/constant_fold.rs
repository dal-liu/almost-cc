use ir::*;

pub fn run_constant_fold_pass(func: &mut Function) -> bool {
    let mut modified = false;

    for block in &mut func.basic_blocks {
        for inst in &mut block.instructions {
            let Instruction::Binary { dst, lhs, op, rhs } = inst else {
                continue;
            };

            let (&mut Value::Number(left_num), &mut Value::Number(right_num)) = (lhs, rhs) else {
                continue;
            };

            let new_num = match op {
                BinaryOp::Add => left_num + right_num,
                BinaryOp::Sub => left_num - right_num,
                BinaryOp::Mul => left_num * right_num,
                BinaryOp::BitAnd => left_num & right_num,
                BinaryOp::Shl => left_num << right_num,
                BinaryOp::Shr => left_num >> right_num,
                BinaryOp::Lt => (left_num < right_num) as i64,
                BinaryOp::Le => (left_num <= right_num) as i64,
                BinaryOp::Eq => (left_num == right_num) as i64,
                BinaryOp::Ge => (left_num >= right_num) as i64,
                BinaryOp::Gt => (left_num > right_num) as i64,
            };

            *inst = Instruction::Assign {
                dst: *dst,
                src: Value::Number(new_num),
            };
            modified = true;
        }
    }

    modified
}
