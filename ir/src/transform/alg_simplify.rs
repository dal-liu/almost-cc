use ir::*;

pub fn algebraic_simplification(func: &mut Function) -> bool {
    let mut modified = false;

    for block in &mut func.basic_blocks {
        for old_inst in &mut block.instructions {
            if let Instruction::Binary { dst, lhs, op, rhs } = old_inst {
                let new_inst = match op {
                    BinaryOp::Add => match (&lhs, &rhs) {
                        (Value::Number(0), _) => Some(Instruction::Assign {
                            dst: *dst,
                            src: rhs.clone(),
                        }),
                        (_, Value::Number(0)) => Some(Instruction::Assign {
                            dst: *dst,
                            src: lhs.clone(),
                        }),
                        _ => None,
                    },

                    BinaryOp::Sub | BinaryOp::Shl | BinaryOp::Shr => match rhs {
                        Value::Number(0) => Some(Instruction::Assign {
                            dst: *dst,
                            src: lhs.clone(),
                        }),
                        _ => None,
                    },

                    BinaryOp::Mul => match (&lhs, &rhs) {
                        (Value::Number(0), _) | (_, Value::Number(0)) => {
                            Some(Instruction::Assign {
                                dst: *dst,
                                src: Value::Number(0),
                            })
                        }
                        (Value::Number(1), _) => Some(Instruction::Assign {
                            dst: *dst,
                            src: rhs.clone(),
                        }),
                        (_, Value::Number(1)) => Some(Instruction::Assign {
                            dst: *dst,
                            src: lhs.clone(),
                        }),
                        _ => None,
                    },

                    BinaryOp::BitAnd => match (&lhs, &rhs) {
                        (Value::Number(0), _) | (_, Value::Number(0)) => {
                            Some(Instruction::Assign {
                                dst: *dst,
                                src: Value::Number(0),
                            })
                        }
                        (Value::Number(-1), _) => Some(Instruction::Assign {
                            dst: *dst,
                            src: rhs.clone(),
                        }),
                        (_, Value::Number(-1)) => Some(Instruction::Assign {
                            dst: *dst,
                            src: lhs.clone(),
                        }),
                        _ => None,
                    },

                    _ => None,
                };

                if let Some(new_inst) = new_inst {
                    *old_inst = new_inst;
                    modified = true;
                }
            }
        }
    }

    modified
}
