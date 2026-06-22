use ir::*;
use l3;
use utils::interner::Interner;

use crate::analysis::dominators::DominatorTree;
use crate::analysis::loops::LoopInfo;
use crate::tracing::compute_trace_order;

pub fn translate_program(prog: &mut Program) -> l3::Program {
    l3::Program {
        functions: prog
            .functions
            .iter()
            .map(|func| translate_function(func, &mut prog.interner))
            .collect(),
        interner: prog.interner.clone(),
    }
}

fn translate_symbol_id(symbol_id: &SymbolId) -> l3::SymbolId {
    l3::SymbolId(symbol_id.0)
}

fn translate_value(val: &Value) -> l3::Value {
    match val {
        Value::Variable(var) => l3::Value::Variable(translate_symbol_id(var)),
        Value::Function(callee) => l3::Value::Function(translate_symbol_id(callee)),
        Value::Number(num) => l3::Value::Number(*num),
    }
}

fn translate_callee(callee: &Callee) -> l3::Callee {
    match callee {
        Callee::Value(val) => l3::Callee::Value(translate_value(val)),
        Callee::Print => l3::Callee::Print,
        Callee::Input => l3::Callee::Input,
        Callee::TupleError => l3::Callee::TupleError,
        Callee::TensorError => l3::Callee::TensorError,
    }
}

fn translate_binary_instruction(
    dst: &SymbolId,
    lhs: &Value,
    op: &BinaryOp,
    rhs: &Value,
) -> Vec<l3::Instruction> {
    let dst = translate_symbol_id(dst);
    let lhs = translate_value(lhs);
    let rhs = translate_value(rhs);

    let l3_instruction = match op {
        BinaryOp::Add => l3::Instruction::Binary {
            dst,
            lhs,
            op: l3::BinaryOp::Add,
            rhs,
        },
        BinaryOp::Sub => l3::Instruction::Binary {
            dst,
            lhs,
            op: l3::BinaryOp::Sub,
            rhs,
        },
        BinaryOp::Mul => l3::Instruction::Binary {
            dst,
            lhs,
            op: l3::BinaryOp::Mul,
            rhs,
        },
        BinaryOp::BitAnd => l3::Instruction::Binary {
            dst,
            lhs,
            op: l3::BinaryOp::BitAnd,
            rhs,
        },
        BinaryOp::Shl => l3::Instruction::Binary {
            dst,
            lhs,
            op: l3::BinaryOp::Shl,
            rhs,
        },
        BinaryOp::Shr => l3::Instruction::Binary {
            dst,
            lhs,
            op: l3::BinaryOp::Shr,
            rhs,
        },

        BinaryOp::Lt => l3::Instruction::Compare {
            dst,
            lhs,
            cmp: l3::CompareOp::Lt,
            rhs,
        },
        BinaryOp::Le => l3::Instruction::Compare {
            dst,
            lhs,
            cmp: l3::CompareOp::Le,
            rhs,
        },
        BinaryOp::Eq => l3::Instruction::Compare {
            dst,
            lhs,
            cmp: l3::CompareOp::Eq,
            rhs,
        },
        BinaryOp::Ge => l3::Instruction::Compare {
            dst,
            lhs,
            cmp: l3::CompareOp::Ge,
            rhs,
        },
        BinaryOp::Gt => l3::Instruction::Compare {
            dst,
            lhs,
            cmp: l3::CompareOp::Gt,
            rhs,
        },
    };

    vec![l3_instruction]
}

fn translate_index_instruction(
    func: &Function,
    val: &Value,
    idxs: &[Value],
    interner: &mut Interner<String>,
    prefix: &str,
    suffix: &mut u32,
) -> (Vec<l3::Instruction>, l3::SymbolId) {
    let Value::Variable(var) = val else {
        unreachable!("container value should be a variable")
    };
    let offset = new_l3_variable_name(interner, prefix, suffix);
    let mut l3_instructions = Vec::new();

    match func.variable_type(*var) {
        Some(Type::Array(ndims)) if idxs.len() == *ndims => {
            let ndims = *ndims as i64;

            let dims: Vec<l3::SymbolId> = (1..ndims)
                .map(|i| {
                    let address = new_l3_variable_name(interner, prefix, suffix);
                    let dim = new_l3_variable_name(interner, prefix, suffix);
                    l3_instructions.extend([
                        l3::Instruction::Binary {
                            dst: address,
                            lhs: translate_value(val),
                            op: l3::BinaryOp::Add,
                            rhs: l3::Value::Number((i + 1) * 8),
                        },
                        l3::Instruction::Load {
                            dst: dim,
                            src: address,
                        },
                        l3::Instruction::Binary {
                            dst: dim,
                            lhs: l3::Value::Variable(dim),
                            op: l3::BinaryOp::Shr,
                            rhs: l3::Value::Number(1),
                        },
                    ]);
                    dim
                })
                .collect();

            l3_instructions.push(l3::Instruction::Assign {
                dst: offset,
                src: translate_value(&idxs[idxs.len() - 1]),
            });

            for i in 0..idxs.len() - 1 {
                let linearized_idx = new_l3_variable_name(interner, prefix, suffix);

                l3_instructions.push(l3::Instruction::Assign {
                    dst: linearized_idx,
                    src: translate_value(&idxs[i]),
                });

                for j in i..idxs.len() - 1 {
                    l3_instructions.push(l3::Instruction::Binary {
                        dst: linearized_idx,
                        lhs: l3::Value::Variable(linearized_idx),
                        op: l3::BinaryOp::Mul,
                        rhs: l3::Value::Variable(dims[j]),
                    });
                }

                l3_instructions.push(l3::Instruction::Binary {
                    dst: offset,
                    lhs: l3::Value::Variable(offset),
                    op: l3::BinaryOp::Add,
                    rhs: l3::Value::Variable(linearized_idx),
                });
            }

            l3_instructions.extend([
                l3::Instruction::Binary {
                    dst: offset,
                    lhs: l3::Value::Variable(offset),
                    op: l3::BinaryOp::Mul,
                    rhs: l3::Value::Number(8),
                },
                l3::Instruction::Binary {
                    dst: offset,
                    lhs: l3::Value::Variable(offset),
                    op: l3::BinaryOp::Add,
                    rhs: l3::Value::Number(8 + ndims * 8),
                },
            ]);
        }

        Some(Type::Tuple) if idxs.len() == 1 => {
            l3_instructions.extend([
                l3::Instruction::Binary {
                    dst: offset,
                    lhs: translate_value(&idxs[0]),
                    op: l3::BinaryOp::Mul,
                    rhs: l3::Value::Number(8),
                },
                l3::Instruction::Binary {
                    dst: offset,
                    lhs: l3::Value::Variable(offset),
                    op: l3::BinaryOp::Add,
                    rhs: l3::Value::Number(8),
                },
            ]);
        }

        _ => panic!("container type should be array of correct ndims or tuple"),
    }

    l3_instructions.push(l3::Instruction::Binary {
        dst: offset,
        lhs: l3::Value::Variable(offset),
        op: l3::BinaryOp::Add,
        rhs: translate_value(val),
    });

    (l3_instructions, offset)
}

fn translate_new_array_instruction(
    dst: &SymbolId,
    dims: &[Value],
    interner: &mut Interner<String>,
    prefix: &str,
    suffix: &mut u32,
) -> Vec<l3::Instruction> {
    let mut l3_instructions = Vec::with_capacity(dims.len());

    let mut decoded_dims = Vec::with_capacity(dims.len());
    for dim in dims {
        let dst = new_l3_variable_name(interner, prefix, suffix);
        l3_instructions.push(l3::Instruction::Binary {
            dst,
            lhs: translate_value(dim),
            op: l3::BinaryOp::Shr,
            rhs: l3::Value::Number(1),
        });
        decoded_dims.push(dst);
    }

    let array_size = new_l3_variable_name(interner, prefix, suffix);
    l3_instructions.push(l3::Instruction::Assign {
        dst: array_size,
        src: l3::Value::Variable(decoded_dims[0]),
    });

    for i in 1..decoded_dims.len() {
        l3_instructions.push(l3::Instruction::Binary {
            dst: array_size,
            lhs: l3::Value::Variable(array_size),
            op: l3::BinaryOp::Mul,
            rhs: l3::Value::Variable(decoded_dims[i]),
        });
    }

    l3_instructions.extend([
        l3::Instruction::Binary {
            dst: array_size,
            lhs: l3::Value::Variable(array_size),
            op: l3::BinaryOp::Add,
            rhs: l3::Value::Number(dims.len() as i64),
        },
        l3::Instruction::Binary {
            dst: array_size,
            lhs: l3::Value::Variable(array_size),
            op: l3::BinaryOp::Shl,
            rhs: l3::Value::Number(1),
        },
        l3::Instruction::Binary {
            dst: array_size,
            lhs: l3::Value::Variable(array_size),
            op: l3::BinaryOp::Add,
            rhs: l3::Value::Number(1),
        },
        l3::Instruction::CallResult {
            dst: translate_symbol_id(dst),
            callee: l3::Callee::Allocate,
            args: vec![l3::Value::Variable(array_size), l3::Value::Number(1)],
        },
    ]);

    for (i, dim) in dims.iter().enumerate() {
        let dim_address = new_l3_variable_name(interner, prefix, suffix);
        l3_instructions.extend([
            l3::Instruction::Binary {
                dst: dim_address,
                lhs: l3::Value::Variable(translate_symbol_id(dst)),
                op: l3::BinaryOp::Add,
                rhs: l3::Value::Number((i as i64 + 1) * 8),
            },
            l3::Instruction::Store {
                dst: dim_address,
                src: translate_value(dim),
            },
        ]);
    }

    l3_instructions
}

fn translate_instruction(
    func: &Function,
    inst: &Instruction,
    interner: &mut Interner<String>,
    prefix: &str,
    suffix: &mut u32,
) -> Vec<l3::Instruction> {
    match inst {
        Instruction::Define { .. } => vec![],

        Instruction::Assign { dst, src } => vec![l3::Instruction::Assign {
            dst: translate_symbol_id(dst),
            src: translate_value(src),
        }],

        Instruction::Binary { dst, lhs, op, rhs } => {
            translate_binary_instruction(dst, lhs, op, rhs)
        }

        Instruction::Extract { dst, src, idxs } => {
            let (mut l3_instructions, offset) =
                translate_index_instruction(func, src, idxs, interner, prefix, suffix);
            l3_instructions.push(l3::Instruction::Load {
                dst: translate_symbol_id(dst),
                src: offset,
            });
            l3_instructions
        }

        Instruction::Insert { dst, idxs, src } => {
            let (mut l3_instructions, offset) =
                translate_index_instruction(func, dst, idxs, interner, prefix, suffix);
            l3_instructions.push(l3::Instruction::Store {
                dst: offset,
                src: translate_value(src),
            });
            l3_instructions
        }

        Instruction::ArrayLength { dst, src, dim } => {
            let offset = new_l3_variable_name(interner, prefix, suffix);
            vec![
                l3::Instruction::Binary {
                    dst: offset,
                    lhs: translate_value(dim),
                    op: l3::BinaryOp::Mul,
                    rhs: l3::Value::Number(8),
                },
                l3::Instruction::Binary {
                    dst: offset,
                    lhs: l3::Value::Variable(offset),
                    op: l3::BinaryOp::Add,
                    rhs: l3::Value::Number(8),
                },
                l3::Instruction::Binary {
                    dst: offset,
                    lhs: l3::Value::Variable(offset),
                    op: l3::BinaryOp::Add,
                    rhs: translate_value(src),
                },
                l3::Instruction::Load {
                    dst: translate_symbol_id(dst),
                    src: offset,
                },
            ]
        }

        Instruction::TupleLength { dst, src } => {
            let dst = translate_symbol_id(dst);
            let Value::Variable(src) = src else {
                unreachable!("tuple length src should be a variable");
            };
            vec![
                l3::Instruction::Load {
                    dst,
                    src: translate_symbol_id(src),
                },
                l3::Instruction::Binary {
                    dst,
                    lhs: l3::Value::Variable(dst),
                    op: l3::BinaryOp::Shl,
                    rhs: l3::Value::Number(1),
                },
                l3::Instruction::Binary {
                    dst,
                    lhs: l3::Value::Variable(dst),
                    op: l3::BinaryOp::Add,
                    rhs: l3::Value::Number(1),
                },
            ]
        }

        Instruction::Call { callee, args } => vec![l3::Instruction::Call {
            callee: translate_callee(callee),
            args: args.iter().map(|arg| translate_value(arg)).collect(),
        }],

        Instruction::CallResult { dst, callee, args } => vec![l3::Instruction::CallResult {
            dst: translate_symbol_id(dst),
            callee: translate_callee(callee),
            args: args.iter().map(|arg| translate_value(arg)).collect(),
        }],

        Instruction::NewArray { dst, dims } => {
            translate_new_array_instruction(dst, dims, interner, prefix, suffix)
        }

        Instruction::NewTuple { dst, len } => vec![l3::Instruction::CallResult {
            dst: translate_symbol_id(dst),
            callee: l3::Callee::Allocate,
            args: vec![translate_value(len), l3::Value::Number(1)],
        }],

        _ => unreachable!("instruction to translate should not be terminator or phi"),
    }
}

fn translate_function(func: &Function, interner: &mut Interner<String>) -> l3::Function {
    let dom_tree = DominatorTree::new(func);
    let loops = LoopInfo::new(func, &dom_tree);

    let prefix = longest_variable_name(func, interner).to_owned() + "_";
    let trace_order = compute_trace_order(func, &loops);
    let mut l3_instructions = Vec::new();
    let mut suffix = 0;

    for i in 0..trace_order.len() {
        let block_id = trace_order[i];

        l3_instructions.push(l3::Instruction::Label(translate_symbol_id(
            &func.basic_blocks[block_id.0].label,
        )));

        l3_instructions.extend(
            func.basic_blocks[block_id.0]
                .instructions
                .iter()
                .flat_map(|inst| translate_instruction(func, inst, interner, &prefix, &mut suffix)),
        );

        match &func.basic_blocks[block_id.0].terminator {
            Instruction::Branch(label) => {
                if i < trace_order.len() - 1 {
                    if label == &func.basic_blocks[trace_order[i + 1].0].label {
                        continue;
                    }
                }
                l3_instructions.push(l3::Instruction::Branch(translate_symbol_id(label)));
            }

            Instruction::BranchCondition {
                cond,
                true_label,
                false_label,
            } => {
                l3_instructions.push(l3::Instruction::BranchCondition {
                    cond: translate_value(cond),
                    label: translate_symbol_id(true_label),
                });

                if i < trace_order.len() - 1
                    && false_label == &func.basic_blocks[trace_order[i + 1].0].label
                {
                    continue;
                }

                l3_instructions.push(l3::Instruction::Branch(translate_symbol_id(false_label)));
            }

            Instruction::Return => l3_instructions.push(l3::Instruction::Return),

            Instruction::ReturnValue(val) => {
                l3_instructions.push(l3::Instruction::ReturnValue(translate_value(val)))
            }

            _ => unreachable!("terminator should be branch or return"),
        }
    }

    l3::Function::new(
        translate_symbol_id(&func.name),
        func.params
            .iter()
            .map(|param| translate_symbol_id(&param.var))
            .collect(),
        l3_instructions,
    )
}

fn longest_variable_name<'a>(func: &Function, interner: &'a Interner<String>) -> &'a str {
    func.params
        .iter()
        .map(|param| &param.var)
        .chain(
            func.basic_blocks
                .iter()
                .flat_map(|block| &block.instructions)
                .flat_map(|inst| {
                    inst.uses()
                        .filter_map(|use_| match use_ {
                            Value::Variable(var) => Some(var),
                            _ => None,
                        })
                        .chain(inst.defs())
                }),
        )
        .map(|var| interner.resolve(var.0))
        .fold(None, |longest, name| match longest {
            None => Some(name),
            Some(longest) if name.len() > longest.len() => Some(name),
            _ => longest,
        })
        .map_or("", |longest| longest)
}

fn new_l3_variable_name(
    interner: &mut Interner<String>,
    prefix: &str,
    suffix: &mut u32,
) -> l3::SymbolId {
    let id = l3::SymbolId(interner.intern(format!("{}{}", prefix, suffix)));
    *suffix += 1;
    id
}
