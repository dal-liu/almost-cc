use std::mem;

use ir::*;
use utils::interner::Interner;

pub fn linearize_data_types(prog: &mut Program) {
    for func in &mut prog.functions {
        let prefix = longest_variable_name(func, &prog.interner).to_owned() + "_";
        let mut defines = Vec::new();
        let mut suffix = 0;

        for block in &mut func.basic_blocks {
            let num_insts = block.instructions.len();

            for inst in mem::replace(&mut block.instructions, Vec::with_capacity(num_insts)) {
                match inst {
                    Instruction::Extract { dst, src, idxs } => {
                        block.instructions.extend(linearize_extract_instruction(
                            &func.symtab,
                            dst,
                            &src,
                            &idxs,
                            &mut prog.interner,
                            &prefix,
                            &mut suffix,
                            &mut defines,
                        ))
                    }

                    Instruction::Insert { dst, idxs, src } => {
                        block.instructions.extend(linearize_insert_instruction(
                            &func.symtab,
                            &dst,
                            &idxs,
                            &src,
                            &mut prog.interner,
                            &prefix,
                            &mut suffix,
                            &mut defines,
                        ))
                    }

                    Instruction::ArrayLength { dst, src, dim } => {
                        block
                            .instructions
                            .extend(linearize_array_length_instruction(
                                dst,
                                &src,
                                &dim,
                                &mut prog.interner,
                                &prefix,
                                &mut suffix,
                                &mut defines,
                            ));
                    }

                    Instruction::TupleLength { dst, src } => block
                        .instructions
                        .extend(linearize_tuple_length_instruction(dst, &src)),

                    Instruction::NewArray { dst, dims } => {
                        block.instructions.extend(linearize_new_array_instruction(
                            dst,
                            &dims,
                            &mut prog.interner,
                            &prefix,
                            &mut suffix,
                            &mut defines,
                        ))
                    }

                    Instruction::NewTuple { dst, len } => block
                        .instructions
                        .extend(linearize_new_tuple_instruction(dst, &len)),

                    _ => block.instructions.push(inst),
                }
            }
        }

        func.basic_blocks[0].instructions.extend(
            defines
                .into_iter()
                .map(|(ty, var)| Instruction::Define { ty, var }),
        );

        func.symtab = SymbolTable::new(&func.params, &func.basic_blocks);
    }
}

fn linearize_extract_instruction(
    symtab: &SymbolTable,
    dst: SymbolId,
    src: &Value,
    idxs: &[Value],
    interner: &mut Interner<String>,
    prefix: &str,
    suffix: &mut u32,
    defines: &mut Vec<(Type, SymbolId)>,
) -> Vec<Instruction> {
    let (mut instructions, offset) =
        flatten_index(symtab, src, idxs, interner, prefix, suffix, defines);
    instructions.push(Instruction::Load {
        dst,
        src: Value::Variable(offset),
    });
    instructions
}

fn linearize_insert_instruction(
    symtab: &SymbolTable,
    dst: &Value,
    idxs: &[Value],
    src: &Value,
    interner: &mut Interner<String>,
    prefix: &str,
    suffix: &mut u32,
    defines: &mut Vec<(Type, SymbolId)>,
) -> Vec<Instruction> {
    let (mut instructions, offset) =
        flatten_index(symtab, dst, idxs, interner, prefix, suffix, defines);
    instructions.push(Instruction::Store {
        dst: Value::Variable(offset),
        src: src.clone(),
    });
    instructions
}

fn flatten_index(
    symtab: &SymbolTable,
    val: &Value,
    idxs: &[Value],
    interner: &mut Interner<String>,
    prefix: &str,
    suffix: &mut u32,
    defines: &mut Vec<(Type, SymbolId)>,
) -> (Vec<Instruction>, SymbolId) {
    let Value::Variable(var) = val else {
        unreachable!("container value should be a variable")
    };
    let offset = new_variable_name(Type::Int64, interner, prefix, suffix, defines);
    let mut instructions = Vec::new();

    match symtab.type_map.get(var) {
        Some(Type::Array(ndims)) if idxs.len() == *ndims => {
            let ndims = *ndims as i64;
            let num_idxs = idxs.len();

            let dims: Vec<SymbolId> = (1..ndims)
                .map(|i| {
                    let address = new_variable_name(Type::Int64, interner, prefix, suffix, defines);
                    let dim = new_variable_name(Type::Int64, interner, prefix, suffix, defines);
                    instructions.extend([
                        Instruction::Binary {
                            dst: address,
                            lhs: val.clone(),
                            op: BinaryOp::Add,
                            rhs: Value::Number((i + 1) * 8),
                        },
                        Instruction::Load {
                            dst: dim,
                            src: Value::Variable(address),
                        },
                        Instruction::Binary {
                            dst: dim,
                            lhs: Value::Variable(dim),
                            op: BinaryOp::Shr,
                            rhs: Value::Number(1),
                        },
                    ]);
                    dim
                })
                .collect();

            instructions.push(Instruction::Assign {
                dst: offset,
                src: idxs[num_idxs - 1].clone(),
            });

            for (i, idx) in idxs.iter().take(num_idxs - 1).enumerate() {
                let linearized_idx =
                    new_variable_name(Type::Int64, interner, prefix, suffix, defines);

                instructions.push(Instruction::Assign {
                    dst: linearized_idx,
                    src: idx.clone(),
                });

                for &dim in dims.iter().take(num_idxs - 1).skip(i) {
                    instructions.push(Instruction::Binary {
                        dst: linearized_idx,
                        lhs: Value::Variable(linearized_idx),
                        op: BinaryOp::Mul,
                        rhs: Value::Variable(dim),
                    });
                }

                instructions.push(Instruction::Binary {
                    dst: offset,
                    lhs: Value::Variable(offset),
                    op: BinaryOp::Add,
                    rhs: Value::Variable(linearized_idx),
                });
            }

            instructions.extend([
                Instruction::Binary {
                    dst: offset,
                    lhs: Value::Variable(offset),
                    op: BinaryOp::Mul,
                    rhs: Value::Number(8),
                },
                Instruction::Binary {
                    dst: offset,
                    lhs: Value::Variable(offset),
                    op: BinaryOp::Add,
                    rhs: Value::Number(8 + ndims * 8),
                },
            ]);
        }

        Some(Type::Tuple) if idxs.len() == 1 => {
            instructions.extend([
                Instruction::Binary {
                    dst: offset,
                    lhs: idxs[0].clone(),
                    op: BinaryOp::Mul,
                    rhs: Value::Number(8),
                },
                Instruction::Binary {
                    dst: offset,
                    lhs: Value::Variable(offset),
                    op: BinaryOp::Add,
                    rhs: Value::Number(8),
                },
            ]);
        }

        _ => panic!("container type should be array or tuple of correct ndims/idxs"),
    }

    instructions.push(Instruction::Binary {
        dst: offset,
        lhs: Value::Variable(offset),
        op: BinaryOp::Add,
        rhs: val.clone(),
    });

    (instructions, offset)
}

fn linearize_array_length_instruction(
    dst: SymbolId,
    src: &Value,
    dim: &Value,
    interner: &mut Interner<String>,
    prefix: &str,
    suffix: &mut u32,
    defines: &mut Vec<(Type, SymbolId)>,
) -> Vec<Instruction> {
    let offset = new_variable_name(Type::Int64, interner, prefix, suffix, defines);
    vec![
        Instruction::Binary {
            dst: offset,
            lhs: dim.clone(),
            op: BinaryOp::Mul,
            rhs: Value::Number(8),
        },
        Instruction::Binary {
            dst: offset,
            lhs: Value::Variable(offset),
            op: BinaryOp::Add,
            rhs: Value::Number(8),
        },
        Instruction::Binary {
            dst: offset,
            lhs: Value::Variable(offset),
            op: BinaryOp::Add,
            rhs: src.clone(),
        },
        Instruction::Load {
            dst,
            src: Value::Variable(offset),
        },
    ]
}

fn linearize_tuple_length_instruction(dst: SymbolId, src: &Value) -> Vec<Instruction> {
    vec![
        Instruction::Load {
            dst,
            src: src.clone(),
        },
        Instruction::Binary {
            dst,
            lhs: Value::Variable(dst),
            op: BinaryOp::Shl,
            rhs: Value::Number(1),
        },
        Instruction::Binary {
            dst,
            lhs: Value::Variable(dst),
            op: BinaryOp::Add,
            rhs: Value::Number(1),
        },
    ]
}

fn linearize_new_array_instruction(
    dst: SymbolId,
    dims: &[Value],
    interner: &mut Interner<String>,
    prefix: &str,
    suffix: &mut u32,
    defines: &mut Vec<(Type, SymbolId)>,
) -> Vec<Instruction> {
    let mut instructions = Vec::with_capacity(dims.len());

    let mut decoded_dims = Vec::with_capacity(dims.len());
    for dim in dims {
        let dst = new_variable_name(Type::Int64, interner, prefix, suffix, defines);
        instructions.push(Instruction::Binary {
            dst,
            lhs: dim.clone(),
            op: BinaryOp::Shr,
            rhs: Value::Number(1),
        });
        decoded_dims.push(dst);
    }

    let array_size = new_variable_name(Type::Int64, interner, prefix, suffix, defines);
    instructions.push(Instruction::Assign {
        dst: array_size,
        src: Value::Variable(decoded_dims[0]),
    });

    for &dim in decoded_dims.iter().skip(1) {
        instructions.push(Instruction::Binary {
            dst: array_size,
            lhs: Value::Variable(array_size),
            op: BinaryOp::Mul,
            rhs: Value::Variable(dim),
        });
    }

    instructions.extend([
        Instruction::Binary {
            dst: array_size,
            lhs: Value::Variable(array_size),
            op: BinaryOp::Add,
            rhs: Value::Number(dims.len() as i64),
        },
        Instruction::Binary {
            dst: array_size,
            lhs: Value::Variable(array_size),
            op: BinaryOp::Shl,
            rhs: Value::Number(1),
        },
        Instruction::Binary {
            dst: array_size,
            lhs: Value::Variable(array_size),
            op: BinaryOp::Add,
            rhs: Value::Number(1),
        },
        Instruction::CallResult {
            dst,
            callee: Callee::Allocate,
            args: vec![Value::Variable(array_size), Value::Number(1)],
        },
    ]);

    for (i, dim) in dims.iter().enumerate() {
        let dim_address = new_variable_name(Type::Int64, interner, prefix, suffix, defines);
        instructions.extend([
            Instruction::Binary {
                dst: dim_address,
                lhs: Value::Variable(dst),
                op: BinaryOp::Add,
                rhs: Value::Number((i as i64 + 1) * 8),
            },
            Instruction::Store {
                dst: Value::Variable(dim_address),
                src: dim.clone(),
            },
        ]);
    }

    instructions
}

fn linearize_new_tuple_instruction(dst: SymbolId, len: &Value) -> Vec<Instruction> {
    vec![Instruction::CallResult {
        dst,
        callee: Callee::Allocate,
        args: vec![len.clone(), Value::Number(1)],
    }]
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

fn new_variable_name(
    ty: Type,
    interner: &mut Interner<String>,
    prefix: &str,
    suffix: &mut u32,
    defines: &mut Vec<(Type, SymbolId)>,
) -> SymbolId {
    let id = SymbolId(interner.intern(format!("{}{}", prefix, suffix)));
    *suffix += 1;
    defines.push((ty, id));
    id
}
