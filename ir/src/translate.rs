use ir::*;

use crate::analysis::dominators::DominatorTree;
use crate::analysis::loops::LoopInfo;
use crate::tracing::compute_trace_order;

pub fn translate_program(prog: &mut Program) -> l3::Program {
    l3::Program {
        functions: prog.functions.iter().map(translate_function).collect(),
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
        Callee::Allocate => l3::Callee::Allocate,
    }
}

fn translate_binary_instruction(
    dst: &SymbolId,
    lhs: &Value,
    op: &BinaryOp,
    rhs: &Value,
) -> l3::Instruction {
    let dst = translate_symbol_id(dst);
    let lhs = translate_value(lhs);
    let rhs = translate_value(rhs);

    match op {
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
    }
}

fn translate_instruction(inst: &Instruction) -> Option<l3::Instruction> {
    match inst {
        Instruction::Define { .. } => None,

        Instruction::Assign { dst, src } => Some(l3::Instruction::Assign {
            dst: translate_symbol_id(dst),
            src: translate_value(src),
        }),

        Instruction::Binary { dst, lhs, op, rhs } => {
            Some(translate_binary_instruction(dst, lhs, op, rhs))
        }

        Instruction::Call { callee, args } => Some(l3::Instruction::Call {
            callee: translate_callee(callee),
            args: args.iter().map(translate_value).collect(),
        }),

        Instruction::CallResult { dst, callee, args } => Some(l3::Instruction::CallResult {
            dst: translate_symbol_id(dst),
            callee: translate_callee(callee),
            args: args.iter().map(translate_value).collect(),
        }),

        Instruction::Load { dst, src } => {
            let Value::Variable(src) = src else {
                unreachable!("asdf")
            };
            Some(l3::Instruction::Load {
                dst: translate_symbol_id(dst),
                src: translate_symbol_id(src),
            })
        }

        Instruction::Store { dst, src } => {
            let Value::Variable(dst) = dst else {
                unreachable!("asdf")
            };
            Some(l3::Instruction::Store {
                dst: translate_symbol_id(dst),
                src: translate_value(src),
            })
        }

        _ => unreachable!(
            "instruction to translate should not be terminator, phi, or un-linearized instruction"
        ),
    }
}

fn translate_function(func: &Function) -> l3::Function {
    let dom_tree = DominatorTree::new(func);
    let loops = LoopInfo::new(func, &dom_tree);
    let trace_order = compute_trace_order(func, &loops);
    let mut l3_instructions = Vec::new();

    for i in 0..trace_order.len() {
        let block_id = trace_order[i];

        l3_instructions.push(l3::Instruction::Label(translate_symbol_id(
            &func.basic_blocks[block_id.0].label,
        )));

        l3_instructions.extend(
            func.basic_blocks[block_id.0]
                .instructions
                .iter()
                .filter_map(translate_instruction),
        );

        match &func.basic_blocks[block_id.0].terminator {
            Instruction::Branch(label) => {
                if i < trace_order.len() - 1
                    && label == &func.basic_blocks[trace_order[i + 1].0].label
                {
                    continue;
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
