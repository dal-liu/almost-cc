use l3::*;

type InstId = usize;

#[derive(Debug)]
pub struct Context {
    pub block_id: BlockId,
    pub inst_ids: Vec<InstId>,
}

pub fn create_contexts(func: &Function) -> Vec<Context> {
    let mut contexts = Vec::new();

    for block in &func.basic_blocks {
        contexts.push(Context {
            block_id: block.id,
            inst_ids: Vec::new(),
        });

        for (i, inst) in block.instructions.iter().enumerate() {
            let context = contexts.last_mut().unwrap();

            match inst {
                Instruction::Return
                | Instruction::ReturnValue(_)
                | Instruction::Branch(_)
                | Instruction::BranchCond { .. } => {
                    context.inst_ids.push(i);
                    contexts.push(Context {
                        block_id: block.id,
                        inst_ids: Vec::new(),
                    });
                }

                Instruction::Label(_)
                | Instruction::Call { .. }
                | Instruction::CallResult { .. } => contexts.push(Context {
                    block_id: block.id,
                    inst_ids: Vec::new(),
                }),

                _ => context.inst_ids.push(i),
            }
        }
    }

    contexts.retain(|ctx| !ctx.inst_ids.is_empty());
    contexts
}
