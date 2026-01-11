use l3::*;

type InstId = usize;

#[derive(Debug, Clone)]
pub struct Context {
    pub block_id: BlockId,
    pub inst_ids: Vec<InstId>,
    pub terminator: Option<InstId>,
}

impl Context {
    fn new(block_id: BlockId) -> Self {
        Self {
            block_id,
            inst_ids: Vec::new(),
            terminator: None,
        }
    }
}

pub fn create_contexts(func: &Function) -> Vec<Context> {
    let mut contexts = Vec::new();

    for (i, block) in func.basic_blocks.iter().enumerate() {
        contexts.push(Context::new(BlockId(i)));

        for (j, inst) in block.instructions.iter().enumerate() {
            let context = contexts.last_mut().unwrap();

            match inst {
                Instruction::Return
                | Instruction::ReturnValue(_)
                | Instruction::Branch(_)
                | Instruction::BranchCondition { .. } => {
                    context.inst_ids.push(j);
                    contexts.push(Context::new(BlockId(i)));
                }

                Instruction::Label(_)
                | Instruction::Call { .. }
                | Instruction::CallResult { .. } => {
                    context.terminator = Some(j);
                    contexts.push(Context::new(BlockId(i)));
                }

                _ => context.inst_ids.push(j),
            }
        }
    }

    contexts.retain(|ctx| !ctx.inst_ids.is_empty() || ctx.terminator.is_some());
    contexts
}
