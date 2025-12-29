use l3::*;

type InstId = usize;

#[derive(Debug, Clone)]
pub struct Context<'a> {
    pub block_id: BlockId,
    pub inst_ids: Vec<InstId>,
    pub terminator: Option<&'a Instruction>,
}

impl Context<'_> {
    fn new(block_id: BlockId) -> Self {
        Self {
            block_id,
            inst_ids: Vec::new(),
            terminator: None,
        }
    }
}

pub fn create_contexts(func: &Function) -> Vec<Context<'_>> {
    let mut contexts = Vec::new();

    for block in &func.basic_blocks {
        contexts.push(Context::new(block.id));

        for (i, inst) in block.instructions.iter().enumerate() {
            let context = contexts.last_mut().unwrap();

            match inst {
                Instruction::Return
                | Instruction::ReturnValue(_)
                | Instruction::Branch(_)
                | Instruction::BranchCond { .. } => {
                    context.inst_ids.push(i);
                    contexts.push(Context::new(block.id));
                }

                Instruction::Label(_)
                | Instruction::Call { .. }
                | Instruction::CallResult { .. } => {
                    context.terminator = Some(inst);
                    contexts.push(Context::new(block.id));
                }

                _ => context.inst_ids.push(i),
            }
        }
    }

    contexts.retain(|ctx| !ctx.inst_ids.is_empty() || ctx.terminator.is_some());
    contexts
}
