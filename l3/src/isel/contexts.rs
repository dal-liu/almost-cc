use l3::*;

#[derive(Debug, Clone)]
pub struct Context {
    pub inst_ids: Vec<InstId>,
    pub terminator: Option<InstId>,
}

impl Context {
    fn new() -> Self {
        Self {
            inst_ids: Vec::new(),
            terminator: None,
        }
    }
}

pub fn create_contexts(func: &Function) -> Vec<Context> {
    let mut contexts = Vec::new();

    for (i, block) in func.basic_blocks.iter().enumerate() {
        contexts.push(Context::new());

        for (j, inst) in block.instructions.iter().enumerate() {
            let context = contexts.last_mut().unwrap();
            let inst_id = InstId(i, j);

            match inst {
                Instruction::Return
                | Instruction::ReturnValue(_)
                | Instruction::Branch(_)
                | Instruction::BranchCondition { .. } => {
                    context.inst_ids.push(inst_id);
                    contexts.push(Context::new());
                }

                Instruction::Label(_)
                | Instruction::Call { .. }
                | Instruction::CallResult { .. } => {
                    context.terminator = Some(inst_id);
                    contexts.push(Context::new());
                }

                _ => context.inst_ids.push(inst_id),
            }
        }
    }

    contexts.retain(|ctx| !ctx.inst_ids.is_empty() || ctx.terminator.is_some());
    contexts
}
