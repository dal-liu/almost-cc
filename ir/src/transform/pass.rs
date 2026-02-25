use ir::*;

pub trait FunctionPass {
    fn run(func: &mut Function, ctx: &mut PassContext) -> bool;
}

#[derive(Debug)]
pub struct PassContext {}
