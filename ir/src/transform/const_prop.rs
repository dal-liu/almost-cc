use std::iter;

use ir::*;
use utils::{Interner, Worklist};

use crate::analysis::def_use::DefUseChain;
use crate::analysis::use_def::{Operand, UseDefChain};
use crate::transform::pass::{FunctionPass, PassContext};

enum LatticeCell {
    Unknown,
    Constant(Value),
    Overdefined,
}

pub struct SCCPPass;

impl FunctionPass for SCCPPass {
    fn run(func: &mut Function, ctx: &mut PassContext) -> bool {
        let def_use = DefUseChain::new(func);

        let mut ssa_worklist = Worklist::new();
        let mut flow_worklist = Worklist::new();
        let mut executable_flag = HashMap::new();

        flow_worklist.extend(
            func.cfg.successors[0]
                .iter()
                .map(|&succ| (BlockId(0), succ)),
        );

        false
    }
}
