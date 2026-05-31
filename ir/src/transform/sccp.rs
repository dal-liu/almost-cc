// use std::collections::HashMap;
//
// use ir::*;
// use utils::Worklist;
//
// use crate::analysis::def_use::DefUseChain;
// use crate::analysis::use_def::{Operand, UseDefChain};
// use crate::xform::pass::{Pass, PassContext};
//
// enum LatticeCell {
//     Unknown,
//     Constant(Value),
//     Overdefined,
// }
//
// pub struct SCCP {
//     ssa_worklist: Worklist<SymbolId>,
//     flow_worklist: Worklist<(BlockId, BlockId)>,
//     executable_flag: HashMap<BlockId, bool>,
// }
//
// impl SCCP {
//     pub fn new() -> Self {
//         Self {
//             ssa_worklist: Worklist::new(),
//             flow_worklist: Worklist::new(),
//             executable_flag: HashMap::new(),
//         }
//     }
// }
//
// impl Pass for SCCP {
//     fn name(&self) -> &str {
//         "sccp"
//     }
//
//     fn run(&mut self, func: &mut Function, ctx: &mut PassContext) -> bool {
//         let def_use = DefUseChain::new(func);
//
//         // let mut ssa_worklist = Worklist::new();
//         // let mut flow_worklist = Worklist::new();
//         // let mut executable_flag = HashMap::new();
//
//         self.flow_worklist.extend(
//             func.cfg.successors[0]
//                 .iter()
//                 .map(|&succ| (BlockId(0), succ)),
//         );
//
//         false
//     }
// }
