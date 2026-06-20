use ir::*;
use utils::bitvector::BitVector;

use crate::analysis::loops::LoopInfo;

pub fn compute_trace_order(func: &Function, loops: &LoopInfo) -> Vec<BlockId> {
    let num_blocks = func.basic_blocks.len();
    let mut unmarked = BitVector::with_value(num_blocks, true);
    let mut order = Vec::new();

    while let Some(i) = unmarked
        .iter()
        .min_by_key(|&i| func.cfg.predecessors[i].len())
    {
        let mut id = BlockId(i);

        loop {
            unmarked.reset(id.0);
            order.push(id);

            let Some(&succ) = func.cfg.successors[id.0]
                .iter()
                .filter(|&&succ| unmarked.test(succ.0))
                .max_by_key(|&&succ| 10u32.pow(loops.loop_depth(succ)))
            else {
                break;
            };

            id = succ;
        }
    }

    order
}
