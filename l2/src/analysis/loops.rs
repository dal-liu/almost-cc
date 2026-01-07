use std::collections::HashMap;

use l2::*;
use utils::BitVector;

use crate::analysis::dominators::DominatorTree;

type LoopId = usize;

#[derive(Debug)]
pub struct LoopForest {
    merged_loops: Vec<Loop>,
    block_map: HashMap<BlockId, LoopId>,
}

impl LoopForest {
    pub fn new(func: &Function, dominators: &DominatorTree) -> Self {
        let num_blocks = func.basic_blocks.len();

        let back_edges = func.basic_blocks.iter().flat_map(|block| {
            let latch = block.id;
            func.cfg.successors[latch.0]
                .iter()
                .filter_map(move |&header| {
                    dominators
                        .dominates(header, latch)
                        .then_some((latch, header))
                })
        });

        let natural_loops = back_edges.map(|(latch, header)| {
            let mut stack = vec![latch];
            let mut loop_blocks = BitVector::new(num_blocks);
            loop_blocks.set(header.0);

            while let Some(id) = stack.pop() {
                let i = id.0;
                if !loop_blocks.test(i) {
                    loop_blocks.set(i);
                    stack.extend(func.cfg.predecessors[i].iter().copied());
                }
            }

            (header, loop_blocks)
        });

        let mut merged_loops: Vec<Loop> = natural_loops
            .fold(
                vec![BitVector::new(num_blocks); num_blocks],
                |mut merged_loops, (header, blocks)| {
                    merged_loops[header.0].union(&blocks);
                    merged_loops
                },
            )
            .into_iter()
            .enumerate()
            .filter_map(|(i, blocks)| {
                blocks.any().then_some(Loop {
                    header: BlockId(i),
                    basic_blocks: blocks.iter().map(BlockId).collect(),
                    depth: 0,
                    children: Vec::new(),
                })
            })
            .collect();
        merged_loops.sort_by_key(|loop_| loop_.basic_blocks.len());

        let mut roots = Vec::new();
        let mut block_map = HashMap::new();

        for i in 0..merged_loops.len() {
            for &id in merged_loops.iter().flat_map(|loop_| &loop_.basic_blocks) {
                block_map.entry(id).or_insert(i);
            }

            let (left, right) = merged_loops.split_at_mut(i + 1);
            let loop_header = left[i].header;

            let parent = right.iter_mut().find(|other| {
                dominators.dominates(other.header, loop_header)
                    && other.basic_blocks.contains(&loop_header)
            });

            match parent {
                Some(parent) => parent.children.push(i),
                None => roots.push(i),
            }
        }

        let mut stack: Vec<(LoopId, u32)> = roots.iter().map(|&root| (root, 1)).collect();
        while let Some((node, depth)) = stack.pop() {
            let loop_ = &mut merged_loops[node];
            loop_.depth = depth;
            for &child in &loop_.children {
                stack.push((child, depth + 1));
            }
        }

        Self {
            merged_loops,
            block_map,
        }
    }

    pub fn loop_depth(&self, block: BlockId) -> u32 {
        match self.block_map.get(&block) {
            Some(&loop_id) => self.merged_loops[loop_id].depth,
            None => 0,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Loop {
    header: BlockId,
    basic_blocks: Vec<BlockId>,
    depth: u32,
    children: Vec<LoopId>,
}
