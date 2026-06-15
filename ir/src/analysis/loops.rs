use std::collections::HashMap;

use ir::*;
use utils::bitvector::BitVector;
use utils::interner::{DisplayResolved, Interner};

use crate::analysis::dominators::DominatorTree;

type LoopId = usize;

#[derive(Debug)]
pub struct LoopInfo {
    merged_loops: Vec<Loop>,
    roots: Vec<LoopId>,
    block_map: HashMap<BlockId, LoopId>,
}

impl LoopInfo {
    pub fn new(func: &Function, dominators: &DominatorTree) -> Self {
        let num_blocks = func.basic_blocks.len();

        let back_edges = (0..num_blocks).flat_map(|i| {
            let latch = BlockId(i);
            func.cfg.successors[i].iter().filter_map(move |&header| {
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
                    sub_loops: Vec::new(),
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

            let outer_loop = right.iter_mut().find(|other| {
                dominators.dominates(other.header, loop_header)
                    && other.basic_blocks.contains(&loop_header)
            });

            match outer_loop {
                Some(parent) => parent.sub_loops.push(i),
                None => roots.push(i),
            }
        }

        let mut stack: Vec<(LoopId, u32)> = roots.iter().map(|&root| (root, 1)).collect();
        while let Some((node, depth)) = stack.pop() {
            let loop_ = &mut merged_loops[node];
            loop_.depth = depth;
            for &sub_loop in &loop_.sub_loops {
                stack.push((sub_loop, depth + 1));
            }
        }

        Self {
            merged_loops,
            roots,
            block_map,
        }
    }

    pub fn loop_depth(&self, block_id: BlockId) -> u32 {
        match self.block_map.get(&block_id) {
            Some(&loop_id) => self.merged_loops[loop_id].depth,
            None => 0,
        }
    }

    pub fn display<'a>(&'a self, func: &'a Function) -> LoopInfoDisplay<'a> {
        LoopInfoDisplay { func, loops: self }
    }
}

#[derive(Debug)]
pub struct LoopInfoDisplay<'a> {
    loops: &'a LoopInfo,
    func: &'a Function,
}

impl DisplayResolved for LoopInfoDisplay<'_> {
    fn fmt_with(
        &self,
        f: &mut std::fmt::Formatter,
        interner: &Interner<String>,
    ) -> std::fmt::Result {
        let mut stack = self.loops.roots.clone();
        while let Some(loop_id) = stack.pop() {
            let loop_ = &self.loops.merged_loops[loop_id];

            writeln!(
                f,
                "{}{}",
                "  ".repeat(loop_.depth as usize - 1),
                loop_.display(self.func).resolved(interner)
            )?;

            for &sub_loop in &loop_.sub_loops {
                stack.push(sub_loop);
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Loop {
    header: BlockId,
    basic_blocks: Vec<BlockId>,
    depth: u32,
    sub_loops: Vec<LoopId>,
}

impl<'a> Loop {
    fn display(&'a self, func: &'a Function) -> LoopDisplay<'a> {
        LoopDisplay { func, loop_: self }
    }
}

#[derive(Debug)]
pub struct LoopDisplay<'a> {
    func: &'a Function,
    loop_: &'a Loop,
}

impl<'a> DisplayResolved for LoopDisplay<'a> {
    fn fmt_with(
        &self,
        f: &mut std::fmt::Formatter,
        interner: &Interner<String>,
    ) -> std::fmt::Result {
        let labels: Vec<String> = self
            .loop_
            .basic_blocks
            .iter()
            .map(|block_id| {
                self.func.basic_blocks[block_id.0]
                    .label
                    .resolved(interner)
                    .to_string()
            })
            .collect();
        write!(f, "{}", labels.join(", "))
    }
}
