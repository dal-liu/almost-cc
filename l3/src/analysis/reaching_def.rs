use std::collections::HashMap;
use std::fmt;

use l3::*;
use utils::{BitVector, DisplayResolved, Interner};

use crate::analysis::dataflow::{Dataflow, Direction};

type InstId = usize;

#[derive(Debug)]
pub struct ReachingDefResult {
    pub interner: Interner<Instruction>,
    pub in_: Vec<Vec<BitVector>>,
}

impl DisplayResolved for ReachingDefResult {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        for vec in &self.in_ {
            for bitvec in vec {
                let mut lines: Vec<String> = bitvec
                    .iter()
                    .map(|k| format!("{}\n", self.interner.resolve(k).resolved(interner)))
                    .collect();
                lines.sort();
                writeln!(f, "IN\n{{\n{}}}", lines.join(""))?;
            }
        }
        Ok(())
    }
}

#[derive(Debug)]
struct ReachingDefAnalysis {
    interner: Interner<Instruction>,
    def_table: HashMap<SymbolId, Vec<InstId>>,
    block_gen: Vec<BitVector>,
    block_kill: Vec<BitVector>,
}

impl ReachingDefAnalysis {
    pub fn new(func: &Function) -> Self {
        let (interner, def_table) = func
            .basic_blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .filter(|inst| inst.defs().is_some())
            .fold(
                (Interner::new(), HashMap::new()),
                |(mut interner, mut def_table), inst| {
                    let index = interner.intern(inst.clone());
                    if let Some(def) = inst.defs() {
                        def_table.entry(def).or_insert(Vec::new()).push(index);
                    }
                    (interner, def_table)
                },
            );

        let num_insts = interner.len();
        let num_blocks = func.basic_blocks.len();
        let mut block_gen = vec![BitVector::new(num_insts); num_blocks];
        let mut block_kill = vec![BitVector::new(num_insts); num_blocks];

        for (i, block) in func.basic_blocks.iter().enumerate() {
            for inst in block.instructions.iter().rev() {
                let Some(def) = inst.defs() else {
                    continue;
                };

                let j = interner[inst];
                if !block_kill[i].test(j) {
                    block_gen[i].set(j);
                }

                block_kill[i].set_from(
                    def_table[&def]
                        .iter()
                        .filter_map(|&id| (j != id).then_some(id)),
                );
            }
        }

        ReachingDefAnalysis {
            interner,
            def_table,
            block_gen,
            block_kill,
        }
    }
}

impl Dataflow for ReachingDefAnalysis {
    const DIRECTION: Direction = Direction::Forward;

    fn boundary(&self) -> BitVector {
        BitVector::new(self.interner.len())
    }

    fn meet(&self, current: &mut BitVector, other: &BitVector) {
        current.union(&other);
    }

    fn transfer(&self, input: &BitVector, id: BlockId) -> BitVector {
        let mut output = input.clone();
        output.difference(&self.block_kill[id.0]);
        output.union(&self.block_gen[id.0]);
        output
    }
}

pub fn compute_reaching_def(func: &Function) -> ReachingDefResult {
    let mut func_clone = func.clone();

    if !func.params.is_empty() {
        let dummy_block = BasicBlock {
            instructions: func
                .params
                .iter()
                .map(|&param| Instruction::Assign {
                    dst: param,
                    src: Value::Variable(param),
                })
                .collect(),
        };
        func_clone.cfg.predecessors[0].push(BlockId(func.basic_blocks.len()));
        func_clone.cfg.predecessors.push(vec![]);
        func_clone.cfg.successors.push(vec![BlockId(0)]);
        func_clone.basic_blocks.push(dummy_block);
    }

    let reaching_def = ReachingDefAnalysis::new(&func_clone);
    let (block_in, _) = reaching_def.solve(&func_clone);

    let empty_dataflow_set = || -> Vec<Vec<BitVector>> {
        func_clone
            .basic_blocks
            .iter()
            .map(|block| {
                vec![BitVector::new(reaching_def.interner.len()); block.instructions.len()]
            })
            .collect()
    };

    let mut inst_in = empty_dataflow_set();
    let mut inst_out = empty_dataflow_set();

    for (i, block) in func_clone.basic_blocks.iter().enumerate() {
        for (j, inst) in block.instructions.iter().enumerate() {
            inst_in[i][j] = if j == 0 {
                block_in[i].clone()
            } else {
                inst_out[i][j - 1].clone()
            };

            inst_out[i][j] = inst_in[i][j].clone();
            if let Some(def) = inst.defs() {
                let k = reaching_def.interner[inst];
                inst_out[i][j].reset_from(
                    reaching_def.def_table[&def]
                        .iter()
                        .filter_map(|&id| (k != id).then_some(id)),
                );
                inst_out[i][j].set(k);
            }
        }
    }

    ReachingDefResult {
        interner: reaching_def.interner,
        in_: inst_in,
    }
}
