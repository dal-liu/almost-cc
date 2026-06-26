use std::collections::HashMap;
use std::fmt;

use l3::*;
use utils::bitvector::BitVector;
use utils::interner::{DisplayResolved, Interner};

use crate::analysis::dataflow::{Dataflow, Direction};

#[derive(Debug)]
pub struct ReachingDefResult {
    pub interner: Interner<InstId>,
    pub in_: Vec<Vec<BitVector>>,
}

impl ReachingDefResult {
    #[allow(dead_code)]
    pub fn display<'a>(&'a self, func: &'a Function) -> ReachingDefResultDisplay<'a> {
        ReachingDefResultDisplay {
            func,
            reaching_def: self,
        }
    }
}

#[derive(Debug)]
pub struct ReachingDefResultDisplay<'a> {
    func: &'a Function,
    reaching_def: &'a ReachingDefResult,
}

impl DisplayResolved for ReachingDefResultDisplay<'_> {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        for vec in &self.reaching_def.in_ {
            for bitvec in vec {
                let mut lines: Vec<String> = bitvec
                    .iter()
                    .map(|idx| {
                        let inst_id = *self.reaching_def.interner.resolve(idx);
                        format!(
                            "{}\n",
                            self.func
                                .instruction(inst_id)
                                .expect("inst id should be valid")
                                .resolved(interner)
                        )
                    })
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
    interner: Interner<InstId>,
    def_table: HashMap<SymbolId, BitVector>,
    block_gen: Vec<BitVector>,
    block_kill: Vec<BitVector>,
}

impl ReachingDefAnalysis {
    pub fn new(func: &Function) -> Self {
        let interner: Interner<InstId> = func
            .basic_blocks
            .iter()
            .enumerate()
            .flat_map(|(i, block)| (0..block.instructions.len()).map(move |j| InstId(i, j)))
            .collect();

        let num_insts = interner.len();
        let mut def_table = HashMap::new();

        for (i, block) in func.basic_blocks.iter().enumerate() {
            for (j, inst) in block.instructions.iter().enumerate() {
                if let Some(def) = inst.defs() {
                    def_table
                        .entry(def)
                        .or_insert(BitVector::new(num_insts))
                        .set(interner.get(&InstId(i, j)));
                }
            }
        }

        let num_blocks = func.basic_blocks.len();
        let mut block_gen = vec![BitVector::new(num_insts); num_blocks];
        let mut block_kill = vec![BitVector::new(num_insts); num_blocks];

        for (i, block) in func.basic_blocks.iter().enumerate() {
            for (j, inst) in block.instructions.iter().enumerate().rev() {
                let Some(def) = inst.defs() else {
                    continue;
                };

                let k = interner.get(&InstId(i, j));
                if !block_kill[i].test(k) {
                    block_gen[i].set(k);
                }

                block_kill[i].set_from(def_table[&def].iter().filter(|&id| k != id));
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
        current.union(other);
    }

    fn transfer(&self, input: &BitVector, block_id: BlockId) -> BitVector {
        let mut output = input.clone();
        output.difference(&self.block_kill[block_id.0]);
        output.union(&self.block_gen[block_id.0]);
        output
    }
}

pub fn compute_reaching_def(func: &mut Function) -> ReachingDefResult {
    let has_params = !func.params.is_empty();

    if has_params {
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
        func.cfg.predecessors[0].push(BlockId(func.basic_blocks.len()));
        func.cfg.predecessors.push(vec![]);
        func.cfg.successors.push(vec![BlockId(0)]);
        func.basic_blocks.push(dummy_block);
    }

    let reaching_def = ReachingDefAnalysis::new(func);
    let (block_in, _) = reaching_def.solve(func);

    let empty_dataflow_set = || -> Vec<Vec<BitVector>> {
        func.basic_blocks
            .iter()
            .map(|block| {
                vec![BitVector::new(reaching_def.interner.len()); block.instructions.len()]
            })
            .collect()
    };

    let mut inst_in = empty_dataflow_set();
    let mut inst_out = empty_dataflow_set();

    for (i, block) in func.basic_blocks.iter().enumerate() {
        for (j, inst) in block.instructions.iter().enumerate() {
            inst_in[i][j] = if j == 0 {
                block_in[i].clone()
            } else {
                inst_out[i][j - 1].clone()
            };

            inst_out[i][j] = inst_in[i][j].clone();
            if let Some(def) = inst.defs() {
                let k = reaching_def.interner.get(&InstId(i, j));
                inst_out[i][j]
                    .reset_from(reaching_def.def_table[&def].iter().filter(|&idx| k != idx));
                inst_out[i][j].set(k);
            }
        }
    }

    if has_params {
        inst_in.pop();
        func.basic_blocks.pop();
        func.cfg.successors.pop();
        func.cfg.predecessors.pop();
        func.cfg.predecessors[0].pop();
    }

    ReachingDefResult {
        interner: reaching_def.interner,
        in_: inst_in,
    }
}
