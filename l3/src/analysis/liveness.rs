use std::fmt;

use l3::*;
use utils::{BitVector, DisplayResolved, Interner};

use crate::analysis::dataflow::{Dataflow, Direction};

type InstId = usize;

#[derive(Debug)]
pub struct LivenessResult {
    pub interner: Interner<SymbolId>,
    pub out: Vec<Vec<BitVector>>,
}

impl LivenessResult {
    pub fn is_dead_at(&self, block_id: BlockId, inst_id: InstId, symbol_id: SymbolId) -> bool {
        !self.out[block_id.0][inst_id].test(self.interner[&symbol_id])
    }
}

impl DisplayResolved for LivenessResult {
    fn fmt_with(&self, f: &mut fmt::Formatter, interner: &Interner<String>) -> fmt::Result {
        writeln!(f, "(\n(out")?;

        for vec in &self.out {
            for bitvec in vec {
                let mut line: Vec<String> = bitvec
                    .iter()
                    .map(|val| format!("%{}", interner.resolve(self.interner.resolve(val).0)))
                    .collect();
                line.sort();
                writeln!(f, "({})", line.join(" "))?;
            }
        }

        writeln!(f, ")")
    }
}

#[derive(Debug)]
struct LivenessAnalysis {
    interner: Interner<SymbolId>,
    block_gen: Vec<BitVector>,
    block_kill: Vec<BitVector>,
}

impl LivenessAnalysis {
    pub fn new(func: &Function) -> Self {
        let interner = func
            .basic_blocks
            .iter()
            .flat_map(|block| &block.instructions)
            .flat_map(|inst| inst.uses().chain(inst.defs()))
            .chain(func.params.iter().copied())
            .fold(Interner::new(), |mut interner, val| {
                interner.intern(val);
                interner
            });

        let num_vars = interner.len();
        let num_blocks = func.basic_blocks.len();
        let mut block_gen = vec![BitVector::new(num_vars); num_blocks];
        let mut block_kill = vec![BitVector::new(num_vars); num_blocks];

        for (i, block) in func.basic_blocks.iter().enumerate() {
            for inst in &block.instructions {
                block_gen[i].set_from(inst.uses().filter_map(|use_| {
                    let j = interner[&use_];
                    (!block_kill[i].test(j)).then_some(j)
                }));
                block_kill[i].set_from(inst.defs().iter().map(|def| interner[&def]));
            }
        }

        LivenessAnalysis {
            interner,
            block_gen,
            block_kill,
        }
    }
}

impl Dataflow for LivenessAnalysis {
    const DIRECTION: Direction = Direction::Backward;

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

pub fn compute_liveness(func: &Function) -> LivenessResult {
    let liveness = LivenessAnalysis::new(func);
    let (block_out, _) = liveness.solve(func);

    let empty_dataflow_set = || -> Vec<Vec<BitVector>> {
        func.basic_blocks
            .iter()
            .map(|block| vec![BitVector::new(liveness.interner.len()); block.instructions.len()])
            .collect()
    };

    let mut inst_in = empty_dataflow_set();
    let mut inst_out = empty_dataflow_set();

    for (i, block) in func.basic_blocks.iter().enumerate() {
        for (j, inst) in block.instructions.iter().enumerate().rev() {
            inst_out[i][j] = if j == block.instructions.len() - 1 {
                block_out[i].clone()
            } else {
                inst_in[i][j + 1].clone()
            };

            inst_in[i][j] = inst_out[i][j].clone();
            inst_in[i][j].reset_from(inst.defs().iter().map(|def| liveness.interner[&def]));
            inst_in[i][j].set_from(inst.uses().map(|use_| liveness.interner[&use_]));
        }
    }

    LivenessResult {
        interner: liveness.interner,
        out: inst_out,
    }
}
