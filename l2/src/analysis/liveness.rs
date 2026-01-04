use l2::*;
use utils::{BitVector, DisplayResolved, Interner, Worklist};

#[derive(Debug)]
pub struct LivenessResult {
    pub block_out: Vec<BitVector>,
    pub inst_in: Vec<Vec<BitVector>>,
    pub inst_out: Vec<Vec<BitVector>>,
    pub interner: Interner<Value>,
}

impl DisplayResolved for LivenessResult {
    fn fmt_with(
        &self,
        f: &mut std::fmt::Formatter,
        interner: &Interner<String>,
    ) -> std::fmt::Result {
        writeln!(f, "(\n(in")?;

        for vec in &self.inst_in {
            for bitvec in vec {
                let mut line: Vec<String> = bitvec
                    .iter()
                    .map(|val| self.interner.resolve(val).resolved(interner).to_string())
                    .collect();
                line.sort();
                writeln!(f, "({})", line.join(" "))?;
            }
        }

        writeln!(f, ")\n\n(out")?;

        for vec in &self.inst_out {
            for bitvec in vec {
                let mut line: Vec<String> = bitvec
                    .iter()
                    .map(|val| self.interner.resolve(val).resolved(interner).to_string())
                    .collect();
                line.sort();
                writeln!(f, "({})", line.join(" "))?;
            }
        }

        writeln!(f, ")\n\n)")
    }
}

pub fn compute_liveness(func: &Function) -> LivenessResult {
    let interner = func
        .basic_blocks
        .iter()
        .flat_map(|block| &block.instructions)
        .flat_map(|inst| inst.uses().chain(inst.defs()))
        .chain(Register::gp_registers().map(Value::Register))
        .fold(Interner::new(), |mut interner, val| {
            interner.intern(val);
            interner
        });

    let num_gp_variables = interner.len();
    let num_blocks = func.basic_blocks.len();
    let mut block_gen = vec![BitVector::new(num_gp_variables); num_blocks];
    let mut block_kill = vec![BitVector::new(num_gp_variables); num_blocks];

    for (i, block) in func.basic_blocks.iter().enumerate() {
        for inst in &block.instructions {
            block_gen[i].set_from(inst.uses().filter_map(|use_| {
                let j = interner[&use_];
                (!block_kill[i].test(j)).then_some(j)
            }));
            block_kill[i].set_from(inst.defs().map(|def| interner[&def]));
        }
    }

    let mut block_in = vec![BitVector::new(num_gp_variables); num_blocks];
    let mut block_out = vec![BitVector::new(num_gp_variables); num_blocks];
    let mut worklist = Worklist::new();
    worklist.extend((0..num_blocks).map(BlockId));

    while let Some(id) = worklist.pop() {
        let i = id.0;

        block_out[i].clear();
        for succ in &func.cfg.successors[i] {
            block_out[i].union(&block_in[succ.0]);
        }

        let mut temp = block_out[i].clone();
        temp.difference(&block_kill[i]);
        temp.union(&block_gen[i]);

        if temp != block_in[i] {
            block_in[i] = temp;
            worklist.extend(func.cfg.predecessors[i].iter().copied());
        }
    }

    let empty_dataflow_set = || -> Vec<Vec<BitVector>> {
        func.basic_blocks
            .iter()
            .map(|block| vec![BitVector::new(num_gp_variables); block.instructions.len()])
            .collect()
    };

    let mut inst_gen = empty_dataflow_set();
    let mut inst_kill = empty_dataflow_set();
    let mut inst_in = empty_dataflow_set();
    let mut inst_out = empty_dataflow_set();

    for (i, block) in func.basic_blocks.iter().enumerate() {
        for (j, inst) in block.instructions.iter().enumerate().rev() {
            inst_gen[i][j].set_from(inst.uses().map(|use_| interner[&use_]));
            inst_kill[i][j].set_from(inst.defs().map(|def| interner[&def]));

            inst_out[i][j] = if j == block.instructions.len() - 1 {
                block_out[i].clone()
            } else {
                inst_in[i][j + 1].clone()
            };

            inst_in[i][j] = inst_out[i][j].clone();
            inst_in[i][j].difference(&inst_kill[i][j]);
            inst_in[i][j].union(&inst_gen[i][j]);
        }
    }

    LivenessResult {
        block_out,
        inst_in,
        inst_out,
        interner,
    }
}
