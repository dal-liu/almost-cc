use std::fs::File;
use std::io::{self, BufWriter, Write};

use l2;
use l3::*;
use utils::{DisplayResolved, Interner};

use crate::analysis::{DefUseChain, compute_liveness, compute_reaching_def};
use crate::isel::contexts::create_contexts;
use crate::isel::forest::{NodeId, SelectionForest, generate_forest};
use crate::isel::tiling::{Cover, cover_forest, isel_tiles};

struct CodeGenerator {
    stream: BufWriter<File>,
}

impl CodeGenerator {
    fn new() -> io::Result<Self> {
        let file = File::create("prog.L2")?;
        Ok(Self {
            stream: BufWriter::new(file),
        })
    }

    fn emit_program(&mut self, prog: &Program) -> io::Result<()> {
        for func in &prog.functions {
            self.emit_function(func, &prog.interner)?;
        }
        Ok(())
    }

    fn emit_function(&mut self, func: &Function, interner: &Interner<String>) -> io::Result<()> {
        let liveness = compute_liveness(func);
        let reaching_def = compute_reaching_def(func);
        let def_use = DefUseChain::new(func, &reaching_def);
        let tiles = isel_tiles();
        let mut contexts = create_contexts(func);

        fn dfs(
            forest: &SelectionForest,
            id: NodeId,
            cover: &Cover,
            l2_instructions: &mut Vec<l2::Instruction>,
        ) {
            for child in forest.children(id) {
                dfs(forest, child, cover, l2_instructions);
            }

            if let Some(tile) = cover.map.get(&id) {
                l2_instructions.extend((tile.emit)(forest, id));
            }
        }

        for ctx in &mut contexts {
            let forest = generate_forest(func, &liveness, &def_use, ctx);
            let covers = cover_forest(&forest, &tiles);
            let mut l2_instructions = Vec::new();

            for cover in &covers {
                dfs(&forest, cover.root, cover, &mut l2_instructions);
            }

            for l2_inst in &l2_instructions {
                writeln!(self.stream, "    {}", l2_inst.resolved(interner))?;
            }
        }

        Ok(())
    }

    fn finish(mut self) -> io::Result<()> {
        self.stream.flush()
    }
}

pub fn generate_code(prog: &Program) -> io::Result<()> {
    let mut code_generator = CodeGenerator::new()?;
    code_generator.emit_program(&prog)?;
    code_generator.finish()
}
