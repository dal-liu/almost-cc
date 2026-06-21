use std::fs::File;
use std::io::{self, BufWriter, Write};

use ir::*;

use crate::translate::translate_program;

struct CodeGenerator {
    stream: BufWriter<File>,
}

impl CodeGenerator {
    fn new() -> io::Result<Self> {
        let file = File::create("prog.L3")?;
        Ok(Self {
            stream: BufWriter::new(file),
        })
    }

    fn emit_program(&mut self, prog: &mut Program) -> io::Result<()> {
        write!(self.stream, "{}", translate_program(prog))
    }

    fn finish(mut self) -> io::Result<()> {
        self.stream.flush()
    }
}

pub fn generate_code(prog: &mut Program) -> io::Result<()> {
    let mut code_generator = CodeGenerator::new()?;
    code_generator.emit_program(prog)?;
    code_generator.finish()
}
