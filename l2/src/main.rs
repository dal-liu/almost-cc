mod analysis;
mod codegen;
mod parser;
mod regalloc;
mod transform;
mod translation;

use clap::Parser;

use crate::codegen::generate_code;
use crate::parser::parse_file;
use crate::regalloc::allocate_registers;
use crate::transform::run_peephole_passes;

#[derive(Parser)]
struct Cli {
    #[arg(short, default_value_t = false)]
    verbose: bool,

    #[arg(short, default_value_t = 1)]
    generate: u8,

    source: String,
}

fn main() {
    let cli = Cli::parse();
    if let Some(mut prog) = parse_file(&cli.source) {
        if cli.verbose {
            print!("{}", &prog);
        }

        for func in &mut prog.functions {
            allocate_registers(func, &mut prog.interner);
            run_peephole_passes(func);
        }

        if cli.generate == 1 {
            generate_code(&prog).unwrap();
        }
    }
}
