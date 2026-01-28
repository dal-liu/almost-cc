mod analysis;
mod parser;
mod ssa;
mod transform;

use clap::Parser;
use utils::DisplayResolved;

use crate::parser::parse_file;
use crate::ssa::construct_ssa_form;
use crate::transform::{constant_folding, constant_propagation};

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
            construct_ssa_form(func, &mut prog.interner);

            loop {
                let mut modified = false;
                modified |= constant_propagation(func);
                modified |= constant_folding(func);

                if !modified {
                    break;
                }
            }

            println!("{}", func.resolved(&prog.interner));
        }
    }
}
