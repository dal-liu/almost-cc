mod analysis;
mod isel;
mod parser;

use clap::Parser;
use utils::DisplayResolved;

use crate::analysis::{DefUseChain, compute_liveness, compute_reaching_def};
use crate::isel::{cover_forest, create_contexts, generate_forest, isel_tiles};
use crate::parser::parse_file;

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
    if let Some(prog) = parse_file(&cli.source) {
        if cli.verbose {
            print!("{}", &prog);
        }

        for func in &prog.functions {
            let liveness = compute_liveness(func);
            let reaching_def = compute_reaching_def(func);
            let def_use = DefUseChain::new(func, &reaching_def);
            let mut contexts = create_contexts(func);
            let tiles = isel_tiles();
            for ctx in &mut contexts {
                let forest = generate_forest(func, &liveness, &def_use, ctx);
                print!("{}", forest.resolved(&prog.interner));
                let covers = cover_forest(&forest, &tiles);
                dbg!(&covers);
            }
        }
    }
}
