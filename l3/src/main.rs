mod analysis;
mod isel;
mod parser;

use clap::Parser;
use utils::DisplayResolved;

use crate::analysis::{build_def_use, compute_liveness, compute_reaching_def};
use crate::isel::{create_contexts, generate_forests, greedy_match};
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
            let def_use = build_def_use(func, &reaching_def);
            let mut contexts = create_contexts(func);
            let forests = generate_forests(func, &liveness, &def_use, &mut contexts);
            for forest in &forests {
                print!("{}", forest.resolved(&prog.interner));
                greedy_match(forest);
            }
        }
    }
}
