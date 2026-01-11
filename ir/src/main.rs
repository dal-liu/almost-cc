mod analysis;
mod parser;
mod ssa;

use clap::Parser;
use utils::DisplayResolved;

use crate::analysis::{DominanceFrontier, DominatorTree};
use crate::parser::parse_file;
use crate::ssa::construct_ssa_form;

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
            let dom_tree = DominatorTree::new(func);
            let dom_front = DominanceFrontier::new(func, &dom_tree);
            construct_ssa_form(func, &mut prog.interner, &dom_tree, &dom_front);
            print!("{}", func.resolved(&prog.interner));
        }
    }
}
