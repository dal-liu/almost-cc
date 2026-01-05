mod analysis;
mod parser;

use clap::Parser;

use crate::analysis::{DominanceFrontier, DominatorTree};
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
            let dom_tree = DominatorTree::new(func);
            let dom_frontier = DominanceFrontier::new(func, &dom_tree);
            dbg!(&dom_frontier);
        }
    }
}
