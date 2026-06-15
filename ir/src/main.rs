mod analysis;
mod parser;
mod ssa;
mod transform;

use clap::Parser;
use utils::interner::DisplayResolved;

use crate::analysis::{dominators::DominatorTree, loops::LoopInfo};
use crate::parser::parse_file;
use crate::ssa::{construct_ssa_form, split_critical_edges};

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

        construct_ssa_form(&mut prog);
        split_critical_edges(&mut prog);

        for func in &prog.functions {
            let dom_tree = DominatorTree::new(func);
            let loops = LoopInfo::new(func, &dom_tree);
            println!("{}", loops.display(func).resolved(&prog.interner));
        }
    }
}
