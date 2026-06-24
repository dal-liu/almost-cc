mod analysis;
mod codegen;
mod parser;
mod ssa;
mod tracing;
mod transform;
mod translate;

use clap::Parser;
use utils::cli::Cli;

use crate::codegen::generate_code;
use crate::parser::parse_file;
use crate::ssa::{construct_ssa_form, destroy_ssa_form, split_critical_edges};

fn main() {
    let cli = Cli::parse();
    if let Some(mut prog) = parse_file(&cli.source) {
        if cli.verbose {
            print!("{}", &prog);
        }

        if cli.opt_level > 0 {
            construct_ssa_form(&mut prog);
            split_critical_edges(&mut prog);
            destroy_ssa_form(&mut prog);
        }

        if cli.generate == 1 {
            generate_code(&mut prog).unwrap();
        }
    }
}
