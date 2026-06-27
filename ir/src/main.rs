mod analysis;
mod codegen;
mod data_layout;
mod parser;
mod ssa;
mod tracing;
mod transform;
mod translate;

use clap::Parser;
use utils::cli::Cli;

use crate::codegen::generate_code;
use crate::data_layout::linearize_data_types;
use crate::parser::parse_file;
use crate::ssa::{
    construct_ssa_form, destroy_ssa_form, hoist_define_instructions, split_critical_edges,
};
use crate::transform::run_opt_pipeline;

fn main() {
    let cli = Cli::parse();
    if let Some(mut prog) = parse_file(&cli.source) {
        if cli.verbose {
            print!("{}", &prog);
        }

        linearize_data_types(&mut prog);
        hoist_define_instructions(&mut prog);
        construct_ssa_form(&mut prog);
        split_critical_edges(&mut prog);

        if cli.opt_level > 0 {
            run_opt_pipeline(&mut prog);
        }

        destroy_ssa_form(&mut prog);

        if cli.generate == 1 {
            generate_code(&mut prog).unwrap();
        }
    }
}
