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
use crate::transform::run_opt_pipeline;

fn main() {
    let cli = Cli::parse();
    if let Some(mut prog) = parse_file(&cli.source) {
        if cli.verbose {
            print!("{}", &prog);
        }

        if cli.opt_level > 0 {
            run_opt_pipeline(&mut prog);
        }

        if cli.generate == 1 {
            generate_code(&mut prog).unwrap();
        }
    }
}
