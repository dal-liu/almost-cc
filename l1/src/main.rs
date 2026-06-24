mod codegen;
mod parser;

use clap::Parser;
use utils::cli::Cli;

use crate::codegen::generate_code;
use crate::parser::parse_file;

fn main() {
    let cli = Cli::parse();
    if let Some(prog) = parse_file(&cli.source) {
        if cli.verbose {
            print!("{}", &prog);
        }
        if cli.generate == 1 {
            generate_code(&prog).unwrap()
        }
    }
}
