mod analysis;
mod codegen;
mod isel;
mod parser;

use clap::Parser;

use crate::codegen::generate_code;
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

        if cli.generate == 1 {
            generate_code(&prog).unwrap();
        }
    }
}
