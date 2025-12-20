mod analysis;
mod codegen;
mod isel;
mod mangling;
mod parser;
mod translation;

use clap::Parser;

use crate::codegen::generate_code;
use crate::mangling::mangle_labels;
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
    if let Some(mut prog) = parse_file(&cli.source) {
        if cli.verbose {
            print!("{}", &prog);
        }

        let (prefix, mut suffix) = mangle_labels(&mut prog);

        if cli.generate == 1 {
            generate_code(&mut prog, &prefix, &mut suffix).unwrap();
        }
    }
}
