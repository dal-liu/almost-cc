mod analysis;
mod parser;
mod ssa;

use clap::Parser;

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

        construct_ssa_form(&mut prog);
        println!("{}", &prog);
    }
}
