use clap::Parser;

#[derive(Parser)]
pub struct Cli {
    #[arg(short, default_value_t = false)]
    pub verbose: bool,

    #[arg(short, default_value_t = 1)]
    pub generate: u8,

    #[arg(short = 'O', default_value_t = 0)]
    pub opt_level: u8,

    pub source: String,
}
