use std::process::ExitCode;

use clap::Parser;
use substrait_explain::cli::Cli;

fn main() -> ExitCode {
    let cli = Cli::parse();
    cli.run()
}
