use anyhow::Result;
use clap::Parser;
use substrait_explain::cli::Cli;

fn main() -> Result<()> {
    let cli = Cli::parse();
    cli.run()
}
