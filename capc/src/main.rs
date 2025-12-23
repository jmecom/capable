use std::path::PathBuf;

use clap::{Parser, Subcommand};
use miette::{miette, NamedSource, Result};

use capc::{load_stdlib, parse_module, type_check_program};

#[derive(Debug, Parser)]
#[command(name = "capc", version, about = "Capable compiler (milestone 0/1)")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    Parse { path: PathBuf },
    Check { path: PathBuf },
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    match cli.command {
        Command::Parse { path } => {
            let source = std::fs::read_to_string(&path)
                .map_err(|err| miette!("failed to read {}: {err}", path.display()))?;
            let module = parse_module(&source).map_err(|err| {
                let named = NamedSource::new(path.display().to_string(), source);
                miette::Report::new(err).with_source_code(named)
            })?;
            println!("{module:#?}");
            Ok(())
        }
        Command::Check { path } => {
            let source = std::fs::read_to_string(&path)
                .map_err(|err| miette!("failed to read {}: {err}", path.display()))?;
            let module = parse_module(&source).map_err(|err| {
                let named = NamedSource::new(path.display().to_string(), source.clone());
                miette::Report::new(err).with_source_code(named)
            })?;
            let stdlib = load_stdlib().map_err(|err| {
                miette::Report::new(err)
            })?;
            type_check_program(&module, &stdlib).map_err(|err| {
                let named = NamedSource::new(path.display().to_string(), source);
                miette::Report::new(err).with_source_code(named)
            })?;
            println!("ok");
            Ok(())
        }
    }
}
