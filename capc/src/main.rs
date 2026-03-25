use std::path::PathBuf;

use clap::{Parser, Subcommand};
use miette::{miette, NamedSource, Result};

use capc::{build_binary, load_program, module_path_for, parse_module, type_check_loaded, LinkOptions};

#[derive(Debug, Parser)]
#[command(name = "capc", version, about = "Capable compiler (milestone 0/1)")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    Parse { path: PathBuf },
    Check {
        path: PathBuf,
        #[arg(long)]
        safe_only: bool,
    },
    Build {
        path: PathBuf,
        #[arg(short, long)]
        out: Option<PathBuf>,
        #[arg(long)]
        out_dir: Option<PathBuf>,
        #[arg(long)]
        safe_only: bool,
        #[arg(long = "link-lib")]
        link_libs: Vec<String>,
        #[arg(long = "link-search")]
        link_search: Vec<PathBuf>,
    },
    Run {
        path: PathBuf,
        #[arg(long)]
        out_dir: Option<PathBuf>,
        #[arg(long)]
        safe_only: bool,
        #[arg(long = "link-lib")]
        link_libs: Vec<String>,
        #[arg(long = "link-search")]
        link_search: Vec<PathBuf>,
        #[arg(trailing_var_arg = true)]
        args: Vec<String>,
    },
    Audit { path: PathBuf },
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
        Command::Check { path, safe_only } => {
            let loaded = load_program(&path)?;
            let _program = type_check_loaded(&loaded, safe_only)?;
            println!("ok");
            Ok(())
        }
        Command::Build {
            path,
            out,
            out_dir,
            safe_only,
            link_libs,
            link_search,
        } => {
            let loaded = load_program(&path)?;
            let program = type_check_loaded(&loaded, safe_only)?;
            let out_path = build_binary(
                &loaded,
                &program,
                LinkOptions {
                    out,
                    out_dir,
                    link_libs: &link_libs,
                    link_search: &link_search,
                },
            )?;
            println!("built {}", out_path.display());
            Ok(())
        }
        Command::Run {
            path,
            out_dir,
            safe_only,
            link_libs,
            link_search,
            args,
        } => {
            let loaded = load_program(&path)?;
            let program = type_check_loaded(&loaded, safe_only)?;
            let out_path = build_binary(
                &loaded,
                &program,
                LinkOptions {
                    out: None,
                    out_dir,
                    link_libs: &link_libs,
                    link_search: &link_search,
                },
            )?;
            let status = std::process::Command::new(&out_path)
                .args(&args)
                .status()
                .map_err(|err| miette!("failed to run {}: {err}", out_path.display()))?;
            if !status.success() {
                return Err(miette!("program exited with failure"));
            }
            Ok(())
        }
        Command::Audit { path } => audit_unsafe(&path),
    }
}

fn audit_unsafe(path: &PathBuf) -> Result<()> {
    let loaded = load_program(path)?;

    let mut findings = Vec::new();
    if let Some(entry) = audit_entry("user", &loaded.module, &loaded.root) {
        findings.push(entry);
    }
    for module in &loaded.user_modules {
        if let Some(entry) = audit_entry("user", module, &loaded.root) {
            findings.push(entry);
        }
    }
    for module in &loaded.stdlib {
        if let Some(entry) = audit_entry("stdlib", module, &capc::stdlib_root()) {
            findings.push(entry);
        }
    }

    if findings.is_empty() {
        println!("no unsafe packages");
        return Ok(());
    }

    findings.sort();
    println!("unsafe packages:");
    for entry in findings {
        println!("- {entry}");
    }
    Ok(())
}

fn audit_entry(
    scope: &str,
    module: &capc::ast::Module,
    root: &std::path::Path,
) -> Option<String> {
    let mut reasons = Vec::new();
    if module.package == capc::ast::PackageSafety::Unsafe {
        reasons.push("package unsafe");
    }
    if module
        .items
        .iter()
        .any(|item| matches!(item, capc::ast::Item::ExternFunction(_)))
    {
        reasons.push("extern declarations");
    }
    if reasons.is_empty() {
        return None;
    }
    Some(format!(
        "{scope}: {} ({}): {}",
        module.name,
        module_path_for(root, &module.name).display(),
        reasons.join(", ")
    ))
}
