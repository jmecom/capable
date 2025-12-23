use std::path::PathBuf;

use clap::{Parser, Subcommand};
use miette::{miette, NamedSource, Result};

use capc::{build_object, parse_module, type_check_program, validate_module_path, ModuleGraph};

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
    },
    Run {
        path: PathBuf,
        #[arg(long)]
        out_dir: Option<PathBuf>,
        #[arg(long)]
        safe_only: bool,
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
            let source = std::fs::read_to_string(&path)
                .map_err(|err| miette!("failed to read {}: {err}", path.display()))?;
            let module = parse_module(&source).map_err(|err| {
                let named = NamedSource::new(path.display().to_string(), source.clone());
                miette::Report::new(err).with_source_code(named)
            })?;
            let root = path.parent().ok_or_else(|| {
                miette!("entry path has no parent directory")
            })?;
            validate_module_path(&module, &path, root).map_err(|err| {
                miette::Report::new(err)
            })?;
            let mut graph = ModuleGraph::new();
            let stdlib = graph.load_stdlib().map_err(|err| {
                miette::Report::new(err)
            })?;
            let user_modules = graph.load_user_modules_transitive(&path, &module).map_err(|err| {
                miette::Report::new(err)
            })?;
            if safe_only {
                enforce_safe_only(&module, &user_modules)?;
            }
            type_check_program(&module, &stdlib, &user_modules).map_err(|err| {
                let named = NamedSource::new(path.display().to_string(), source);
                miette::Report::new(err).with_source_code(named)
            })?;
            println!("ok");
            Ok(())
        }
        Command::Build {
            path,
            out,
            out_dir,
            safe_only,
        } => {
            let out_path = build_binary(&path, out, out_dir, safe_only)?;
            println!("built {}", out_path.display());
            Ok(())
        }
        Command::Run {
            path,
            out_dir,
            safe_only,
        } => {
            let out_path = build_binary(&path, None, out_dir, safe_only)?;
            let status = std::process::Command::new(&out_path)
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

fn build_binary(
    path: &PathBuf,
    out: Option<PathBuf>,
    out_dir: Option<PathBuf>,
    safe_only: bool,
) -> Result<PathBuf> {
    let source = std::fs::read_to_string(path)
        .map_err(|err| miette!("failed to read {}: {err}", path.display()))?;
    let module = parse_module(&source).map_err(|err| {
        let named = NamedSource::new(path.display().to_string(), source.clone());
        miette::Report::new(err).with_source_code(named)
    })?;
    let root = path.parent().ok_or_else(|| miette!("entry path has no parent directory"))?;
    validate_module_path(&module, path, root).map_err(|err| miette::Report::new(err))?;
    let mut graph = ModuleGraph::new();
    let stdlib = graph.load_stdlib().map_err(|err| miette::Report::new(err))?;
    let user_modules = graph
        .load_user_modules_transitive(path, &module)
        .map_err(|err| miette::Report::new(err))?;
    if safe_only {
        enforce_safe_only(&module, &user_modules)?;
    }
    type_check_program(&module, &stdlib, &user_modules).map_err(|err| {
        let named = NamedSource::new(path.display().to_string(), source.clone());
        miette::Report::new(err).with_source_code(named)
    })?;

    let workspace_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
    let build_dir = out_dir.unwrap_or_else(|| workspace_root.join("target").join("capc-out"));
    std::fs::create_dir_all(&build_dir).map_err(|err| {
        miette!("failed to create build dir {}: {err}", build_dir.display())
    })?;
    let obj_path = build_dir.join("program.o");
    build_object(&module, &user_modules, &stdlib, &obj_path)
        .map_err(|err| miette!("codegen failed: {err}"))?;

    let status = std::process::Command::new("cargo")
        .arg("build")
        .arg("-p")
        .arg("capable_runtime")
        .status()
        .map_err(|err| miette!("failed to run cargo build: {err}"))?;
    if !status.success() {
        return Err(miette!("runtime build failed"));
    }

    let stub_path = build_dir.join("capable_stub.rs");
    std::fs::write(
        &stub_path,
        "extern \"C\" { fn capable_rt_start() -> i32; }\nfn main() { let code = unsafe { capable_rt_start() }; std::process::exit(code); }\n",
    )
    .map_err(|err| miette!("failed to write stub: {err}"))?;

    let out_path = out.unwrap_or_else(|| build_dir.join("capable"));
    let runtime_lib_dir = workspace_root.join("target").join("debug");
    let status = std::process::Command::new("rustc")
        .arg(&stub_path)
        .arg("-L")
        .arg(&runtime_lib_dir)
        .arg("-lstatic=capable_runtime")
        .arg("-C")
        .arg(format!("link-arg={}", obj_path.display()))
        .arg("-o")
        .arg(&out_path)
        .status()
        .map_err(|err| miette!("failed to run rustc: {err}"))?;
    if !status.success() {
        return Err(miette!("link failed"));
    }
    Ok(out_path)
}

fn enforce_safe_only(entry: &capc::ast::Module, user_modules: &[capc::ast::Module]) -> Result<()> {
    let mut offenders = Vec::new();
    if entry.package == capc::ast::PackageSafety::Unsafe {
        offenders.push(entry.name.to_string());
    }
    for module in user_modules {
        if module.package == capc::ast::PackageSafety::Unsafe {
            offenders.push(module.name.to_string());
        }
    }
    if offenders.is_empty() {
        return Ok(());
    }
    offenders.sort();
    offenders.dedup();
    Err(miette!(
        "safe-only build rejected unsafe package(s): {}",
        offenders.join(", ")
    ))
}

fn audit_unsafe(path: &PathBuf) -> Result<()> {
    let source = std::fs::read_to_string(path)
        .map_err(|err| miette!("failed to read {}: {err}", path.display()))?;
    let module = parse_module(&source).map_err(|err| {
        let named = NamedSource::new(path.display().to_string(), source.clone());
        miette::Report::new(err).with_source_code(named)
    })?;
    let root = path
        .parent()
        .ok_or_else(|| miette!("entry path has no parent directory"))?;
    let mut graph = ModuleGraph::new();
    let stdlib = graph.load_stdlib().map_err(|err| miette::Report::new(err))?;
    let user_modules = graph
        .load_user_modules_transitive(path, &module)
        .map_err(|err| miette::Report::new(err))?;

    let mut findings = Vec::new();
    if module.package == capc::ast::PackageSafety::Unsafe {
        findings.push(format!(
            "user: {} ({})",
            module.name,
            module_path_for(root, &module.name).display()
        ));
    }
    for module in &user_modules {
        if module.package == capc::ast::PackageSafety::Unsafe {
            findings.push(format!(
                "user: {} ({})",
                module.name,
                module_path_for(root, &module.name).display()
            ));
        }
    }
    for module in &stdlib {
        if module.package == capc::ast::PackageSafety::Unsafe {
            findings.push(format!(
                "stdlib: {} ({})",
                module.name,
                module_path_for(&capc::stdlib_root(), &module.name).display()
            ));
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

fn module_path_for(root: &std::path::Path, name: &capc::ast::Path) -> PathBuf {
    let mut path = root.to_path_buf();
    for seg in &name.segments {
        path.push(&seg.item);
    }
    path.set_extension("cap");
    path
}
