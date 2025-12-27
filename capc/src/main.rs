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
                enforce_safe_only(&module, &user_modules, root)?;
            }
            let _program = type_check_program(&module, &stdlib, &user_modules).map_err(|err| {
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
            link_libs,
            link_search,
        } => {
            let out_path = build_binary(&path, out, out_dir, safe_only, &link_libs, &link_search)?;
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
            let out_path = build_binary(&path, None, out_dir, safe_only, &link_libs, &link_search)?;
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

fn build_binary(
    path: &PathBuf,
    out: Option<PathBuf>,
    out_dir: Option<PathBuf>,
    safe_only: bool,
    link_libs: &[String],
    link_search: &[PathBuf],
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
        enforce_safe_only(&module, &user_modules, root)?;
    }
    let program = type_check_program(&module, &stdlib, &user_modules).map_err(|err| {
        let named = NamedSource::new(path.display().to_string(), source.clone());
        miette::Report::new(err).with_source_code(named)
    })?;

    // Step 5: Pass HIR to codegen
    let workspace_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
    let build_dir = out_dir.unwrap_or_else(|| workspace_root.join("target").join("capc-out"));
    std::fs::create_dir_all(&build_dir).map_err(|err| {
        miette!("failed to create build dir {}: {err}", build_dir.display())
    })?;
    let obj_path = build_dir.join("program.o");
    build_object(&program, &obj_path)
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
    let mut rustc = std::process::Command::new("rustc");
    rustc
        .arg(&stub_path)
        .arg("-L")
        .arg(&runtime_lib_dir)
        .arg("-lstatic=capable_runtime")
        .arg("-C")
        .arg(format!("link-arg={}", obj_path.display()))
        .arg("-o")
        .arg(&out_path);
    for path in link_search {
        rustc.arg("-L").arg(path);
    }
    for lib in link_libs {
        rustc.arg("-l").arg(lib);
    }
    let output = rustc.output().map_err(|err| miette!("failed to run rustc: {err}"))?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stderr = stderr.trim();
        if stderr.is_empty() {
            return Err(miette!("link failed"));
        }
        return Err(miette!("link failed: {stderr}"));
    }
    Ok(out_path)
}

fn enforce_safe_only(
    entry: &capc::ast::Module,
    user_modules: &[capc::ast::Module],
    root: &std::path::Path,
) -> Result<()> {
    let mut offenders = Vec::new();
    if entry.package == capc::ast::PackageSafety::Unsafe {
        offenders.push(format!(
            "{} ({}): package unsafe",
            entry.name,
            module_path_for(root, &entry.name).display()
        ));
    }
    for module in user_modules {
        if module.package == capc::ast::PackageSafety::Unsafe {
            offenders.push(format!(
                "{} ({}): package unsafe",
                module.name,
                module_path_for(root, &module.name).display()
            ));
        }
    }
    if offenders.is_empty() {
        return Ok(());
    }
    offenders.sort();
    offenders.dedup();
    let mut message = String::from("safe-only build rejected unsafe package(s):");
    for entry in offenders {
        message.push_str("\n- ");
        message.push_str(&entry);
    }
    Err(miette!(message))
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
    if let Some(entry) = audit_entry("user", &module, root) {
        findings.push(entry);
    }
    for module in &user_modules {
        if let Some(entry) = audit_entry("user", module, root) {
            findings.push(entry);
        }
    }
    for module in &stdlib {
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

fn module_path_for(root: &std::path::Path, name: &capc::ast::Path) -> PathBuf {
    let mut path = root.to_path_buf();
    for seg in &name.segments {
        path.push(&seg.item);
    }
    path.set_extension("cap");
    path
}
