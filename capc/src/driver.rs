use std::path::{Path, PathBuf};

use miette::{miette, NamedSource, Result};

use crate::ast::{Module, PackageSafety, Path as AstPath};
use crate::hir::HirProgram;
use crate::{build_object, parse_module, type_check_program, validate_module_path, ModuleGraph};

#[derive(Clone)]
pub struct LoadedProgram {
    pub path: PathBuf,
    pub source: String,
    pub module: Module,
    pub stdlib: Vec<Module>,
    pub user_modules: Vec<Module>,
    pub root: PathBuf,
}

pub struct LinkOptions<'a> {
    pub out: Option<PathBuf>,
    pub out_dir: Option<PathBuf>,
    pub link_libs: &'a [String],
    pub link_search: &'a [PathBuf],
}

pub fn load_program(path: &Path) -> Result<LoadedProgram> {
    let source = std::fs::read_to_string(path)
        .map_err(|err| miette!("failed to read {}: {err}", path.display()))?;
    let module = parse_module(&source).map_err(|err| {
        let named = NamedSource::new(path.display().to_string(), source.clone());
        miette::Report::new(err).with_source_code(named)
    })?;
    let root = path
        .parent()
        .ok_or_else(|| miette!("entry path has no parent directory"))?
        .to_path_buf();
    validate_module_path(&module, path, &root).map_err(|err| {
        let err = err.with_context(format!("while loading module `{}`", module.name));
        miette::Report::new(err)
    })?;
    let mut graph = ModuleGraph::new();
    let stdlib = graph.load_stdlib().map_err(miette::Report::new)?;
    let user_modules = graph
        .load_user_modules_transitive(path, &module)
        .map_err(miette::Report::new)?;
    Ok(LoadedProgram {
        path: path.to_path_buf(),
        source,
        module,
        stdlib,
        user_modules,
        root,
    })
}

pub fn type_check_loaded(loaded: &LoadedProgram, safe_only: bool) -> Result<HirProgram> {
    if safe_only {
        enforce_safe_only(&loaded.module, &loaded.user_modules, &loaded.root)?;
    }
    type_check_program(&loaded.module, &loaded.stdlib, &loaded.user_modules).map_err(|err| {
        let named = NamedSource::new(loaded.path.display().to_string(), loaded.source.clone());
        miette::Report::new(err).with_source_code(named)
    })
}

pub fn build_binary(
    loaded: &LoadedProgram,
    program: &HirProgram,
    options: LinkOptions<'_>,
) -> Result<PathBuf> {
    let workspace_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("..");
    let build_dir = options
        .out_dir
        .unwrap_or_else(|| workspace_root.join("target").join("capc-out"));
    std::fs::create_dir_all(&build_dir)
        .map_err(|err| miette!("failed to create build dir {}: {err}", build_dir.display()))?;

    let obj_path = build_dir.join("program.o");
    build_object(program, &obj_path).map_err(|err| {
        let named = NamedSource::new(loaded.path.display().to_string(), loaded.source.clone());
        miette::Report::new(err).with_source_code(named)
    })?;

    let cargo_path = resolve_tool("CARGO", "cargo");
    let rustc_path = resolve_tool("RUSTC", "rustc");

    let status = std::process::Command::new(&cargo_path)
        .arg("build")
        .arg("-p")
        .arg("capable_runtime")
        .status()
        .map_err(|err| miette!("failed to run {} build: {err}", cargo_path.display()))?;
    if !status.success() {
        return Err(miette!("runtime build failed"));
    }

    let stub_path = build_dir.join("capable_stub.rs");
    std::fs::write(
        &stub_path,
        "extern \"C\" { fn capable_rt_start() -> i32; }\nfn main() { let code = unsafe { capable_rt_start() }; std::process::exit(code); }\n",
    )
    .map_err(|err| miette!("failed to write stub: {err}"))?;

    let out_path = options.out.unwrap_or_else(|| {
        let name = loaded
            .path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("a.out");
        build_dir.join(name)
    });
    let runtime_lib_dir = workspace_root.join("target").join("debug");
    let mut rustc = std::process::Command::new(&rustc_path);
    rustc
        .arg(&stub_path)
        .arg("-L")
        .arg(&runtime_lib_dir)
        .arg("-lstatic=capable_runtime")
        .arg("-C")
        .arg(format!("link-arg={}", obj_path.display()))
        .arg("-o")
        .arg(&out_path);
    for path in options.link_search {
        rustc.arg("-L").arg(path);
    }
    for lib in options.link_libs {
        rustc.arg("-l").arg(lib);
    }
    let output = rustc
        .output()
        .map_err(|err| miette!("failed to run {}: {err}", rustc_path.display()))?;
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

pub fn enforce_safe_only(entry: &Module, user_modules: &[Module], root: &Path) -> Result<()> {
    let mut offenders = Vec::new();
    if entry.package == PackageSafety::Unsafe {
        offenders.push(format!(
            "{} ({}): package unsafe",
            entry.name,
            module_path_for(root, &entry.name).display()
        ));
    }
    for module in user_modules {
        if module.package == PackageSafety::Unsafe {
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

pub fn module_path_for(root: &Path, name: &AstPath) -> PathBuf {
    let mut path = root.to_path_buf();
    for seg in &name.segments {
        path.push(&seg.item);
    }
    path.set_extension("cap");
    path
}

fn resolve_tool(env_var: &str, fallback_name: &str) -> PathBuf {
    if let Some(path) = std::env::var_os(env_var) {
        return PathBuf::from(path);
    }
    if let Some(home) = std::env::var_os("HOME") {
        let candidate = PathBuf::from(home).join(".cargo").join("bin").join(fallback_name);
        if candidate.exists() {
            return candidate;
        }
    }
    PathBuf::from(fallback_name)
}
