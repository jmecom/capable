use std::collections::{HashMap, VecDeque};
use std::fs;
use std::path::{Path, PathBuf};

use crate::error::ParseError;
use crate::parser::parse_module;
use crate::ast::Module;

pub fn stdlib_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../stdlib")
}

pub fn load_stdlib() -> Result<Vec<Module>, ParseError> {
    let root = stdlib_root().join("sys");
    let mut modules = Vec::new();
    let mut entries = Vec::new();
    for entry in fs::read_dir(&root).map_err(|err| {
        ParseError::new(format!("failed to read stdlib dir {root:?}: {err}"), crate::ast::Span::new(0, 0))
    })? {
        let entry = entry.map_err(|err| {
            ParseError::new(format!("failed to read stdlib entry: {err}"), crate::ast::Span::new(0, 0))
        })?;
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) == Some("cap") {
            entries.push(path);
        }
    }
    entries.sort();
    for path in entries {
        let module = load_module_from_path(&path)?;
        validate_module_path(&module, &path, &stdlib_root()).map_err(|err| {
            err.with_context(format!("while loading module `{}`", module.name))
        })?;
        modules.push(module);
    }
    Ok(modules)
}

pub fn load_module_from_path(path: &Path) -> Result<Module, ParseError> {
    let source = fs::read_to_string(path).map_err(|err| {
        ParseError::new(
            format!("failed to read {}: {err}", path.display()),
            crate::ast::Span::new(0, 0),
        )
    })?;
    parse_module(&source)
}

pub fn validate_module_path(
    module: &Module,
    path: &Path,
    root: &Path,
) -> Result<(), ParseError> {
    let rel = path
        .strip_prefix(root)
        .map_err(|_| {
            ParseError::new(
                format!("module path {} is not under root {}", path.display(), root.display()),
                module.span,
            )
        })?
        .with_extension("");
    let mut expected = String::new();
    for (i, seg) in module.name.segments.iter().enumerate() {
        if i > 0 {
            expected.push(std::path::MAIN_SEPARATOR);
        }
        expected.push_str(&seg.item);
    }
    let rel_str = rel.to_string_lossy();
    if rel_str != expected {
        return Err(ParseError::new(
            format!(
                "module `{}` must live at {}/{}.cap",
                module.name,
                root.display(),
                expected
            ),
            module.span,
        ));
    }
    Ok(())
}

pub struct ModuleGraph {
    cache: HashMap<PathBuf, Module>,
}

impl ModuleGraph {
    pub fn new() -> Self {
        Self {
            cache: HashMap::new(),
        }
    }

    pub fn load_stdlib(&mut self) -> Result<Vec<Module>, ParseError> {
        let root = stdlib_root().join("sys");
        let mut modules = Vec::new();
        let mut entries = Vec::new();
        for entry in fs::read_dir(&root).map_err(|err| {
            ParseError::new(
                format!("failed to read stdlib dir {root:?}: {err}"),
                crate::ast::Span::new(0, 0),
            )
        })? {
            let entry = entry.map_err(|err| {
                ParseError::new(
                    format!("failed to read stdlib entry: {err}"),
                    crate::ast::Span::new(0, 0),
                )
            })?;
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) == Some("cap") {
                entries.push(path);
            }
        }
        entries.sort();
        for path in entries {
            let module = self.load_cached(&path)?;
            validate_module_path(&module, &path, &stdlib_root()).map_err(|err| {
                err.with_context(format!("while loading module `{}`", module.name))
            })?;
            modules.push(module);
        }
        Ok(modules)
    }

    pub fn load_user_modules_transitive(
        &mut self,
        entry_path: &Path,
        entry_module: &Module,
    ) -> Result<Vec<Module>, ParseError> {
        let dir = entry_path
            .parent()
            .ok_or_else(|| {
                ParseError::new(
                    "entry path has no parent directory".to_string(),
                    crate::ast::Span::new(0, 0),
                )
            })?
            .to_path_buf();
        let mut modules = Vec::new();
        let mut queue: VecDeque<(PathBuf, PathBuf)> = VecDeque::new();

        for use_decl in &entry_module.uses {
            if let Some(path) = resolve_use_path(&dir, use_decl)? {
                queue.push_back((path, dir.clone()));
            }
        }

        while let Some((path, base_dir)) = queue.pop_front() {
            if self.cache.contains_key(&path) {
                continue;
            }
            let module = self.load_cached(&path)?;
            validate_module_path(&module, &path, &base_dir).map_err(|err| {
                err.with_context(format!("while loading module `{}`", module.name))
            })?;
            for use_decl in &module.uses {
                if let Some(dep_path) = resolve_use_path(&base_dir, use_decl)? {
                    queue.push_back((dep_path, base_dir.clone()));
                }
            }
            modules.push(module);
        }

        Ok(modules)
    }

    fn load_cached(&mut self, path: &Path) -> Result<Module, ParseError> {
        if let Some(module) = self.cache.get(path) {
            return Ok(module.clone());
        }
        let module = load_module_from_path(path)?;
        self.cache.insert(path.to_path_buf(), module.clone());
        Ok(module)
    }
}

pub fn load_user_modules(
    entry_path: &Path,
    entry_module: &Module,
) -> Result<Vec<Module>, ParseError> {
    load_user_modules_transitive(entry_path, entry_module)
}

pub fn load_user_modules_transitive(
    entry_path: &Path,
    entry_module: &Module,
) -> Result<Vec<Module>, ParseError> {
    let dir = entry_path
        .parent()
        .ok_or_else(|| {
            ParseError::new(
                "entry path has no parent directory".to_string(),
                crate::ast::Span::new(0, 0),
            )
        })?
        .to_path_buf();
    let mut modules = Vec::new();
    let mut cache: HashMap<PathBuf, Module> = HashMap::new();
    let mut queue: VecDeque<(PathBuf, PathBuf)> = VecDeque::new();

    for use_decl in &entry_module.uses {
        if let Some(path) = resolve_use_path(&dir, use_decl)? {
            queue.push_back((path, dir.clone()));
        }
    }

    while let Some((path, base_dir)) = queue.pop_front() {
        if cache.contains_key(&path) {
            continue;
        }
        let module = load_module_from_path(&path)?;
        validate_module_path(&module, &path, &base_dir).map_err(|err| {
            err.with_context(format!("while loading module `{}`", module.name))
        })?;
        for use_decl in &module.uses {
            if let Some(dep_path) = resolve_use_path(&base_dir, use_decl)? {
                queue.push_back((dep_path, base_dir.clone()));
            }
        }
        cache.insert(path, module.clone());
        modules.push(module);
    }

    Ok(modules)
}

fn resolve_use_path(base_dir: &Path, use_decl: &crate::ast::UseDecl) -> Result<Option<PathBuf>, ParseError> {
    let segments = &use_decl.path.segments;
    if segments.first().map(|s| s.item.as_str()) == Some("sys") {
        return Ok(None);
    }
    let mut path = base_dir.to_path_buf();
    if segments.len() == 1 {
        path.push(format!("{}.cap", segments[0].item));
    } else {
        for seg in segments.iter().take(segments.len() - 1) {
            path.push(&seg.item);
        }
        path.push(format!("{}.cap", segments.last().unwrap().item));
    }
    if !path.exists() {
        return Err(ParseError::new(
            format!("module file not found for `{}`", use_decl.path),
            use_decl.span,
        ));
    }
    Ok(Some(path))
}
