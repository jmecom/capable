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
