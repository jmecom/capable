pub mod ast;
pub mod abi;
pub mod codegen;
mod desugar;
pub mod driver;
pub mod error;
pub mod hir;
pub mod lexer;
pub mod loader;
pub mod parser;
mod runtime_intrinsics;
pub mod typeck;

pub use error::{ParseError, TypeError};
pub use codegen::build_object;
pub use driver::{build_binary, enforce_safe_only, load_program, module_path_for, type_check_loaded, LinkOptions, LoadedProgram};
pub use hir::{HirModule, HirProgram};
pub use loader::{
    load_module_from_path, load_stdlib, load_user_modules, load_user_modules_transitive,
    stdlib_root, validate_module_path, ModuleGraph,
};
pub use parser::parse_module;
pub use typeck::{type_check, type_check_program};
