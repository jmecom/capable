pub mod ast;
pub mod abi;
pub mod codegen;
pub mod error;
pub mod hir;
pub mod lexer;
pub mod loader;
pub mod parser;
pub mod typeck;

pub use error::{ParseError, TypeError};
pub use codegen::build_object;
pub use hir::{HirModule, HirProgram};
pub use loader::{
    load_module_from_path, load_stdlib, load_user_modules, load_user_modules_transitive,
    stdlib_root, validate_module_path, ModuleGraph,
};
pub use parser::parse_module;
pub use typeck::{type_check, type_check_program};
