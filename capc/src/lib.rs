pub mod ast;
pub mod error;
pub mod lexer;
pub mod loader;
pub mod parser;
pub mod typeck;

pub use error::{ParseError, TypeError};
pub use loader::{load_module_from_path, load_stdlib, stdlib_root};
pub use parser::parse_module;
pub use typeck::{type_check, type_check_program};
