//! Runtime intrinsic registry.
//!
//! Any stdlib function listed here is treated as an intrinsic: its `.cap` body
//! is ignored, and codegen emits a direct call to the runtime symbol. If a
//! function is not listed here, the Capable implementation is used instead.
//! See `stdlib/README.md` for the stdlib-facing explanation.

mod io;
mod memory;

use std::collections::HashMap;

use cranelift_codegen::ir::Type;

use super::{FnInfo, FnSig};

fn runtime_fn(sig: FnSig, abi_sig: Option<FnSig>, symbol: &str) -> FnInfo {
    FnInfo {
        sig,
        abi_sig,
        symbol: symbol.to_string(),
        runtime_symbol: None,
        is_runtime: true,
    }
}

pub fn register_runtime_intrinsics(ptr_ty: Type) -> HashMap<String, FnInfo> {
    let mut map = HashMap::new();
    io::register_io_intrinsics(&mut map);
    memory::register_memory_intrinsics(&mut map);
    let _ = ptr_ty;
    map
}
