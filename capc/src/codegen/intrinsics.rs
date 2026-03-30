//! Runtime intrinsic registry.
//!
//! Any stdlib function listed here is treated as an intrinsic: its `.cap` body
//! is ignored, and codegen emits a direct call to the runtime symbol. If a
//! function is not listed here, the Capable implementation is used instead.
//! See `stdlib/README.md` for the stdlib-facing explanation.

use std::collections::HashMap;

use super::{FnInfo, FnSig};
use crate::runtime_intrinsics::runtime_bindings;

pub fn register_runtime_intrinsics() -> HashMap<String, FnInfo> {
    runtime_bindings()
        .iter()
        .map(|(key, binding)| {
            (
                key.clone(),
                FnInfo {
                    sig: FnSig {
                        params: binding.sig.params.clone(),
                        ret: binding.sig.ret.clone(),
                    },
                    abi_sig: binding.abi_sig.as_ref().map(|abi_sig| FnSig {
                        params: abi_sig.params.clone(),
                        ret: abi_sig.ret.clone(),
                    }),
                    symbol: binding.symbol.to_string(),
                    runtime_symbol: None,
                    is_runtime: true,
                },
            )
        })
        .collect()
}
