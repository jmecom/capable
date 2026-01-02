//! ABI quirks and lowering helpers.
//!
//! These helpers are the single source of truth for the special-case ABI
//! shapes we use to lower `Result` values (ResultOut).

use crate::abi::AbiType;

use super::FnSig;

/// Return true if the ABI type is lowered using ResultOut parameters.
pub fn is_result_out(ty: &AbiType) -> bool {
    matches!(ty, AbiType::ResultOut(_, _))
}

/// Return true if the ABI type uses any Result lowering.
pub fn is_result_lowering(ty: &AbiType) -> bool {
    is_result_out(ty)
}

fn is_sret_lowering(abi_sig: &FnSig, sig: &FnSig) -> bool {
    if sig.ret != AbiType::Ptr || abi_sig.ret != AbiType::Unit {
        return false;
    }
    if abi_sig.params.len() != sig.params.len() + 1 {
        return false;
    }
    if abi_sig.params.first() != Some(&AbiType::Ptr) {
        return false;
    }
    abi_sig.params[1..] == sig.params
}

/// Return true if a signature mismatch is explained by Result or sret lowering.
pub fn abi_sig_requires_lowering(abi_sig: &FnSig, sig: &FnSig) -> bool {
    abi_sig != sig && (is_result_lowering(&abi_sig.ret) || is_sret_lowering(abi_sig, sig))
}

/// Error message used when a layout is requested for a lowered Result ABI.
pub fn result_lowering_layout_error() -> &'static str {
    "layout for result out params"
}

/// Error message used when a Result ABI form is not supported.
pub fn result_abi_mismatch_error() -> &'static str {
    "result abi mismatch"
}

/// Error message used when ResultOut lowering is requested but unsupported.
pub fn result_out_params_error() -> &'static str {
    "result out params"
}
