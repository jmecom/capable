//! ABI quirks and lowering helpers.
//!
//! These helpers are the single source of truth for the special-case ABI
//! shapes we use to lower `Result` values (ResultString and ResultOut).

use crate::abi::AbiType;

use super::FnSig;

/// ResultString ABI uses a u64 length slot across targets.
pub fn result_string_len_bytes() -> u32 {
    8
}

/// Return true if the ABI type is lowered as ResultString.
pub fn is_result_string(ty: &AbiType) -> bool {
    matches!(ty, AbiType::ResultString)
}

/// Return true if the ABI type is lowered using ResultOut parameters.
pub fn is_result_out(ty: &AbiType) -> bool {
    matches!(ty, AbiType::ResultOut(_, _))
}

/// Return true if the ABI type uses any Result lowering.
pub fn is_result_lowering(ty: &AbiType) -> bool {
    is_result_string(ty) || is_result_out(ty)
}

/// Return true if a signature mismatch is explained by Result lowering.
pub fn abi_sig_requires_lowering(abi_sig: &FnSig, sig: &FnSig) -> bool {
    abi_sig != sig && is_result_lowering(&abi_sig.ret)
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

/// Error message used when ResultString lowering is requested but unsupported.
pub fn result_string_params_error() -> &'static str {
    "result abi"
}
