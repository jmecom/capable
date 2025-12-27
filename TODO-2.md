TODO-2

- Decide ResultString ABI size for len on 32-bit; either document u64 or use ptr-sized slot + widen in codegen.
- Clarify string slice intrinsics mapping where both string::as_slice and string::bytes map to capable_rt_string_as_slice; add comment or distinct runtime symbol if needed.
- Revisit ResultOut stack slot alignment guarantees; ensure alignment is explicitly respected if Cranelift requires it.
- Add ABI mismatch guardrails: if abi_sig != sig, assert it is a handled ResultString/ResultOut lowering.
- Add symbol-level tests for wrapper export + runtime import if we want hard enforcement.
