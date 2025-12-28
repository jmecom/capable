# Standard Library Notes

Some stdlib methods are implemented by the runtime instead of Capable code.
If a method has a stub body (like `return 0` or `return ()`), its actual
implementation lives in the runtime and the compiler treats it as an intrinsic.

The single source of truth for these runtime-backed intrinsics is:
- `capc/src/codegen/intrinsics.rs`

Anything not listed there is a real Capable implementation.
