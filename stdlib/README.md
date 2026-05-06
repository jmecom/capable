# Standard Library Notes

Some stdlib methods are implemented by the runtime instead of Capable code.
If a method has a stub body (like `return 0` or `return ()`), its actual
implementation lives in the runtime and the compiler treats it as an intrinsic.

The single source of truth for these runtime-backed intrinsics is:
- `capc/src/codegen/intrinsics.rs`

Anything not listed there is a real Capable implementation.

## API conventions

- Default stdlib APIs use the process default allocator and are meant for
  ordinary application code.
- Explicit `Alloc` paths are for controlled allocation. Prefer methods on
  `Alloc` where the API has one; `_with_alloc` variants are the low-level
  escape hatch for APIs that must pass an allocator to runtime intrinsics.
- String helpers whose names end in `_view` never copy string bytes. Default
  string helpers also prefer views when a view can represent the result.
- Helpers named `copy_*`, `to_text`, or `copy_string` allocate owned data.
