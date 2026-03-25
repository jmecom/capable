# ABI Notes

This doc captures current ABI/lowering rules used by the compiler and runtime.
These rules are intentionally simple and may evolve.

## Struct returns (sret)

Capable does not currently return non-opaque structs in registers. Instead it
uses an explicit "structure return" (sret) out-parameter:

- A function that returns a non-opaque struct is lowered to:
  - an extra first parameter: `out: *T`
  - return type: `unit`
- The callee writes the struct into `out`.
- Callers allocate stack space and pass `&out`.

This is a common ABI strategy used by many toolchains for larger values.

## Result out-params for struct payloads

When a `Result<T, E>` payload is a non-opaque struct, the return is lowered to
out-params:

- The function takes extra `ok_out` / `err_out` pointer params for the
  struct payloads.
- The function returns only the Result tag (`Ok`/`Err` discriminator).
- Scalar payloads still use the normal in-register `Result` layout.

## Runtime wrappers

Runtime-backed intrinsics keep their original ABI (no sret) and are wrapped by
compiler-generated stubs when needed.

## Allocation convention

The user-facing stdlib now defaults to the process allocator for ordinary code.
Explicit `Alloc` handles still appear in low-level APIs and `_with_alloc`
variants, and those handles are passed through to the runtime. The runtime
currently backs `Alloc` with libc `malloc`/`free`, but the ABI keeps explicit
allocator passing available for future custom or bounded allocators.

## Status

- Inline-by-value struct returns are not implemented yet.
- These rules apply to non-opaque structs only. Opaque/capability types remain
  handles and return directly.

## Slice + string layout

`Slice<T>` and `string` are plain structs in the stdlib and are passed by value:

- `Slice<T>` layout: `{ ptr: *T, len: i32 }`
- `string` layout: `{ bytes: Slice<u8> }`

`string` values are views into UTF-8 byte storage; they do not imply ownership
in the ABI. Owned text is represented by `string::Text` (backed by `Vec<u8>`).
