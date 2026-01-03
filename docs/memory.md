# Memory Model

This document describes how Capable implements memory safety without a full
borrow checker, and how that shapes the stdlib and ABI.

## Goals
- Safe code is memory-safe, so capability security remains meaningful.
- Unsafe operations are explicit and auditable.
- Allocation is explicit (Zig-like) and testable.
- The core model is simple enough to keep the language small and predictable.

## Safe vs unsafe
Safe code cannot:
- dereference or do arithmetic on raw pointers
- call FFI or inline assembly
- access unchecked memcpy/memmove helpers

Unsafe code (`package unsafe`) may do all of the above. Tooling can audit and
reject unsafe dependencies (`--safe-only`, `audit`).

## Allocators are explicit
- APIs that allocate take an explicit `Alloc` handle (or are methods that already
  carry one).
- The runtime currently backs `Alloc` with libc malloc/free, but the ABI keeps
  allocator passing explicit for future custom allocators.

## Owned vs borrowed data
Capable separates owned buffers from borrowed views.

### Owned
- `Vec<T>` is an owned, growable buffer.
- `Text` is an owned UTF-8 buffer backed by `Vec<u8>`.
- Owned types are move-only to reduce double-free patterns.

### Borrowed
- `Slice<T>` and `MutSlice<T>` are non-owning views.
- Safe indexing and slicing are bounds-checked.

Because Capable does not have a full lifetime system, safe code is restricted
from letting slices escape:
- Safe non-stdlib modules may not return `Slice<T>` / `MutSlice<T>`.
- Safe non-stdlib modules may not store slices inside structs or enums.

This keeps slice lifetimes local until we introduce a richer lifetime model.

## Unsafe pointer surface
The stdlib exposes low-level pointer helpers in `sys::unsafe_ptr` for unsafe
code. The surface is intentionally small and explicit:
- `ptr_is_null` only checks nullness; it does not imply ownership or validity.
- `memcpy` and `memmove` are available for unsafe code and lowered in codegen.

## ABI and lowering notes
- Opaque structs lower to handles; field access is special-cased in codegen.
- `sret` and `ResultOut` conventions are documented.
- Slice/string layout and alloc-passing conventions are stable and documented.

## Design trade-offs
- We avoid a borrow checker to keep the language small and predictable.
- We still preserve memory safety in safe code by restricting raw pointers and
  slice escapes.
- The stdlib is trusted to use unsafe internals where needed; user code remains
  safe by default.

## Roadmap
- Introduce a lightweight lifetime model or scoped borrows to relax slice
  escape restrictions without losing safety.
- Expand allocator ergonomics without losing explicit allocation.
- Keep the unsafe surface small and auditable.
