# Safe Bytes Without Raw Pointers

This document explains how Capable safe code can work with raw bytes (packets, file contents) **without** exposing raw pointers or requiring a borrow checker.

Goal: keep **capability security** meaningful by ensuring **safe code is memory-safe**, while still supporting systems-style byte manipulation.

---

## 1. Core idea

Safe code never performs pointer arithmetic or raw pointer dereference.

Instead, safe code manipulates raw data using:
- **Owning vectors** (heap-backed, move-only)
- **Slice views** (read-only and mutable, bounds-checked)
- `sys.*` APIs that accept slices and perform OS/FFI pointer work internally

Unsafe code exists for FFI/asm/special cases, but is quarantined (`package unsafe`) and auditable.

---

## 2. Types

### 2.1 `Vec<u8>` (owned, heap-backed)
An owning contiguous byte buffer. Conceptually similar to Rust `Vec<u8>` or Go `[]byte` when used as an owned value.

Properties:
- owns its allocation
- **move-only** (non-copyable) to prevent double-free patterns
- provides safe operations (push/extend/etc.)
- can be converted to slices for reading/writing

### 2.2 `Text` (owned UTF-8, heap-backed)
An owning, growable UTF-8 string builder backed by `Vec<u8>`.
Conceptually similar to Rust `String` and Go `strings.Builder`.

### 2.3 `Slice[T]` and `MutSlice[T]` (non-owning views)
Non-owning views into contiguous memory.

Properties:
- `Slice[T]` is read-only
- `MutSlice[T]` is mutable
- indexing is bounds-checked in safe code
- subslicing is bounds-checked
- safe code cannot access the underlying pointer or do arithmetic

Implementation detail:
- implemented as `{ ptr, len }` in the stdlib, but pointer types remain unsafe for user code.

---

## 3. Minimal API surface (suggested v0.2)

### 3.1 Allocation / ownership
Allocation is explicit (Zig-like). Functions that allocate accept an allocator value.

```cap
opaque struct Alloc
opaque struct Vec[T]     // move-only owner

impl Alloc {
  fn vec_u8_new(self) -> Vec<u8>
  fn vec_u8_with_capacity(self, cap: i32) -> Result<Vec<u8>, AllocErr>
  fn vec_u8_free(self, v: Vec<u8>) -> unit
}
````

### 3.2 Views

```cap
struct Slice[T] { ptr: *T, len: i32 }
struct MutSlice[T] { ptr: *T, len: i32 }

impl Vec<u8> {
  fn as_slice(self) -> Slice<u8>
}

impl Slice<u8> {
  fn len(self) -> i32
  fn at(self, i: i32) -> u8                 // bounds-checked
  fn slice(self, start: i32, len: i32) -> Result<Slice<u8>, SliceErr>
}
```

Note: For `T` that are Copy, `slice_get` may return `Option[T]`.

---

## 4. What safe code looks like

### 4.1 Parsing bytes

Safe indexing gives “systems parsing” without unsafe pointer math:

```cap
fn parse_u16_be(buf: Slice<u8>, off: i32) -> Result<u16, Err> {
  let b0 = buf.at(off)        // bounds checked
  let b1 = buf.at(off + 1)
  Ok(((b0 as u16) << 8) | (b1 as u16))
}
```

### 4.2 Reading a file into bytes

`sys.fs` provides methods that return owned bytes (`Vec<u8>`) and/or `string`:

```cap
fn ReadFS.read_bytes(self, alloc: Alloc, path: string) -> Result<Vec<u8>, FsErr>
```

Usage:

* safe code receives `Vec<u8>`
* parses it via `Slice<u8>`
* frees it (explicitly or with `defer`)

---

## 5. I/O and syscalls: safe wrappers take slices

OS APIs typically want `(ptr, len)`.

In Capable:

* safe code supplies `Slice<u8>` / `MutSlice<u8>`
* `sys.*` converts internally and calls the OS

Example shapes:

```cap
Socket.send(data: Slice<u8>) -> Result<usize, Err>
File.write(data: Slice<u8>) -> Result<usize, Err>
```

Safe code never manipulates the raw pointers.

---

## 6. Avoiding a borrow checker

Capable does **not** implement a full borrow checker. Instead we keep safe operations non-UB by construction:

* safe code cannot do pointer arithmetic or unchecked deref
* slice indexing/subslicing is bounds-checked
* freeing consumes the owner (`Vec<u8>` is move-only), reducing double-free risk

Remaining hazard without lifetimes: use-after-free by keeping a slice after freeing its `Vec<u8>`.

Mitigations (recommended):

1. **Debug validation:** `Vec<u8>` carries a generation cookie; slices carry `(id,cookie)`; deref checks cookie in debug builds.
2. **Simple restriction (v0.2):** slices derived from a `Vec<u8>` may not escape the current function scope (easy rule).
3. Prefer **arenas** for complex lifetimes: allocate many objects, free once.

Current rule (implemented):
- Safe non-stdlib modules may not return `Slice<T>` / `MutSlice<T>` values or
  embed them in structs/enums. This keeps view lifetimes local until a richer
  lifetime system exists.

---

## 7. Security rationale

Capability security depends on memory safety for safe code.
If safe code could corrupt memory, it could forge or tamper with capability tokens and runtime policy tables.

Therefore:

* raw pointers and FFI are **unsafe-only**
* safe byte work is done via `Vec<u8>` + `Slice` APIs
* `sys.*` performs pointer work internally

This preserves the guarantee:

> Safe libraries cannot escalate authority without being passed explicit capabilities.
