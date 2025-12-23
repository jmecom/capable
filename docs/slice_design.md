# Safe Raw Buffers Without Raw Pointers

This document explains how Capable safe code can work with “raw buffers” (bytes, packets, file contents) **without** exposing raw pointers or requiring a borrow checker.

Goal: keep **capability security** meaningful by ensuring **safe code is memory-safe**, while still supporting systems-style byte manipulation.

---

## 1. Core idea

Safe code never performs pointer arithmetic or raw pointer dereference.

Instead, safe code manipulates raw data using:
- **Owning buffer types** (heap-backed, move-only)
- **Slice views** (read-only and mutable, bounds-checked)
- `sys.*` APIs that accept slices and perform OS/FFI pointer work internally

Unsafe code exists for FFI/asm/special cases, but is quarantined (`package unsafe`) and auditable.

---

## 2. Types

### 2.1 `Buffer` (owned, heap-backed)
An owning contiguous byte buffer. Conceptually similar to Rust `Vec<u8>` or Go `[]byte` when used as an owned value.

Properties:
- owns its allocation
- **move-only** (non-copyable) to prevent double-free patterns
- provides safe operations (push/resize/etc.)
- can be converted to slices for reading/writing

### 2.2 `Slice[T]` and `MutSlice[T]` (non-owning views)
Non-owning views into contiguous memory.

Properties:
- `Slice[T]` is read-only
- `MutSlice[T]` is mutable
- indexing is bounds-checked in safe code
- subslicing is bounds-checked
- safe code cannot access the underlying pointer or do arithmetic

Implementation detail:
- internally may be `{ ptr, len }`, but pointer is not exposed to safe code.

---

## 3. Minimal API surface (suggested v0.2)

### 3.1 Allocation / ownership
Allocation is explicit (Zig-like). Functions that allocate accept an allocator value.

```cap
opaque struct Alloc
opaque struct Buffer    // move-only owner

fn buffer_new(a: Alloc, initial_len: usize) -> Result[Buffer, AllocErr]
fn buffer_len(b: &Buffer) -> usize
fn buffer_push(b: &mut Buffer, x: u8) -> Result[unit, AllocErr]
fn buffer_free(a: Alloc, b: Buffer) -> unit         // consumes b
````

### 3.2 Views

```cap
opaque struct Slice[T]
opaque struct MutSlice[T]

fn buffer_as_slice(b: &Buffer) -> Slice[u8]
fn buffer_as_mut_slice(b: &mut Buffer) -> MutSlice[u8]

fn slice_len[T](s: Slice[T]) -> usize
fn slice_at[T](s: Slice[T], i: usize) -> &T                 // bounds-checked
fn slice_sub[T](s: Slice[T], start: usize, n: usize) -> Slice[T] // bounds-checked
```

Note: For `T` that are Copy, `slice_get` may return `Option[T]`.

---

## 4. What safe code looks like

### 4.1 Parsing bytes

Safe indexing gives “systems parsing” without unsafe pointer math:

```cap
fn parse_u16_be(buf: Slice[u8], off: usize) -> Result[u16, Err] {
  let b0 = buf.at(off)        // bounds checked
  let b1 = buf.at(off + 1)
  Ok(((b0 as u16) << 8) | (b1 as u16))
}
```

### 4.2 Reading a file into a buffer

`sys.fs` provides functions that return a `Buffer` (owned) and/or `string`:

```cap
fn read_all(a: Alloc, fs: ReadFS, path: string) -> Result[Buffer, FsErr]
```

Usage:

* safe code receives `Buffer`
* parses it via `Slice[u8]`
* frees it (explicitly or with `defer`)

---

## 5. I/O and syscalls: safe wrappers take slices

OS APIs typically want `(ptr, len)`.

In Capable:

* safe code supplies `Slice[u8]` / `MutSlice[u8]`
* `sys.*` converts internally and calls the OS

Example shapes:

```cap
sys.net.send(sock: Socket, data: Slice[u8]) -> Result[usize, Err]
sys.fs.write(file: File, data: Slice[u8]) -> Result[usize, Err]
```

Safe code never manipulates the raw pointers.

---

## 6. Avoiding a borrow checker

Capable does **not** implement a full borrow checker. Instead we keep safe operations non-UB by construction:

* safe code cannot do pointer arithmetic or unchecked deref
* slice indexing/subslicing is bounds-checked
* freeing consumes the owner (`Buffer` is move-only), reducing double-free risk

Remaining hazard without lifetimes: use-after-free by keeping a slice after freeing its `Buffer`.

Mitigations (recommended):

1. **Debug validation:** `Buffer` carries a generation cookie; slices carry `(id,cookie)`; deref checks cookie in debug builds.
2. **Simple restriction (v0.2):** slices derived from a `Buffer` may not escape the current function scope (easy rule).
3. Prefer **arenas** for complex lifetimes: allocate many objects, free once.

---

## 7. Security rationale

Capability security depends on memory safety for safe code.
If safe code could corrupt memory, it could forge or tamper with capability tokens and runtime policy tables.

Therefore:

* raw pointers and FFI are **unsafe-only**
* safe buffer work is done via `Buffer` + `Slice` APIs
* `sys.*` performs pointer work internally

This preserves the guarantee:

> Safe libraries cannot escalate authority without being passed explicit capabilities.
