# Memory Safety Design

This document records the memory-safety decisions needed to keep Capable’s **capability model meaningful**.

Capable’s primary security goal is: **safe libraries do not have ambient authority**.
However, that goal collapses if “safe” code can corrupt memory, because memory corruption can be used to forge/tamper with capability tokens and the runtime’s policy state.

So we adopt a simple rule:

> **Safe code must be memory-safe.**
> Memory-unsafe operations exist, but they are **`unsafe`** and auditable.

We intentionally do **not** implement a full Rust-style borrow checker.

---

## 1. Why memory safety matters for capabilities

The capability model relies on:
- capability types being **unforgeable** in safe code (`capability struct`, no constructors)
- privileged operations happening only through `sys.*`, which enforces attenuation checks

If safe code can do out-of-bounds writes / UAF / double-free, it may:
- forge a `Console` / `ReadFS` value by overwriting its bytes
- corrupt the runtime’s capability table (e.g., change a `ReadFS` root from `./config` to `/`)
- divert control flow into privileged runtime routines

Therefore: **capability security requires memory safety for safe code**.

---

## 2. Core decision: raw pointers are `unsafe`

### Safe code
Safe code cannot:
- dereference raw pointers
- do pointer arithmetic
- cast between unrelated pointer types
- call memcpy/memmove-style primitives with unchecked sizes
- perform FFI calls

### Unsafe code
`package unsafe` may:
- use raw pointers (`*T`, `*const T`, `*mut T`)
- do pointer arithmetic and casts
- call FFI / inline asm / raw syscalls

Current implementation:
- raw pointer type syntax is `*T`
- any pointer types in `package safe` are rejected at typecheck
- `sys.mem` exposes `malloc`/`free` as `extern` in the unsafe stdlib

Tooling supports:
- `--safe-only` builds that reject unsafe dependencies
- `audit` that reports unsafe modules/packages transitively

---

## 3. Safe memory primitives (no borrow checker)

We want explicit, Zig-like control without GC, but without letting safe libs corrupt memory.

### 3.1 Bounds-checked slices/arrays in safe code
- Safe indexing is bounds-checked (at least in debug / safe mode).
- No “escape hatch” from slice to raw pointer in safe code.

### 3.2 Explicit allocators (optional dependency, not a security boundary)
Capable may expose explicit allocation via an `Alloc` handle:
- `alloc(Alloc, n) -> Owned[T]` or similar
- `free(Alloc, Owned[T])` consumes the owner

This makes allocation explicit, testable, and controllable, but allocation itself is not treated as OS authority.

### 3.3 Move-only ownership for heap allocations (minimal static help)
To avoid double-free and many UAF patterns without a borrow checker:
- “owning” heap handles are **move-only** (non-copyable)
- freeing consumes the owner (cannot free twice)

Example conceptually:
- `Owned[T]` (move-only)
- `drop(Owned[T])` or `free(Alloc, Owned[T])` consumes it

### 3.4 Prefer arenas for complex allocation patterns
Provide `Arena` allocation for common compiler/library workloads:
- allocate many objects
- free once at end
- avoids per-object free complexity and many UAF hazards

---

## 4. Capability representation hardening (defense in depth)

Even with safe memory rules, we harden capability tokens against accidental misuse:

- capabilities are represented as opaque handles like `(id, cookie)` (e.g., 64-bit)
- runtime stores a table: `id -> {cookie, policy}`
- every `sys.*` call validates:
  - `id` exists
  - cookie matches (stale/forged values rejected)
  - policy allows the operation (attenuation check)

This is not a substitute for memory safety (memory corruption can still break invariants), but it reduces damage from bugs and makes failures fail-closed.

---

## 5. Resulting guarantees

### If all transitive dependencies are `safe`
- Safe code cannot perform memory-unsafe operations.
- Safe code cannot forge or tamper with capabilities.
- Safe code cannot perform privileged OS actions without explicit capability values.

### If any dependency is `unsafe`
- It may bypass memory safety and/or capability discipline via raw pointers/FFI.
- Tooling must surface this clearly (`audit`, `--safe-only`).

---

## 6. Roadmap implications

- v0.1: no pointers exposed in safe code (capability model proof first)
- v0.2: introduce safe slices + explicit allocators + move-only owning handles + arenas
- v0.3+: expand unsafe/FFI support with strict auditing and safe-only enforcement modes

---

## Summary

To keep Capable’s capability security real for third-party libraries:
- **safe code must be memory-safe**
- raw pointers and FFI are **unsafe-only**
- safe code uses bounds-checked slices + move-only owners + arenas
- runtime validates capability handles + policies on each `sys.*` call
