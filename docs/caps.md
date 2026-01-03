# Capability Model Design

This repo implements a **capability-secure systems language** where **safe code has no ambient authority**.

The core idea: **authority is a value**. If code doesn’t receive a capability value, it cannot perform the corresponding privileged action (filesystem, network, console, process, etc.).

---

## Goals

- Prevent “ambient authority” in libraries: third-party safe code should not be able to touch the OS unless explicitly granted.
- Make authority flow visible in function signatures and call sites.
- Keep the model practical for systems work by drawing an explicit boundary: `unsafe` / FFI can bypass the model and must be auditable.

---

## Mental model

A capability is an **unforgeable token of permission** (think: keycard).

- `Console` capability → permission to print
- `ReadFS` capability → permission to read files (scoped/attenuated)
- `RootCap` capability → root authority that can mint sub-capabilities

No global “OS object” exists in safe code. Authority flows only via:
- function parameters
- return values
- struct fields that explicitly contain capabilities

---

## The three moving parts

### 1) Capability types (unforgeable tokens)
Capabilities are declared with the `capability` keyword in `sys.*` (capability types are always opaque and declare no fields):

- user code can hold/pass/store them
- user code cannot construct them (`Console{}` is illegal outside `sys.console`)
- user code cannot inspect internals (no field access)

This prevents forging authority “out of nowhere.”

### 2) Privileged APIs require the capability value
All privileged operations live under `sys.*` as methods on capability types:

- `Console.println(s: string)`
- `ReadFS.read_to_string(path: string)`

So if you don’t have a `Console`, you cannot call `println`. The compiler will reject it.

### 3) Capability kinds and attenuation rules

Capability types use the same move kinds as other structs:

- `capability struct` defaults to **affine** (move-only, drop OK).
- `linear capability struct` means **must be consumed** on all paths.
- `copy capability struct` means **unrestricted** (only use this for caps you truly want to duplicate).

Attenuation is enforced by method shape:

- Any method that returns a capability must take `self` by value.
- Any method that takes `&self` cannot return a capability.

Quick examples:

```cap
capability struct Dir
capability struct FileRead

impl Dir {
  pub fn open(self, name: string) -> FileRead { return () }
}
```

```cap
capability struct Dir
capability struct FileRead

impl Dir {
  pub fn open(self: &Dir, name: string) -> FileRead { return () } // error
}
```

Not every opaque handle is a capability. Use `capability` for authority-bearing tokens (filesystem, console, stdin); use `opaque` for unforgeable data handles (allocators, vectors). Slices are plain structs in the stdlib, but pointer types remain unsafe for user code.

### 3) Root authority comes from `RootCap`
The entrypoint receives a root capability:

```cap
pub fn main(rc: RootCap) -> i32
````

`RootCap` can mint attenuated capabilities:

* `rc.mint_console() -> Console`
* `rc.mint_readfs(root: string) -> ReadFS`

This is the only “source” of capability tokens in safe code.

---

## Enforcement model: A + B

We use two layers of enforcement.

### A) Static enforcement (compiler / typechecker)

**A enforces: “you can’t call privileged operations without the right capability value.”**

Mechanically:

* there is no `core` API that does I/O
* privileged functions exist only in `sys.*`
* those functions require capability arguments
* capability types are opaque and unconstructible in safe user code

Result:

* safe code cannot reach I/O without explicitly receiving a capability value
* authority flow is visible in function signatures and call sites

Example:

```cap
fn add(a: i32, b: i32) -> i32 { a + b } // cannot print/read/net: no caps in scope
```

### B) Dynamic enforcement (runtime / sys layer)

**B enforces: “even with a capability, you only get what it grants.”**

Capabilities can encode *attenuation* (scoping / rights). Example: `ReadFS` is not “the whole filesystem,” but “read-only access rooted at ./config”.

So `fs.read_to_string(path)` performs runtime checks:

* reject absolute paths
* normalize `.` / `..`
* ensure the resolved path stays under the capability’s root
* enforce rights (read-only vs write, etc.)

Result:

* code with a `ReadFS("./config")` cannot read `../secrets.txt`
* policies live in capabilities; sys calls must validate them

---

## Why this blocks ambient authority in libraries

In typical systems languages, the OS is “ambiently” available: any code can call `open()`, `connect()`, etc.

In this model, safe code can only do OS actions by calling `sys.*` functions, and those require capability arguments. Libraries can’t “decide” to read your disk unless you passed them a `ReadFS`. They can’t “phone home” unless you passed them a network capability.

Authority becomes explicit and reviewable.

---

## Safe vs Unsafe boundary

This model provides strong guarantees for **safe code** only.

* `unsafe` / FFI / inline asm can bypass `sys.*` and talk to the OS directly.
* Therefore:

  * `package safe` is the default
  * `package unsafe` is required for FFI/asm/raw syscalls
  * tooling supports `--safe-only` builds and `audit` output

Guarantee statement (what we actually promise):

> Any code that is transitively **safe** cannot perform privileged operations unless explicitly given the corresponding capability value.

---

## Practical tests (what we rely on)

Negative tests should not use placeholders like `???`.
Instead, they should fail due to missing capabilities or inability to forge them.

Examples:

1. Missing cap in scope:

* calling `c.println(...)` without any `c` bound from `rc.mint_console()` should fail.

2. Forging a capability:

* `let c = Console{}` should fail: “cannot construct opaque/capability type.”

These tests validate the unforgeability + explicit passing model.

---

## Summary

* **A (static)**: no capability value ⇒ cannot compile privileged calls
* **B (dynamic)**: capability value still enforces scope/rights at runtime
* Capability types are **opaque tokens** minted only from `RootCap`
* `unsafe` is the explicit escape hatch and must be auditable

```
::contentReference[oaicite:0]{index=0}
```
