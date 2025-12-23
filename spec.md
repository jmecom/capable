# Capable — Capability-Secure Systems Language (spec.md)

> **Status:** Draft v0.1 (implementation-guiding, not a formal standard)
> **Goal:** A small, practical systems language where **safe code has no ambient authority**.
> **Core idea:** If you didn’t receive a capability as a value, you can’t do the thing.

---

## 0. Project context

We want a language that makes it *normal* to write code like:

- Pure functions: `fn add(a: int, b: int) -> int` should not be able to touch the filesystem, network, time, process state, etc.
- OS access is explicit: filesystem/network/clock/etc must be passed in as capabilities (values).
- This is primarily to prevent **ambient authority in libraries** (supply-chain “surprises”), not to create a perfect in-process sandbox against malicious native code.
- The language should feel **simple like Go**: small surface area, straight-line readability, fast compilation.
- **No GC.** Deterministic, explicit resource management (linear/affine types planned after the first vertical slice).

We are targeting “Level A+B” enforcement:
- **A (Static)**: compiler ensures safe code cannot call privileged operations unless it has the right capability value.
- **B (Dynamic)**: the `sys` layer enforces scoping/attenuation (e.g., a `ReadFS` rooted at `./config` cannot escape to `../`).

We explicitly accept a boundary:
- Native **FFI / inline asm / raw syscalls** can bypass this. Therefore:
  - they are **unsafe-only**
  - they are **auditable**
  - we provide tooling modes like `--safe-only` that refuse unsafe deps.

---

## 1. Non-goals

v0.1 non-goals:
- Full resource safety (linear types) — planned for v0.2+
- Concurrency
- Generics beyond the minimum needed for `Result` (optional)
- Full package manager / registry (simple local builds first)
- Perfect security against malicious code in-process (use sandbox/WASI/process isolation for that in later milestones)

---

## 2. Design decisions (agreed)

### 2.1 Capability discipline (object-capability flavor)
- Authority flows only via values you already hold:
  - function parameters
  - return values
  - struct fields explicitly containing capabilities

### 2.2 No ambient authority in safe code
- No “global OS” object.
- No `std::fs::read_to_string("...")` without a filesystem capability value.
- Global initializers are pure (no calling into sys).

### 2.3 Safe vs Unsafe
- Safe code cannot:
  - declare FFI (`extern`)
  - use inline asm
  - issue raw syscalls
  - link arbitrary native libraries “for free”
- Unsafe code can, but must be explicit and visible:
  - `package unsafe` (or similar top-level marker)
  - `cap audit` flags unsafe transitively

### 2.4 Runtime role is small
- Not a VM; a small shim that:
  - constructs `System` (root capability) and passes it to `main`
  - implements `sys.*` operations
  - enforces attenuation/scoping checks

### 2.5 Implementation stance on Tree-sitter
- Tree-sitter is great for **editor tooling**, incremental parsing, highlighting.
- For the compiler, prefer a **hand-written recursive descent parser + Pratt expression parsing** for control and simplicity.

### 2.6 Code generation
- Prefer **Cranelift** for “fast enough, simpler than LLVM” native codegen.
- Keep the backend replaceable (C backend is a possible bootstrap alternative, but Cranelift is the primary recommendation).

### 2.7 libc boundary
- Default build should avoid “accidental libc as an escape hatch.”
- Either:
  - do direct syscalls in a tiny runtime layer, or
  - use a tiny audited C shim
- Linking libc / arbitrary native libs requires `package unsafe` (policy + tooling).

---

## 3. Language overview (v0.1)

### 3.1 Entry point
A Capable program starts at:

```capable
pub fn main(sys: System) -> i32
````

`System` is the root capability minted by the launcher/runtime.

### 3.2 Modules

Each file begins with:

```capable
module <name>
```

Exports use `pub`.

### 3.3 Types (minimal, v0.1)

* Primitives: `i32`, `i64`, `u32`, `bool`, `unit`
* `string` (owned UTF-8 string)
* `struct` (product types)
* `Result[T, E]` (either builtin generic or monomorphized later)

v0.1 avoids exposing raw pointers in safe code.

### 3.4 Control flow

* `let` bindings
* `if`
* `while`
* `return`
* `match` optional (if included, keep it minimal)

### 3.5 No global side effects

* Global constants allowed only if compile-time.
* No global initialization code that can do syscalls.

---

## 4. Capabilities (v0.1)

### 4.1 Capability types are opaque

Capabilities are values of opaque types defined in `sys.*`.

Properties:

* Passable by value
* Storable in structs
* Not constructible by user code
* No field access or pattern matching (unless the sys module explicitly provides it)

### 4.2 Root capability: `System`

`System` can mint attenuated sub-capabilities, e.g.:

* `sys.console(sys) -> Console`
* `sys.fs_read(sys, root: string) -> ReadFS`

### 4.3 Stdlib split

* `core`: pure utilities (no I/O)
* `sys`: privileged ops (all require capability values)

There is no backdoor “syscall” in `core`.

---

## 5. The minimal `sys` surface (v0.1)

### 5.1 Console

```capable
module sys.console

opaque struct Console

pub fn print(c: Console, s: string) -> unit
pub fn println(c: Console, s: string) -> unit
```

### 5.2 Read-only filesystem (scoped)

```capable
module sys.fs

opaque struct ReadFS
enum FsErr { NotFound, PermissionDenied, InvalidPath, IoError }

pub fn read_to_string(fs: ReadFS, path: string) -> Result[string, FsErr]
```

Semantics:

* `ReadFS` is scoped to a root directory set when minted by `System`.
* `path` must normalize such that it stays under the root.
* Attempted traversal (e.g., `../`) fails with `InvalidPath` or `PermissionDenied`.

### 5.3 System minting

```capable
module sys.system

opaque struct System

pub fn console(sys: System) -> sys.console.Console
pub fn fs_read(sys: System, root: string) -> sys.fs.ReadFS
```

Launcher policy:

* v0.1 can hardcode “console allowed, fs optional”
* later: manifest-driven permissions + prompting

---

## 6. Safe vs Unsafe (compiler rules)

### 6.1 Package safety marker

At the top of each file (or package config):

* `package safe` (default)
* `package unsafe`

Rules:

* `package safe`:

  * cannot contain `extern`
  * cannot contain inline asm
  * cannot declare raw syscalls
  * cannot link native libs other than the runtime
* `package unsafe`:

  * may use the above
  * is flagged by tooling and optionally forbidden in `--safe-only` builds

### 6.2 Unsafe “contagion” (tooling policy)

Tooling provides:

* `cap build --safe-only`: fails if any transitive dependency is unsafe
* `cap audit`: prints a tree of which modules/packages are unsafe and why

---

## 7. Minimal examples (acceptance tests)

### 7.1 Pure code cannot print

This should **not compile** (no Console cap in scope):

```capable
module app
use sys.console

pub fn main(sys: System) -> i32 {
  console.println(???, "nope")
  return 0
}
```

### 7.2 Printing works only with Console cap

```capable
module app
use sys.system
use sys.console

pub fn main(sys: System) -> i32 {
  let c = system.console(sys)
  console.println(c, "hello")
  return 0
}
```

### 7.3 File read works only with ReadFS

```capable
module app
use sys.system
use sys.fs
use sys.console

pub fn main(sys: System) -> i32 {
  let c = system.console(sys)
  let rfs = system.fs_read(sys, "./config")

  match fs.read_to_string(rfs, "app.txt") {
    Ok(s) => { console.println(c, s); return 0 }
    Err(e) => { console.println(c, "read failed"); return 1 }
  }
}
```

---

## 8. Compiler architecture

### 8.1 Pipeline stages

1. Lexer
2. Parser → AST
3. Name resolution
4. Type checking
5. Lowering → HIR (typed, desugared)
6. Lowering → IR for codegen (Cranelift)
7. Object emission
8. Linking

### 8.2 Key enforcement points

* **No ambient authority**: only `sys` modules can call runtime intrinsics; their APIs require capability args.
* **Opacity**: capability types are opaque, no constructors in userland.
* **Safe/unsafe**: parser/typechecker enforces restrictions; build tool enforces transitive policy.

### 8.3 Error reporting

Goal: readable compiler errors early.

* Include spans, show source snippets.
* Basic recovery is fine; correctness > IDE resilience for the compiler parser.

---

## 9. Runtime design (v0.1)

The runtime is a small library linked into the binary.

Responsibilities:

* start-up trampoline (calls `main(sys)`)
* internal representation of `System`, `Console`, `ReadFS`
* implement `sys.console.*` and `sys.fs.*`
* implement path normalization and root scoping checks for `ReadFS`

Suggested internal representation:

* `ReadFS` may store:

  * a root directory (string) or an opened directory handle (platform-dependent)
  * rights mask (read-only)
  * optional generation id if using a handle table

Note: For the simplest v0.1, keeping `ReadFS` as an owned struct with an internal root path string is acceptable, provided it remains opaque to user code.

---

## 10. Implementation milestones (step-by-step)

### Milestone 0 — Repo scaffolding & dev loop (1–2 days)

Deliverables:

* `capc` compiler CLI that can parse a file and print AST
* unit test harness
* snapshot tests for parse trees

Suggested Rust crates:

* CLI: `clap`
* diagnostics: `miette` + `thiserror`
* testing snapshots: `insta`

### Milestone 1 — Parser + typechecker for the minimal language (v0.1 core) (3–10 days)

Deliverables:

* lexer (identifiers, ints, strings, keywords)
* recursive descent parser + Pratt expressions
* AST + typed HIR for:

  * functions
  * let bindings
  * if/while/return
  * structs
  * string literals
  * Result + match (optional)

Suggested crates:

* lexer: hand-written or `logos`
* unicode/string handling: standard library (avoid overengineering)
* arenas: `bumpalo` (optional) or just owned AST nodes first

### Milestone 2 — `sys` intrinsics & capability “proof” (vertical slice) (3–10 days)

Goal: prove “no ambient authority” end-to-end.

Deliverables:

* builtin opaque types: `System`, `Console`, `ReadFS`
* implement `sys.system.console`, `sys.console.println`
* compile and run “hello world” requiring a `Console` cap
* compile-time failure if attempting to print without a `Console`

Implementation approach:

* treat `sys.*` functions as compiler-recognized intrinsics that lower to runtime calls.
* keep user-visible APIs in `sys` modules, but implement them in the runtime.

Crates:

* runtime printing: use platform APIs or libc behind the runtime boundary (OK for v0.1)
* if using Rust runtime: `std::io::stdout` is fine internally (runtime is trusted)

### Milestone 3 — Scoped filesystem capability (3–14 days)

Deliverables:

* implement `sys.system.fs_read(sys, root)`
* implement `sys.fs.read_to_string(ReadFS, path)` with attenuation checks
* path normalization rules and tests:

  * allow: `a/b.txt`
  * reject: `../secret`
  * reject: absolute paths
  * reject: path normalization escaping root

Crates:

* path normalization: implement carefully using `std::path` but do not rely solely on lexical normalization; enforce root containment.
* errors: `thiserror`
* tests: golden tests on path behavior

### Milestone 4 — Safe/unsafe boundary and “safe-only” builds (2–7 days)

Deliverables:

* `package safe|unsafe` marker parsing
* compiler errors if `extern` appears in safe code
* `capc --safe-only` rejects unsafe packages
* `cap audit` prints unsafe usage reasons

Crates:

* CLI: `clap`
* output formatting: `owo-colors` (optional)

### Milestone 5 — Cranelift codegen (5–20 days)

Deliverables:

* HIR→Cranelift lowering for the subset:

  * integers, bools
  * string constants
  * function calls
  * basic control flow
* link a native executable
* call runtime symbols for sys intrinsics

Crates:

* `cranelift-codegen`, `cranelift-module`, `cranelift-object`
* `target-lexicon`
* object writing/link: `object`, plus invoking system linker via `cc` or `std::process::Command`

### Milestone 6 — Developer ergonomics (later)

Deliverables:

* Tree-sitter grammar for Capable (editor highlighting)
* minimal LSP (hover/diagnostics)
* formatter (optional)
* package manager + manifest-based permissions (v0.2+)

Crates:

* tree-sitter: `tree-sitter`, `tree-sitter-cli` (grammar repo)
* LSP: `tower-lsp`

---

## 11. Recommended repository layout

```
capable/
  spec.md
  capc/                 # compiler (Rust)
    src/
  runtime/              # runtime shim (Rust or tiny C)
    src/
  stdlib/
    core/               # pure modules (source or baked in)
    sys/                # capability-gated API surface
  tests/
    programs/
      hello.cap
      fs_read.cap
      should_fail_no_console.cap
```

---

## 12. Testing strategy (strongly recommended)

* Snapshot tests for:

  * parser output
  * type errors (message + span)
* Golden tests for capability behavior:

  * compile success/failure conditions
* Runtime tests:

  * fs scoping normalization edge cases
* “No ambient authority” regression tests:

  * ensure no `core` module can do sys intrinsics

---

## 13. Security posture statement (for docs)

Capable provides:

* A strong **language-level guarantee**: safe code cannot perform privileged operations without explicit capabilities.
* Dynamic enforcement of capability scoping in the `sys` runtime layer.

Capable does not (by itself) provide:

* in-process sandboxing against arbitrary native/unsafe code

For untrusted plugins, use a sandbox target (planned):

* WASM/WASI backend or process isolation

---

## 14. Roadmap (beyond v0.1)

### v0.2 — No-GC + resource safety

* introduce linear/affine types for:

  * file handles, sockets, locks
  * heap allocations (`Box[T]!`)
* deterministic destruction / explicit `drop`
* richer `sys` (WriteFS, clock, rand, net)

### v0.3 — Concurrency + explicit scheduling capability

* `spawn` requires `Sched` cap
* message passing that moves ownership

### v0.4 — Packaging + permission manifests

* capability declarations for binaries
* `cap run` that constructs `System` accordingly
* auditing tools that show capability flow surface

---

## 15. Open questions (to resolve early)

1. Do we want capabilities to be copyable by default, or non-copy / “linear” by default?

   * v0.1: copyable is okay (simpler), but it makes authority spread easier.
   * v0.2+: linear caps become very attractive for auditability.

2. `Result[T,E]` generics:

   * builtin generic support for Result only (fast path)
   * or postpone match/result and use error codes initially

3. Runtime implementation language:

   * Rust runtime is fastest to iterate
   * tiny C shim may simplify syscall boundaries later

4. Path scoping:

   * lexical normalization vs realpath-based containment checks
   * consider TOCTOU concerns later (v0.1 is fine with lexical containment)

---

## 16. Summary

v0.1 proves the model with a vertical slice:

* printing requires a `Console` capability
* reading a file requires a scoped `ReadFS` capability
* safe code cannot acquire these “from nowhere”
* unsafe/FFI is quarantined and auditable

This sets the foundation for v0.2: no-GC systems programming with linear resource safety, while keeping the original goal intact:
**libraries do not get ambient authority.**

```

If you want, I can also generate:
- a minimal EBNF grammar for the v0.1 language slice, and
- a `CONTRIBUTING.md` with “compiler phases + invariants” so an agent can work in smaller, verifiable steps.
::contentReference[oaicite:0]{index=0}
```
