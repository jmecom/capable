# Capable — Capability-Secure Systems Language (spec.md)

> **Status:** implemented “vertical slice” (toy compiler + minimal runtime). This doc describes **what exists today** and the **next broad goals** (methods + typed lowering + predictable scoping).

Capable is a small systems language that makes **ambient authority awkward** and **capability-passing the default**.

If you didn’t receive a capability value, you can’t do the thing.

---

## 0. What exists today (current reality)

### Implemented language slice

* **Functions** with parameters + return type
* **Statements:** `let`, assignment, expression statement, `if`, `while`, `return`, `match`
* **Expressions:** literals (`i32`, `u8`, `bool`, `string`, `unit`), binary ops, unary ops, calls, `match` as an expression
* **Modules + imports:** `module ...` + `use ...` (aliasing via last path segment)
* **Enums:** currently treated as **unit variants only** (variants lower to `i32` tags)

### Implemented runtime / target

* Native codegen via **Cranelift** → object file
* Links against a **minimal runtime** (`capable_rt_*`) that implements privileged operations.
* “Capability types” in userland are **opaque handles** (currently lowered as `i64`), enforced dynamically by the runtime.

### Big current limitation

* Codegen does **not** consume typed HIR yet. It “lowers types” again and relies on ad-hoc `ValueRepr` shapes (`Single`, `Pair`, `Result`, `Unit`).
* User-defined `struct` literals are currently not codegen’d (unsupported).

---

## 1. Core idea

Capable is object-capability flavored:

* Authority flows only through values:

  * function args
  * returns
  * stored fields (once structs land)
* Safe code has **no global OS object**, no ambient filesystem/network/time.
* Privileged effects live in `sys.*` and require capability values.

This is meant to prevent “library surprises” and make authority explicit. It is not a perfect in-process sandbox against malicious native code.

---

## 2. Goals (near-term + broad)

### Near-term goals (stabilize the compiler)

1. **One invariant for `unit`** across parsing/typecheck/codegen/ABI:

   * `unit` is zero-sized and never “an `i32 0` pretending to be unit”.
2. **Correct lexical scoping**:

   * `let` is block-scoped
   * assignment mutates an existing binding in an outer scope
3. **Typed lowering bridge**:

   * typechecker produces a typed artifact used by codegen (stop re-inferring shapes).

### Broad goals (next features)

1. **Methods** (ergonomics for capability APIs)

   * `rc.mint_console()` instead of `sys.system.console(rc)`
   * `console.println("hi")` instead of `sys.console.println(console, "hi")`
2. **Postfix operator pipeline** (parser foundation)

   * `.` methods/member access
   * later: `?`, indexing `[]`, etc.
3. **Unified name resolution**

   * the same path rules for types/functions/variants/methods

---

## 3. Non-goals (for now)

* Perfect sandboxing against malicious unsafe/native code (use process isolation/WASI later)
* Concurrency
* Generics beyond `Result[T, E]` (and even that may be “special” initially)
* Full package manager / registry
* Linear/affine types (planned later; would be a big win for authority control)

---

## 4. Surface language (v0 slice)

### 4.1 Entry point

Current compiler treats `main` specially (symbol `capable_main`). The runtime passes the root capability.

Example (intended direction):

```capable
fn main(rc: RootCap) {
  let console = rc.mint_console();
  console.println("hello");
}
```

Current implementation also supports the “sys function” style:

```capable
fn main(sys: sys.system.System) {
  let c = sys.system.console(sys);
  sys.console.println(c, "hello");
}
```

(Methods are a goal; `sys.*` functions exist today.)

### 4.2 Types (current)

* Primitives: `i32`, `u32`, `u8`, `bool`, `unit`
* `string`
* `Result[T, E]` (compiler-lowered tagged union)
* Opaque capability types (currently a set of known `sys.*` types lowered to `Handle`)
* `ptr` exists in the implementation (used for runtime memory helpers); it’s effectively “unsafe-adjacent”.

### 4.3 Control flow

* `if (cond) { ... } else { ... }`
* `while (cond) { ... }`
* `match expr { ... }` (statement or expression)

### 4.4 `match` patterns (current)

* `_` wildcard
* literal patterns (`true`, `false`, ints, u8)
* enum variants (unit variants)
* `Ok(x)` / `Err(e)` for `Result` (special-cased today)

---

## 5. Capabilities and the `sys` surface

### 5.1 Capability representation

* Capability values are **opaque handles** in the language.
* The runtime validates and enforces attenuation/scoping.
* User code cannot construct capability values from raw integers in safe mode (policy goal; enforcement hardens as “safe/unsafe” lands).

### 5.2 Root capability

A root capability can mint attenuated capabilities, e.g. console + filesystem.

This is the user-facing model:

```capable
let console = rc.mint_console();
let fs = rc.mint_readfs("./here");
```

And the current implementation model is equivalent but spelled as `sys.*` functions.

### 5.3 Minimal sys APIs (current shape)

The runtime currently exposes a bunch of functions; these are the “core” ones conceptually:

* Console:

  * `println(console, string) -> unit`
  * `print(console, string) -> unit`
* Read-only filesystem (scoped):

  * `read_to_string(readfs, path) -> Result[string, i32]` (error details are still evolving)

Dynamic enforcement rule:

* `ReadFS(root="./here")` must reject paths that escape root (e.g. `../etc/passwd`).

---

## 6. ABI + lowering rules (current implementation constraints)

This matters because Cranelift calls are currently flattened manually.

### 6.1 Strings

`string` is lowered as a pair:

* `(ptr, len)` where:

  * `ptr` is pointer-sized
  * `len` is `i64`

String literals are currently emitted as readonly data blobs in the object file.

### 6.2 Result

`Result[Ok, Err]` is lowered as:

* `tag: i8` (0 = Ok, 1 = Err)
* followed by flattened Ok payload
* followed by flattened Err payload

The runtime also uses “out-parameter” variants for some results (`ResultOut`, `ResultString`) to keep ABI stable and avoid returning large aggregates. The compiler reconstructs a logical `Result` from those parts.

### 6.3 Unit

`unit` must flatten to **zero values** (this is an active correctness fix to keep consistent across everything).

---

## 7. Safe vs Unsafe (policy direction)

This is not fully enforced end-to-end yet, but it’s a design goal:

* Safe code should not be able to:

  * call arbitrary extern functions
  * issue syscalls directly
  * fabricate capabilities from integers/pointers
* Unsafe should be explicit and auditable:

  * file/module marker (or package-level)
  * tooling flag like `--safe-only` that rejects unsafe dependencies

---

## 8. Compiler architecture (current + target shape)

### 8.1 Current pipeline (today)

1. Lexer
2. Parser → AST
3. Minimal resolution utilities (`use` alias map, stdlib index, enum index)
4. Typechecking exists, but codegen still re-lowers types
5. Codegen → Cranelift → object emission

### 8.2 Target pipeline (what we’re moving toward)

1. Lexer
2. Parser → AST
3. Resolver (one set of rules for types/values/variants)
4. Typechecker → **Typed HIR**
5. Lowering (desugar: methods, `?`, etc.) → **canonical calls**
6. Codegen consumes HIR + resolution results (no guessing)

This is the bridge that makes methods straightforward.

---

## 9. Methods (next major feature) — how we’ll make them not painful

### 9.1 Parser requirement: postfix loop

Stop treating `.` as only “path separator”. Instead:

* parse primary/prefix
* repeatedly apply postfix ops:

  * `(` call
  * `.` member/method
  * later: `?`, `[]`, etc.

### 9.2 Typechecker requirement: method resolution + desugaring

A method call must be resolved using the receiver type, then rewritten into a normal call with a fully qualified symbol.

Example rewrite:

* `console.println("hi")`
* becomes `sys.console.println(console, "hi")` (or whatever canonical symbol)

Resolution happens in typechecking (or immediately after), not in codegen.

### 9.3 Codegen requirement: resolved callee info

Codegen should never “figure out which function you meant”.
It should receive:

* the resolved symbol
* its ABI signature
* the already-typed argument list

---

## 10. Minimal acceptance tests (should keep passing)

### 10.1 No ambient authority

This should not compile (no cap):

```capable
fn f() {
  sys.console.println(?, "nope");
}
```

### 10.2 Scoped filesystem escape is blocked

```capable
fn main(rc: RootCap) {
  let console = rc.mint_console();
  let fs = rc.mint_readfs("./here");

  match fs.read_to_string("../etc/passwd") {
    Ok(_)  => console.println("BUG: escaped sandbox"),
    Err(_) => console.println("blocked"),
  }
}
```

---

## 11. Roadmap (practical ordering)

1. Lock down `unit` invariant end-to-end
2. Typechecker scoping via a scope stack
3. Typed artifact from typechecker → codegen consumes it
4. Parser postfix loop (`.`)
5. Typechecker method resolution + desugaring
6. Expand sys surface + tighten safe/unsafe policy

---

If you want this as a literal patch-style rewrite (so you can drop it into your repo), tell me your current “real” names for:

* root capability type (`RootCap` vs `sys.system.System`)
* whether `module` declarations are mandatory in v0
  …and I’ll align the document to match your current syntax exactly.
