# Capable Tutorial

Capable is a small, capability-secure systems language. Authority is a value: if
you did not receive a capability, you cannot perform that action. This is in contrast
to most languages. Normally, any piece of code in your program, whether that be from
a third-party library or yourself, has full permissions. In Capable, code only
get the capabilities you pass it, so authority is explicit, auditable, and narrow
by default.

This tutorial is a cohesive walk-through of the language as it exists today.
It focuses on how to write real programs, how the capability model works, and
how memory is managed.

## 1) Hello, console

```cap
package safe
module hello
use sys::system

pub fn main(rc: RootCap) -> i32 {
  let c = rc.mint_console()
  c.println("hello")
  return 0
}
```

`RootCap` is the root authority passed to `main`. It can mint narrower
capabilities (console, filesystem, network, allocators).

## 2) Language basics

```cap
package safe
module basics

pub fn add(a: i32, b: i32) -> i32 {
  return a + b
}

pub fn main(rc: RootCap) -> i32 {
  let x = 1
  let y: i32 = 2
  if (x < y) {
    return add(x, y)
  } else {
    return 0
  }
}
```

Key syntax:
- Statements: `let`, assignment, `if`, `while`, `for`, `match`, `return`, `defer`.
- Expressions: literals, calls, binary ops, unary ops, method calls.
- Modules + imports: `module ...` and `use ...` (alias by last path segment).
- `for { ... }` is an infinite loop; `for i in a..b` is a range loop.
- Integer arithmetic traps on overflow.
- Variable shadowing is not allowed.

## 3) Control flow and pattern matching

```cap
package safe
module match_demo

pub fn main(rc: RootCap) -> i32 {
  let flag = true
  match flag {
    true => { return 1 }
    false => { return 0 }
  }
}
```

Matches must be exhaustive; use `_` to cover the rest. `if let` is a
single-arm match:

```cap
if let Ok(x) = make() {
  return x
} else {
  return 0
}
```

## 4) Structs, enums, methods

```cap
package safe
module types

struct Pair { left: i32, right: i32 }

enum Color { Red, Green, Blue }

impl Pair {
  pub fn sum(self) -> i32 { return self.left + self.right }
  pub fn peek(self: &Pair) -> i32 { return self.left }
}
```

- Structs and enums are nominal types.
- Methods are defined in `impl` blocks and lower to `Type__method` in codegen.
- Method receivers can be `self` (move) or `self: &T` (borrow-lite, read-only).

## 5) Results and error flow

Capable uses `Result<T, E>` for recoverable errors, similar to Rust.

```cap
package safe
module results

fn parse() -> Result<i32, i32> {
  return Ok(7)
}

fn use_value() -> Result<i32, i32> {
  let v = parse()?
  return Ok(v + 1)
}
```

Other helpers:

```cap
let v = make().unwrap_or(0)
let e = make().unwrap_err_or(0)
```

## 6) Capabilities and attenuation

Capabilities live in `sys.*` and are declared with `capability` (opaque, no
public fields, no user construction). You only get them from `RootCap`.
They are just structs, but special ones: the compiler treats capability
types as authority tokens, and safe code cannot forge or construct them.
This works because capability structs have no public fields or constructors,
and the compiler rejects any attempt to synthesize them outside the stdlib.
In other words: if you did not receive a capability value, there is no safe
way to manufacture one.

```cap
package safe
module read_config
use sys::system
use sys::fs

pub fn main(rc: RootCap) -> i32 {
  let fs = rc.mint_readfs("./config")
  let alloc = rc.mint_alloc_default()
  match fs.read_to_string(alloc, "app.txt") {
    Ok(s) => { rc.mint_console().println(s); return 0 }
    Err(_) => { return 1 }
  }
}
```

The stdlib is the only safe way to access privileged operations like the
filesystem or network. In `package safe`, you cannot call raw syscalls or FFI.
That means if a function does not receive a `ReadFS` (or `Net`, etc.), it simply
cannot touch those resources. This keeps permissions explicit and local.

If a package is marked `unsafe`, it can bypass memory safety and call raw FFI,
which breaks the capability model entirely. Unsafe code is therefore a huge
risk: it can forge or corrupt capability values, violate attenuation, or reach
privileged operations directly. Treat unsafe dependencies as highly trusted
code and use auditing/`--safe-only` to keep the boundary tight.

Attenuation is one-way: methods that return capabilities must take `self` by
value, so you give up the more powerful capability when you derive a narrower
one. This is enforced by the compiler.

## 7) Kinds: copy, affine, linear

Types can declare how they move:

```cap
capability struct Token           // affine by default (move-only)
copy capability struct RootCap    // unrestricted (copyable)
linear capability struct FileRead // must be consumed
```

Kinds:
- **Copy**: reusable.
- **Affine**: move-only, dropping is allowed.
- **Linear**: move-only and must be consumed on all paths.

## 8) Borrow-lite references: `&T`

Capable has a minimal borrow system for read-only access. The goal is to make
non-consuming reads ergonomic without a full borrow checker.

```cap
impl Thing {
  pub fn ping(self: &Thing) -> i32 { return 1 }
}

pub fn twice(c: &Thing) -> i32 {
  let a = c.ping()
  let b = c.ping()
  return a + b
}
```

Rules:
- `&T` is allowed on parameters and locals.
- Reference locals must be initialized from another local value.
- References cannot be stored in structs/enums or returned.
- References are read-only.

This keeps the language simple without a full borrow checker. It also keeps
lifetimes simple: a borrow is only valid within the current scope, so you never
have to reason about aliasing across function boundaries.

Borrow-lite is intentionally conservative. If you need shared ownership across
functions, pass the value by move (and return it), or design your API to do the
read inside the callee.

## 9) Memory model

Capable has explicit memory management. Owned heap types must be freed.
Non-owning views must not outlive their backing storage.

Owned types:
- `Vec<T>` (heap-backed)
- `Text` (owned UTF-8 bytes, backed by `Vec<u8>`)

Views:
- `string` (non-owning view of bytes)
- `Slice<T>`, `MutSlice<T>` (non-owning views)

Safe code cannot return or store slices in structs/enums, which keeps slice
lifetimes local until a full lifetime model exists.

### Allocators

Allocation is explicit. Functions that allocate accept an `Alloc` handle:

```cap
let alloc = rc.mint_alloc_default()
let v = alloc.vec_u8_new()
...
alloc.vec_u8_free(v)
```

Use `defer` to simplify cleanup.

## 10) Strings: `string` vs `Text`

`string` is a view. `Text` is owned.

```cap
fn build_greeting(alloc: Alloc) -> Result<string, buffer::AllocErr> {
  let s = "hello"
  let _bytes = s.as_slice()
  let _sub = s.slice_range(0, 5)?

  let t = alloc.text_new()
  defer t.free(alloc)
  t.push_str("hello")?
  t.push_byte(' ')?
  t.append("text")?
  let out = t.to_string()?
  return Ok(out)
}
```

Helpers:
- `string.split`, `split_once`, `trim_*`, `contains`, `index_of_*`.
- `string.concat(alloc, other)` creates a new owned string view.
- `Text.slice_range` returns a `string` view into its buffer.

## 11) Slices and indexing

Slices are bounds-checked in safe code. Indexing out of bounds traps.
If you need a checked version, use `slice_range` or `byte_at_checked`.

```cap
fn use_tail(s: string) -> Result<unit, buffer::SliceErr> {
  let buf = s.as_slice()
  let b0 = buf.at(0) // traps if out of bounds
  let tail = buf.slice_range(1, 3)?
  let _b1 = tail.at(0)
  return Ok(())
}
```

## 12) Defer (scope-based cleanup)

`defer` schedules a call to run when the current scope exits (LIFO order):

```cap
let c = rc.mint_console()
let alloc = rc.mint_alloc_default()
let v = alloc.vec_u8_new()

// ensure we free on all paths
 defer alloc.vec_u8_free(v)
```

Deferred expressions must be calls; arguments are evaluated at the defer site.

## 13) Safe vs unsafe

`package safe` is default. Raw pointers and extern calls require
`package unsafe`.

```cap
package unsafe
module pointers

pub fn main(rc: RootCap) -> i32 {
  let alloc = rc.mint_alloc_default()
  let p = alloc.malloc(16)
  alloc.free(p)
  return 0
}
```

Unsafe is auditable: `--safe-only` rejects unsafe dependencies, and `audit`
reports unsafe packages.

## 14) Putting it together: a small parser

```cap
enum ParseErr { MissingEq, OutOfRange, Oom }

fn parse_key_value(line: string, alloc: Alloc) -> Result<string, ParseErr> {
  let eq = match (line.index_of_byte('=')) {
    Ok(i) => { i }
    Err(_) => { return Err(ParseErr::MissingEq) }
  }
  let key = match (line.slice_range(0, eq)) {
    Ok(v) => { v }
    Err(_) => { return Err(ParseErr::OutOfRange) }
  }
  let val = match (line.slice_range(eq + 1, line.len())) {
    Ok(v) => { v }
    Err(_) => { return Err(ParseErr::OutOfRange) }
  }

  let t = alloc.text_new()
  defer t.free(alloc)
  match (t.push_str(key)) {
    Ok(_) => { }
    Err(_) => { return Err(ParseErr::Oom) }
  }
  match (t.push_byte('=')) {
    Ok(_) => { }
    Err(_) => { return Err(ParseErr::Oom) }
  }
  match (t.push_str(val)) {
    Ok(_) => { }
    Err(_) => { return Err(ParseErr::Oom) }
  }
  match (t.to_string()) {
    Ok(out) => { return Ok(out) }
    Err(_) => { return Err(ParseErr::Oom) }
  }
}
```

This pattern (find delimiter, slice views, optionally allocate) is the common
way to parse without raw pointers.

---

## Where to go next

- Read `docs/memory.md` and `docs/ABI.md` for deeper internals.
- Explore `examples/` for real programs.
- Use `defer` aggressively to keep ownership clear.

Capable is intentionally small. The goal is to make capability-based security
and explicit memory ownership the default, not an advanced feature.
