# Capable in 15 Minutes

Capable is a small capability-secure systems language. The main idea: authority is a value. If you didn't receive a capability, you can't do the thing.

This tutorial is a quick tour of the current language slice and the capability model.

## 1) Hello, console

```cap
module hello
use sys::system

pub fn main(rc: RootCap) -> i32 {
  let c = rc.mint_console()
  c.println("hello")
  return 0
}
```

`RootCap` is the root authority passed to `main`. It can mint narrower capabilities (console, filesystem, etc.).

## 2) Basic syntax

```cap
module basics

pub fn add(a: i32, b: i32) -> i32 {
  return a + b
}

pub fn main() -> i32 {
  let x = 1
  let y: i32 = 2
  if (x < y) {
    return add(x, y)
  } else {
    return 0
  }
}
```

- Statements: `let`, assignment, `if`, `while`, `return`, `match`.
- Expressions: literals, calls, binary ops, unary ops, method calls.
- Modules + imports: `module ...` and `use ...` (aliases by last path segment).
- If a function returns `unit`, you can omit the `-> unit` annotation.
- Integer arithmetic traps on overflow.

## 3) Structs and enums

```cap
module types

struct Pair { left: i32, right: i32 }

enum Color { Red, Green, Blue }
```

Structs and enums are nominal types. Enums are currently unit variants only.

## 4) Methods

Methods are defined in `impl` blocks and lower to `Type__method` at compile time.

```cap
module methods

struct Pair { left: i32, right: i32 }

impl Pair {
  pub fn sum(self) -> i32 { return self.left + self.right }
  pub fn add(self, x: i32) -> i32 { return self.sum() + x }
  pub fn peek(self: &Pair) -> i32 { return self.left }
}
```

Method receivers can be `self` (move) or `self: &T` (borrow‑lite, read‑only).

## 5) Results, match, and `?`

```cap
module results

pub fn main() -> i32 {
  let ok: Result[i32, i32] = Ok(10)
  match ok {
    Ok(x) => { return x }
    Err(e) => { return 0 }
  }
}
```

`Result[T, E]` is the only generic type today and is special-cased by the compiler.

Inside a function that returns `Result`, you can use `?` to unwrap or return early:

```cap
module results_try

fn read_value() -> Result[i32, i32] {
  return Ok(7)
}

fn use_value() -> Result[i32, i32] {
  let v = read_value()?
  return Ok(v + 1)
}
```

You can also unwrap with defaults:

```cap
let v = make().unwrap_or(0)
let e = make().unwrap_err_or(0)
```

Matches must be exhaustive; use `_` to cover the rest:

```cap
match flag {
  true => { }
  false => { }
}
```

## 6) Capabilities and attenuation

Capabilities live in `sys.*` and are opaque. You can only get them from `RootCap`.

```cap
module read_config
use sys::system
use sys::fs

pub fn main(rc: RootCap) -> i32 {
  let fs = rc.mint_filesystem("./config")
  let dir = fs.root_dir()
  let file = dir.open_read("app.txt")

  match file.read_to_string() {
    Ok(s) => { rc.mint_console().println(s); return 0 }
    Err(e) => { return 1 }
  }
}
```

This is attenuation: each step narrows authority. There is no safe API to widen back.

## 7) Opaque, copy, affine, linear

Structs can declare their kind:

```cap
opaque struct Token           // affine by default (move-only)
copy opaque struct RootCap    // unrestricted (copyable)
linear opaque struct FileRead // must be consumed
```

Kinds:

- **Unrestricted** (copy): can be reused freely.
- **Affine** (default for opaque): move-only, dropping is OK.
- **Linear**: move-only and must be consumed on all paths.

## 8) Moves and use-after-move

```cap
module moves

opaque struct Token

pub fn main() -> i32 {
  let t = Token{}
  let u = t
  let v = t  // error: use of moved value
  return 0
}
```

Affine and linear values cannot be used after move. If you move in one branch, it's moved after the join.

## 9) Linear must be consumed

```cap
module linear

linear opaque struct Ticket

pub fn main() -> i32 {
  let t = Ticket{}
  drop(t)   // consumes t
  return 0
}
```

Linear values must be consumed along every path. `drop(x)` is a built-in sink that consumes the value.

## 10) Borrow-lite: &T parameters

There is a small borrow feature for read-only access in function parameters and locals.

```cap
module borrow

opaque struct Cap

impl Cap {
  pub fn ping(self: &Cap) -> i32 { return 1 }
}

pub fn twice(c: &Cap) -> i32 {
  let a = c.ping()
  let b = c.ping()
  return a + b
}
```

Rules:

- `&T` is allowed on parameters and locals.
- Reference locals must be initialized from another local value.
- References cannot be stored in structs, enums, or returned.
- References are read-only: they can only satisfy `&T` parameters.
- Passing a value to `&T` implicitly borrows it.

This avoids a full borrow checker while making non-consuming observers ergonomic.

## 11) Safety boundary

`package safe` is default. Raw pointers and extern calls require `package unsafe`.

```cap
package unsafe
module ffi

extern fn some_ffi(x: i32) -> i32
```

## 12) Raw pointers and unsafe

Raw pointers are available as `*T`, but **only** in `package unsafe`.

```cap
package unsafe
module pointers

pub fn main(rc: RootCap) -> i32 {
  let alloc = rc.mint_alloc_default()
  let ptr: *u8 = alloc.malloc(16)
  alloc.free(ptr)
  return 0
}
```

There is no borrow checker for pointers. Use them only inside `package unsafe`.

## 13) What exists today (quick list)

- Methods, modules, enums, match, while, if
- Opaque capability handles in `sys.*`
- Linear/affine checking with control-flow joins
- Borrow-lite `&T` parameters

---

That should be enough to read and write small Capable programs, and understand how attenuation and linearity fit together.
