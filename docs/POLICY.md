# Capable Policy (Contributor Guide)

This is a compact policy reference for language invariants and safety boundaries.

## Safety Boundary

- `package safe` is the default.
- `package unsafe` is required for:
  - raw pointers (`*T`)
  - `extern` functions

## Resource Model

- Most values are plain data and are unrestricted by default.
- `opaque struct` represents a resource/owner handle.
- `capability struct` represents an authority-bearing resource.
- Structs/enums that contain resource/capability fields become move-tracked by
  containment.

## Borrow‑Lite Rules

- `&T` is allowed in parameters and locals.
- Reference locals must be initialized from another local value.
- References cannot be stored in structs/enums or returned.
- References are read‑only and intentionally short-lived.

## Move / Linear Rules

- **Unrestricted**: freely copyable.
- **Affine**: move‑only; use‑after‑move is a type error.
- **Linear**: move‑only and must be consumed on all paths.
- Extracting an affine/linear field consumes the whole root local.
- In practice these rules are primarily for resources, capabilities, and values
  that contain them.

## Capabilities

- Capabilities are opaque (`capability struct`) and cannot be forged.
- Prefer three distinct API shapes in `sys.*`:
  - use operations
  - attenuation operations
  - child-handle operations
- Reusable use operations should borrow where possible.
- Attenuation operations consume the stronger capability.
- Child-handle operations may borrow when they return a fresh linear
  capability such as `FileRead` or `TcpConn`.
- Borrowed capability receivers must not return reusable capabilities.
- Runtime enforces root/relative path checks.

## No‑Implicitness

Keep these as invariants:
- No implicit conversions.
- No implicit function calls.
- No macros or reflection.
- No variable shadowing.

## Arithmetic Semantics

- Integer arithmetic traps on overflow.
- Division by zero traps.
- Modular arithmetic is explicit via `sys.math` helpers.

## Result Flow

Use `Result<T, E>` with a small number of intended forms:

- `?` to propagate an `Err` when the current function returns `Result`.
- `try let x = expr else { ... }` when the success value needs to be bound.
- `try expr else { ... }` or `try expr else err { ... }` for statement-style
  `Result<unit, E>` handling.
- `let PATTERN = expr else { ... }` for non-`Result` pattern matching.
- `match` for real enum branching or multi-arm recovery logic.

`Result` intentionally does not expose helper methods like `ok()` or
`unwrap_or()`. Destructuring should stay visible in the source.
