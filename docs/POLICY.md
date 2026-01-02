# Capable Policy (Contributor Guide)

This is a compact policy reference for language invariants and safety boundaries.

## Safety Boundary

- `package safe` is the default.
- `package unsafe` is required for:
  - raw pointers (`*T`)
  - `extern` functions

## Borrow‑Lite Rules

- `&T` is allowed in parameters and locals.
- Reference locals must be initialized from another local value.
- References cannot be stored in structs/enums or returned.
- References are read‑only: they are only valid for `&T` parameters.

## Move / Linear Rules

- **Unrestricted**: freely copyable.
- **Affine**: move‑only; use‑after‑move is a type error.
- **Linear**: move‑only and must be consumed on all paths.
- Extracting an affine/linear field consumes the whole root local.

## Capabilities

- Capabilities are opaque (`capability struct`) and cannot be forged.
- Attenuation APIs consume the stronger capability.
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

## Result Helpers

- `Result.is_ok()` returns `true` if `Ok`, `false` if `Err`.
- `Result.is_err()` returns `true` if `Err`, `false` if `Ok`.
- `Result.ok()` returns the `Ok` value (traps if `Err`).
- `Result.err()` returns the `Err` value (traps if `Ok`).
- `Result.unwrap_or(default)` returns `Ok` value or the default.
- `Result.unwrap_err_or(default)` returns `Err` value or the default.
