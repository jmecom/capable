# Capable Architecture (Quick Map)

This is a compact guide for contributors. It explains how the compiler is structured and where to make changes.

## Pipeline Overview

```text
source (.cap)
   |
   v
lexer ──> parser ──> AST
                      |
                      v
                 typechecker
                      |
                      v
                     HIR
                      |
                      v
                   codegen
                      |
                      v
               Cranelift IR
                      |
                      v
                 object file
                      |
                      v
                 runtime + link
```

## Key Data Structures

- `capc/src/ast.rs`
  - Surface syntax nodes. Keeps spans and surface types (`Type`).
  - Parser produces AST.
- `capc/src/typeck`
  - `check.rs`: typechecking + move/linear rules.
  - `lower.rs`: lowers AST to HIR using typechecked info.
  - `mod.rs`: shared helpers (type kinds, method resolution).
- `capc/src/hir.rs`
  - Fully typed, resolved IR. Codegen consumes this.
- `capc/src/codegen`
  - Emits Cranelift IR from HIR.
- `runtime/src/lib.rs`
  - Capability enforcement + syscalls.

## Where to Change What

### Add syntax or a new expression
- Update lexer (`capc/src/lexer.rs`) if new tokens.
- Update parser (`capc/src/parser.rs`) to build AST.
- Update AST (`capc/src/ast.rs`) with new node(s).
- Update typechecker (`capc/src/typeck/check.rs`).
- Update lowering (`capc/src/typeck/lower.rs`).
- Update HIR/codegen if needed.
- Update tree-sitter grammar for editor support.

### Change type/move rules
- `capc/src/typeck/check.rs` is the single source for move/linear rules.
- `capc/src/typeck/mod.rs` holds kind computation.

### Capability enforcement
- Runtime tables and checks are in `runtime/src/lib.rs`.
- Stdlib capability APIs are in `stdlib/sys/*.cap`.

## Invariants (Keep These True)

- No implicit conversions or calls.
- Capabilities are opaque; authority must flow through values.
- Move/linear rules are enforced in `check.rs`.
- HIR is fully typed and is the only input to codegen.

## Useful References

- `TUTORIAL.md` for a quick language tour.
- `PLAN.md` for the current roadmap.
- `docs/POLICY.md` for safety and invariants.
- `docs/SAMPLES.md` for golden program outputs.
