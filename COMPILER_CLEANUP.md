# Compiler Cleanup Plan

This pass is complete.

The goal was not a rewrite. The goal was to remove the worst structural
accidents that had built up in `capc/` while keeping language behavior stable.

## Outcomes

- lowering now has real lexical scopes instead of fake `push_scope` /
  `pop_scope` placeholders
- match / `try` control-flow handling is more uniform across type checking and
  codegen, including loop-aware `try let ... else { continue }` cases
- `typeck/check.rs` is split by concern
- `parser.rs` is split by syntactic domain
- monomorphization helper logic is split out of the main pass
- runtime intrinsic registration is grouped by domain instead of one large file
- normal compiler paths no longer rely on the most obvious `expect(...)` /
  `unreachable!()` traps in lowering, parser expression parsing, and loop
  codegen

## Final Status

- [x] First cleanup pass: stable expression identity, desugar pass, first
      `typeck` split, first codegen split, shared driver pipeline
- [x] Milestone 1: real lowering scopes
- [x] Milestone 2: unify match and `try` control-flow handling
- [x] Milestone 3: split `typeck/check.rs`
- [x] Milestone 4: split `codegen/emit.rs` further
- [x] Milestone 5: split `parser.rs`
- [x] Milestone 6: split `typeck/monomorphize.rs` and `codegen/intrinsics.rs`
- [x] Milestone 7: remove remaining internal panics on normal compiler paths

## What Landed

### Lowering

- `capc/src/typeck/lower.rs`
  - lowering locals now live in real scoped stacks
  - synthetic bindings and ordinary locals share the same scope machinery
  - path-based method fallback no longer uses unchecked `expect(...)`

### Type Checking

- `capc/src/typeck/check.rs`
- `capc/src/typeck/check/stmt.rs`
- `capc/src/typeck/check/match_check.rs`
- `capc/src/typeck/check/calls.rs`
- `capc/src/typeck/check/type_params.rs`

The root checker is now a coordinator. Statement checking, match checking,
call/method-call checking, and generic substitution logic are separated.

### Parsing

- `capc/src/parser.rs`
- `capc/src/parser/items.rs`
- `capc/src/parser/stmts.rs`
- `capc/src/parser/exprs.rs`
- `capc/src/parser/patterns.rs`
- `capc/src/parser/types.rs`

The root parser now owns shared state and common helpers. Items, statements,
expressions, patterns, and types live in separate modules.

### Codegen

- `capc/src/codegen/emit.rs`
- `capc/src/codegen/emit/defer.rs`
- `capc/src/codegen/emit/match_lowering.rs`
- `capc/src/codegen/emit/runtime.rs`
- `capc/src/codegen/emit/arith.rs`

`emit.rs` is still the largest single file in the compiler, but the helper
domains that caused the most accidental coupling are now extracted:
defer handling, match lowering, runtime-wrapper lowering, and arithmetic /
trap helpers.

### Monomorphization

- `capc/src/typeck/monomorphize.rs`
- `capc/src/typeck/monomorphize/support.rs`

Support types and substitution / mangling helpers are split out of the main
monomorphization pass.

### Intrinsics

- `capc/src/codegen/intrinsics.rs`
- `capc/src/codegen/intrinsics/io.rs`
- `capc/src/codegen/intrinsics/memory.rs`

The runtime intrinsic registry is now grouped by domain instead of one large
table.

## Result

The compiler is still direct, but it is less brittle:

- fewer giant coordination files
- fewer phase-boundary accidents
- fewer panic-style assumptions on ordinary compile paths
- clearer places to change parser, checker, lowering, monomorphization, or
  runtime-intrinsic behavior without touching unrelated logic

## Verification

- `PATH="$HOME/.cargo/bin:$PATH" cargo test -p capc`
