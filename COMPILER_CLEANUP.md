# Compiler Cleanup Plan

This document is a focused cleanup plan for `capc/`.

The compiler is not junk. The overall pass structure is reasonable, the code is
mostly direct, and the test suite is catching real regressions. But the
implementation has reached the point where adding more language features will
keep making the compiler denser and more accidental unless we clean up the
phase boundaries.

This plan is intentionally pragmatic. It is not a rewrite-from-scratch plan.

## Progress

- [x] Phase 1: Stable expression identity
- [x] Phase 2: Explicit desugar pass
- [x] Phase 3: Split type checking by responsibility
- [x] Phase 4: Split codegen emission
- [x] Phase 5: Centralize the CLI/compiler pipeline

### Completed So Far

- Added stable `ExprId` tracking to AST expressions and switched typed-expression
  tables from `Span` keys to `ExprId`.
- Added a dedicated `desugar` pass and moved parser-side lowering of
  `let ... else`, `try`, and borrowed `for` sugar out of `parser.rs`.
- Removed lowering's type-check fallback path and the `allow_type_fallback`
  escape hatch. Missing type metadata is now an internal compiler bug again.
- Kept parser snapshots stable by redacting internal expression IDs from
  snapshot output instead of baking those IDs into the public AST snapshots.
- Split `typeck` support logic into focused modules:
  - `resolve.rs`
  - `kinds.rs`
  - `safety.rs`
  - `moveck.rs`
  - `infer.rs`
  - `patterns.rs`
  - `type_params.rs`
- Shrunk `typeck/mod.rs` down to orchestration and data definitions and moved
  the high-churn helper logic out of `check.rs`.
- Split codegen emission by concern:
  - `codegen/emit.rs` remains the coordinator
  - `codegen/emit/match_lowering.rs` owns match-expression and match-statement lowering
  - `codegen/emit/runtime.rs` owns runtime-wrapper and unsafe-pointer emission
- Extended match-expression lowering to support `Result`-shaped values instead
  of only `unit` and single scalars.
- Added a dedicated `driver.rs` pipeline module that owns:
  - entry loading
  - stdlib/user module graph loading
  - safe-only enforcement
  - type checking
  - object build and executable link steps
- Removed the duplicated parse/load/check/build orchestration from `main.rs`
  and centralized tool resolution for `cargo`/`rustc`.
- Tightened stdlib handling:
  - runtime-backed stdlib stubs are recognized explicitly
  - their fake source bodies are no longer type-checked or lowered as real code
  - helper stdlib modules are now checked on the same pipeline as user code
- Fixed several previously hidden stdlib issues that surfaced once stdlib was
  type-checked consistently (`sys.option`, `sys.path`, `sys.string`, `sys.vec`).

## Goals

- Make compiler phases easier to reason about.
- Remove brittle implementation techniques that cause accidental regressions.
- Shrink the blast radius of language changes.
- Keep the language behavior stable while improving internal structure.

## Non-Goals

- Rewriting the compiler from scratch.
- Changing the language surface as part of cleanup.
- Expanding traits, generics, or remote capability support during cleanup.

## Current Assessment

The main issues are:

1. Typed expressions are keyed by `Span`, not stable node identity.
2. The parser performs semantic desugaring and now also injects hidden resource
   management.
3. Type checking and lowering leak into each other.
4. A few files are now too large to evolve safely.
5. Some semantic logic is duplicated across phases.
6. The CLI/compiler driver pipeline is repetitive.

The most important concrete example is the span-keyed type table. We already hit
this while implementing borrowed `for` iteration: synthetic expressions that
shared spans collided in the type table and produced wrong lowering behavior.

## Priority Order

Do these in order:

1. Replace span-keyed typing with stable expression identity.
2. Introduce an explicit desugar pass after parsing.
3. Split `typeck` into smaller, cleaner submodules.
4. Split `codegen/emit.rs` into focused emission modules.
5. Centralize the CLI/compiler pipeline.

## Phase 1: Stable Expression Identity

This is the highest-value cleanup.

### Problem

Today expression typing is recorded as:

- `TypeTable { expr_types: HashMap<Span, Ty> }`

That is brittle because:

- synthetic expressions can share spans
- different expressions can accidentally collide
- later phases have to depend on exact span construction discipline

This is the wrong abstraction.

### Plan

- Introduce `ExprId` and `PatternId` or a typed AST node identity equivalent.
- Assign IDs during parsing or in a dedicated AST annotation pass.
- Change the type recorder to key on `ExprId` instead of `Span`.
- Make lowering consume typed expression metadata by ID.
- Remove the need for span-based type lookups entirely.

### Follow-on Cleanup

- Delete `allow_type_fallback` in lowering.
- Delete the fallback path that re-runs type inference during lowering.
- Make missing typed-expression data a hard internal compiler bug.

### Success Criteria

- No compiler phase relies on `Span` as expression identity.
- Synthetic desugaring can freely create nodes without worrying about span
  collisions.
- Lowering does not call back into expression checking to recover types.

## Phase 2: Add an Explicit Desugar Pass

### Problem

The parser currently does more than parse syntax. It also lowers:

- `let ... else`
- `try let`
- `try expr else`
- borrowed `for` iteration

That is workable, but it means the parser now owns semantic rewrites and hidden
implementation details like synthetic bindings and hidden `defer free()`.

### Plan

- Keep parsing purely syntactic.
- Represent the high-level constructs directly in AST first.
- Add a `desugar` pass after parse and before type checking.
- Move all syntax sugar lowering there.

### What Belongs in Desugaring

- `let ... else`
- `try let`
- `try expr else`
- borrowed `for item in vec`
- indexed `for i, item in vec`

### Why This Helps

- parser gets simpler
- lowering logic is centralized
- desugaring becomes independently testable
- future sugar features stop bloating `parser.rs`

### Success Criteria

- `parser.rs` only parses concrete syntax into AST nodes.
- synthetic names and hidden cleanup are created in the desugar pass.
- parser tests and desugar tests are separate.

## Phase 3: Split Type Checking by Responsibility

### Problem

`typeck/check.rs` and `typeck/mod.rs` are carrying too much mixed
responsibility:

- type resolution
- package safety validation
- move checking
- expression typing
- statement typing
- pattern binding
- impl desugaring support
- assorted helper logic

This is manageable now, but it is not clean.

### Plan

Refactor `typeck` into something closer to:

- `typeck/mod.rs`
  - public entry points only
- `typeck/types.rs`
  - `Ty`, builtins, type helpers
- `typeck/resolve.rs`
  - path/type/trait resolution helpers
- `typeck/safety.rs`
  - safe package validation and import safety rules
- `typeck/moveck.rs`
  - move state, branch merge rules, linear consumption checks
- `typeck/patterns.rs`
  - pattern binding and pattern typing
- `typeck/expr.rs`
  - expression checking
- `typeck/stmt.rs`
  - statement/block checking

### Immediate Refactors

- Pull duplicated helper logic into shared helpers.
- Stop duplicating path-base detection helpers in checking and lowering.
- Move enum type-argument inference helpers into one dedicated place.

### Success Criteria

- `typeck/mod.rs` is mostly orchestration and data definitions.
- expression and statement logic are no longer in one multi-thousand-line file.
- move-checking rules are locally understandable.

## Phase 4: Split Codegen Emission

### Problem

`codegen/emit.rs` is now the biggest file in the compiler and handles too many
distinct concerns:

- statement emission
- expression emission
- control-flow lowering
- match/result lowering
- local storage
- arithmetic traps
- defer handling

### Plan

Refactor codegen into focused files, for example:

- `codegen/emit_expr.rs`
- `codegen/emit_stmt.rs`
- `codegen/emit_control.rs`
- `codegen/emit_match.rs`
- `codegen/emit_locals.rs`
- `codegen/emit_defer.rs`

The exact split matters less than separating concerns.

### Success Criteria

- `emit.rs` becomes a thin coordinator or disappears.
- result/match lowering is isolated.
- local storage and defer handling are isolated.
- control-flow bugs no longer require editing one huge file.

## Phase 5: Centralize the Driver Pipeline

### Problem

`main.rs` repeats parse/load/check/build orchestration across commands.

That makes small behavior changes annoying and increases the chance that
commands drift.

### Plan

- Introduce a shared pipeline API in the library, for example:
  - parse entry
  - validate module path
  - load stdlib/user modules
  - enforce safe-only if requested
  - type-check
  - build object
  - link/run
- Make CLI commands thin wrappers over this pipeline.

### Success Criteria

- `main.rs` becomes mostly CLI argument handling.
- parse/check/build/run all share the same pipeline functions.
- error decoration is more consistent across commands.

## Cross-Cutting Rules

While doing this cleanup:

- Do not rewrite semantics casually.
- Keep tests green at each phase.
- Add targeted regression tests for every internal cleanup that changes a code
  path.
- Prefer extracting shared helpers before changing behavior.
- Avoid introducing new syntax/features during compiler cleanup.

## Recommended Sequence of Work

### Step 1

Add stable `ExprId` support and convert typed expression recording to use it.

### Step 2

Move current parser-side sugar lowering into a dedicated desugar pass.

### Step 3

Split `typeck/check.rs` and move resolution helpers out of the giant files.

### Step 4

Split `codegen/emit.rs`.

### Step 5

Clean up the CLI pipeline.

## What Not to Do

- Do not start with file splitting alone. Splitting files without fixing
  span-keyed typing and phase coupling will mostly create more files with the
  same design problems.
- Do not add more syntax sugar before the desugar pass exists.
- Do not rewrite the compiler in a new architecture unless the current one
  proves fundamentally unworkable. It has not.

## End State

If this cleanup succeeds, the compiler should look like this:

- parser parses syntax only
- desugar rewrites sugar only
- type checking owns semantic validation only
- lowering consumes typed nodes without re-inferring them
- codegen emits from HIR without giant monolithic files
- CLI commands share one pipeline

That is enough cleanup to keep evolving the language without the compiler
turning into a pile of accidental invariants.
