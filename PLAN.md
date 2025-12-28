# Capable Roadmap (Austral-Aligned)

This plan keeps Capable aligned with Austral’s goals while staying pragmatic about implementation order. Each phase ends with real examples and tests.

## Completed (Recently)

- Borrow‑lite locals with non‑escaping rules and error coverage.
- Capability hardening: root normalization, symlink escape guard, and tests.
- Linear control‑flow checks with attenuation/untrusted tests.
- Match exhaustiveness for `bool`, `Result`, and enums.
- Safe arithmetic with trap‑on‑overflow and modular helpers.
- Result helpers (`unwrap_or`, `unwrap_err_or`) with tests.
- Explicitness policy: shadowing rejected, unsigned ordering enforced.
- Docs and workflow: architecture/policy/samples, justfile tasks, tutorial updates.

## Priority 1: Backend Cleanup (Highest)

Goal: keep the compiler/backend simple and trustworthy so future features don’t compound complexity.

- Typed lowering bridge: codegen should consume typed HIR directly, no re‑inferring shapes.
- Shared type utilities: centralize numeric/orderable/unsigned checks to avoid drift.
- Clean error plumbing: consistent spans/messages across parse → typeck → lower → codegen.
- HIR simplification: fully resolved, no spans, no unresolved paths.
- Runtime handle conventions: unify tables + lifecycle patterns to prep for drop/close.

Deliverables:
- Typed lowering bridge complete; codegen does not re‑infer type shapes.
- Error messages consistently point at user code spans.
- Small cleanup PRs that reduce “special cases” in codegen.

## Phase 2: Core Discipline

Goal: make capability/linearity guarantees reliable without full borrow checking.

- Linear/affine rules are complete and consistent across control flow.
- Capability attenuation is enforced (consume `self` on narrowing APIs).
- Borrow‑lite: allow `&T` for locals with strict non‑escaping rules.
- Stdlib/runtime tightenings: ensure cap states are checked and canonicalization is consistent.

Deliverables:
- Tests for linear “must consume” on all paths (if/match/loop).
- Tests for borrow‑lite locals (read‑only, non‑escaping).
- At least two sample programs that stress capabilities and linear resources.

## Phase 3: Reliability and Predictability

Goal: make the language predictable under failure and in large programs.

- Exhaustive `match` checking for enums/Result.
- Safe arithmetic: explicit trap‑on‑overflow vs modular operations.
- Enforce explicitness policies (no implicit conversions, no implicit calls).
- Decide and enforce shadowing policy (prefer “no shadowing” for clarity).

Deliverables:
- Exhaustiveness tests with helpful diagnostics.
- Arithmetic tests covering overflow behavior.
- Lint/error surfaces for disallowed implicitness.

## Phase 4: Expressiveness Without Magic

Goal: add power without hidden control flow or inference.

- Typeclasses (bounded ad‑hoc polymorphism).
- Better collections/strings APIs for real programs.
- Error ergonomics (`Result` helpers like `map`, `map_err`, `and_then`).

Deliverables:
- Typeclass MVP with a small stdlib surface (e.g., `Eq`, `Show`).
- Revised sample programs that feel “natural” without macros or inference.

## Next Horizon

- Borrow‑lite polish: allow `&T` forwarding through helpers; tighten diagnostics.
- Capability ergonomics: `&self` methods where attenuation should not consume parents.
- Resource lifecycle: decide whether `drop()` stays a sink or add explicit close/free APIs.
- Runtime error clarity: structured error codes/messages for FS/alloc paths.
- Sample apps: config loader with validation, a log tailer, and a dir walker.

## Ongoing Non‑Goals

Keep these as invariants:
- No GC, no exceptions, no implicit conversions/calls, no macros, no reflection.
- No global state; no subtyping.
- No uninitialized variables.
- No first‑class async.
