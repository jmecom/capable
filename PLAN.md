# Capable Roadmap (Austral-Aligned)

This plan keeps Capable aligned with Austral’s goals while staying pragmatic about implementation order. Each phase ends with real examples and tests.

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

## Ongoing Non‑Goals

Keep these as invariants:
- No GC, no exceptions, no implicit conversions/calls, no macros, no reflection.
- No global state; no subtyping.
- No uninitialized variables.
- No first‑class async.
