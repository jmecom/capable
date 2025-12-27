# Progress Log

This file summarizes major milestones added recently.

## Borrow‑Lite Locals
- Allowed `&T` locals with non‑escaping rules.
- Added strict errors for temp borrows and ref reassignments.
- Method calls on `&T` locals now work.
- Tests:
  - `tests/programs/should_pass_borrow_local.cap`
  - `tests/programs/should_fail_borrow_local_temp.cap`
  - `tests/programs/should_fail_borrow_local_move.cap`
  - `tests/programs/should_fail_borrow_local_assign.cap`

## Capability Hardening + Tests
- Runtime rejects missing root paths at mint time.
- Added root normalization tests in `runtime`.
- Added symlink escape test with fixtures.

## Linear + Attenuation Coverage
- Linear control‑flow tests for match/loop joins.
- Untrusted module attenuation tests (pass + fail).

## Docs + Workflow
- Added `docs/ARCHITECTURE.md`, `docs/POLICY.md`, `docs/SAMPLES.md`, `PLAN.md`.
- Added `justfile` tasks: `typecheck`, `run-tests`, `run-config`, `tree-sitter`.
- Updated `TUTORIAL.md` for `?`, implicit `unit`, and borrow‑lite rules.

## Exhaustive Match Checking (Phase 2)
- Enforced exhaustiveness for `bool`, `Result`, and enums.
- Added tests for non‑exhaustive and exhaustive cases.

## Safe Arithmetic (Phase 2)
- Integer arithmetic now traps on overflow.
- Division by zero traps.
- Added a runtime test that overflows an addition.
