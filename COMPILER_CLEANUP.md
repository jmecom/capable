# Compiler Cleanup

This pass is complete.

The goal was to remove the worst architectural accidents in `capc/` without
turning the compiler into a rewrite project.

## What Landed

- stable expression identity and an explicit desugar pass are already in place
- lowering now has real lexical scopes instead of fake scope helpers
- parser, checker, monomorphization support, and codegen helpers are split by
  concern instead of living in a few giant files
- multi-file diagnostics now attach the correct source file for imported-module
  parse and type-check failures, and the driver has the plumbing needed for
  codegen diagnostics to do the same
- runtime-backed stdlib functions now come from one shared runtime-binding
  registry instead of a stringly boolean table in one place and a separate
  signature registry in another
- the immediate MIR question is settled for now: do not add a full MIR/CFG
  layer yet; if codegen needs another normalization step, start with a smaller
  control-flow-normalized HIR pass first

## Final Status

- [x] Stable expression identity and explicit desugaring
- [x] Real lowering scopes
- [x] Match / `try` control-flow cleanup
- [x] `typeck` split by concern
- [x] Parser split by syntactic domain
- [x] Monomorphization helper split
- [x] Codegen helper split
- [x] Multi-file diagnostics for imported-module compiler errors
- [x] Shared runtime interface registry
- [x] Decision on MIR vs narrower normalization

## Result

The compiler is still direct, but the worst remaining problems are now
deliberate tradeoffs instead of accidental ones:

- diagnostics are no longer effectively entry-file-only
- runtime-backed stdlib functions have one source of truth
- the next control-flow step is bounded and explicit instead of “probably add
  MIR someday”

## Verification

- `PATH="$HOME/.cargo/bin:$PATH" cargo test -p capc`
