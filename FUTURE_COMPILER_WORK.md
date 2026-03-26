# Future Compiler Work

These items are intentionally deferred. They are real architectural projects,
not cleanup chores.

## High-Risk Work

- Add a real MIR / CFG layer.
  Current decision: do not do this yet. If codegen needs another simplification
  step, first try a smaller control-flow-normalized HIR pass that lowers
  `match`, `try`, `defer`, and loop exits into a flatter form without adding a
  whole new compiler IR family.

- Change the typecheck / lowering contract.
  Current decision: do not rewrite this boundary yet. The compiler is stable
  enough now that the next version of this work should only happen if a
  normalized-HIR pass or incremental compilation effort makes the current
  side-table contract too expensive to keep.

- Replace the current build / link pipeline.
  Current decision: do not tackle this in the cleanup pass. The compiler still
  shells out through `cargo` and `rustc` and writes a small Rust stub at link
  time. That is architecturally awkward, but it is isolated and working. It
  should become its own project if cross-compilation, packaging, reproducible
  builds, or compile-time performance make it worth paying down.

## Trigger Conditions

Re-open the work above only if one of these becomes true:

- codegen complexity starts growing faster than localized helper extraction can
  control
- diagnostics or optimization work need an explicit CFG-level representation
- incremental compilation or separate compilation becomes a real goal
- the current shell-driven build pipeline becomes a meaningful product problem

## Order

If this work is reopened, the preferred order is:

1. Try control-flow-normalized HIR before full MIR.
2. Revisit the typecheck / lowering contract only after that.
3. Tackle the build / link pipeline as a separate delivery project.
