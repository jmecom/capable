# Plan: HashMap demo limitations

This plan targets the limitations called out in `examples/hashmap_demo/hashmap_demo.cap`.
Priority is to ship a real modulo operator and then simplify the demo to use it.

## Goals

- Add a `%` (modulo) operator for integer types.
- Update the hash-map demo to use `%` and remove the hand-rolled `mod_i32`.
- Track the remaining open limitations with concrete next steps.

## Milestones

1) Modulo operator support
- Lexer: add `%` token.
- Parser: recognize `%` as a binary operator with the same precedence as `*`/`/`.
- AST/HIR: add `BinaryOp::Mod`.
- Type checker: allow `%` for matching integer types (same rules as `+ - * /`).
- Codegen: emit signed/unsigned remainder with division-by-zero trap.
- Tests: add parser/typecheck/codegen coverage for `%`.

2) HashMap demo refresh
- Replace `mod_i32` with `%`.
- Remove the “NO MODULO OPERATOR” limitation from the demo header.
- Keep other limitations (traits, function pointers, Vec<T> restrictions, copy semantics) as-is.

3) Next limitations (future work)
- Traits/interfaces: define a minimal trait system or ad-hoc interface for hashable keys.
- Generic storage: allow `Vec<T>` for user-defined structs (or add `Vec<Entry>` support).
- Function pointers / first-class functions: enable passing hash functions to generic maps.
- Copy semantics and mutability: consider non-copy structs with explicit mutation tracking.

## Out of scope

- Full generics over all types.
- A production-quality hash map (rehashing, tombstone compaction, probing strategies).
