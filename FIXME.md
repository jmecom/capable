# FIXME

Commits on this branch (main..HEAD), in order:

- 57fc54c Add break and continue statements
- a22c056 Fix nested match codegen panic
- 978f14b Use break in sort example
- d9e0aa3 Add for loop support with range syntax
- a6fe98f Add string comparison operators == and !=
- 9b746d3 Add index syntax [] and change generics to <>
- aa09a93 check tests properly
- 7555204 Change default target name from capable
- f64de33 Remove outdated plan file
- 4bd4f97 remove outdated progress file
- 529b9ed move docs
- 4c6e29e Simple ok+err handlers
- f0b3345 Add Vec indexing syntax and fix for-loop range parsing
- c167d11 Move Result<T, E> enum from compiler builtin to stdlib
- 2f28e94 [WIP] Move Result methods from compiler special-case to stdlib
- 993e805 Fix some stuff
- 7416ad8 Fix loop linearity, indexing constraints, and call conv
- 3f2aca3 Add docs and TODO list
- fc93b87 Add never type and Vec<T> specializations
- d925d5b Add top-level defer statement
- 4763187 Make defer scope-based
- a29d59b Improve net listener and string helpers
- c53e84d Add if let statement sugar
- 15276b0 Add for {} infinite loop sugar
- 4ee669e Add char literals and string helpers
- 48b63f0 stdlib: flesh out string/vec helpers
- 1df8699 codegen: lower struct returns via sret/result-out
- 29effac docs: describe ABI lowering for struct returns

Fixes applied from review:

- Allow user-defined `string` type names now that `string` is userland; update reserved-name test to `i32`.
- Support sret lowering in runtime wrapper emission to avoid ABI mismatches.
- Use return lowering in `try` error path to avoid returning wrong signature.
- Initialize zero values for non-opaque structs to avoid null deref when building Result payloads.

Current known issues/workarounds:

- Result<*T> in stdlib generics triggers a typechecker mismatch (expected Ptr(Path("T", [])) vs Ptr(Param("T"))), so Vec internals avoid Result<*T> and use null checks instead.
