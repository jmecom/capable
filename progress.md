# Project Progress (Capable)

This is a living status report for future-me. It summarizes what exists, where it
lives, and what’s missing vs the spec. Keep it updated as milestones complete.

## Repo Layout (where code lives)

- Compiler: `capc/`
  - Lexer: `capc/src/lexer.rs` (logos-based)
  - Parser: `capc/src/parser.rs` (recursive descent + Pratt)
  - AST: `capc/src/ast.rs`
  - Typechecker: `capc/src/typeck.rs`
  - Codegen: `capc/src/codegen.rs` (Cranelift, runtime intrinsics)
  - Loader/module graph: `capc/src/loader.rs`
  - Errors: `capc/src/error.rs` (ParseError/TypeError + spans)
- Runtime: `runtime/`
  - Runtime ABI + capability tables: `runtime/src/lib.rs`
- Stdlib (source): `stdlib/`
  - Sys modules (capability APIs + helpers): `stdlib/sys/*.cap`
- Tests/programs: `tests/programs/*.cap`
  - Parser snapshots: `capc/tests/parser.rs`
  - Typecheck tests: `capc/tests/typecheck.rs`
  - Run tests: `capc/tests/run.rs`
  - CLI tests: `capc/tests/cli.rs`
- Editor tooling:
  - Tree-sitter grammar: `tree-sitter-capable/`
  - VS Code extension: `vscode/`
  - Docs: `docs/editor.md`
- Docs: `spec.md`, `docs/` (design notes)
- Examples: `examples/` (extern demo)

## Milestones (spec alignment)

- M0: Parser/AST/typecheck + minimal stdlib -> done
- M1: Codegen for basic programs + module loader -> done
- M2: Runtime + sys capability plumbing -> done
- M3: ReadFS scoping (attenuation) -> done
- M4: Safe/unsafe boundary + tooling -> done

## Language Features (current)

Done:
- Modules, `use`, `pub`, `package safe/unsafe`
- Functions, `let`, assignment, `if`, `while`, `return`, `match`
- Structs, opaque structs, enums, struct literals
- `Result[T, E]` as a built-in generic (monomorphized at use sites)
- Primitive types: `i32`, `u32`, `u8`, `bool`, `string`, `unit`
- Pointer types in unsafe code (`*T`)
- `extern` functions in unsafe packages only

Missing / limited:
- `i64` not implemented yet (spec lists it)
- Real generics (currently only `Result` + hardcoded types)
- No user-defined methods; only free functions
- No consts/globals beyond literals
- No lifetimes/borrow checking/linearity

## Stdlib (current)

Sys capabilities (opaque types, minted by `System`):
- `sys.system.System` (root)
- `sys.console.Console`
- `sys.fs.ReadFS` + `FsErr`

Helpers (stdlib source):
- `sys.string`: `len`, `byte_at`, `as_slice`, `bytes`, `split_whitespace`, `split`
- `sys.bytes`: `len`, `at`, `is_whitespace`
- `sys.buffer`: `Alloc`, `Buffer`, `Slice`, `MutSlice` + buffer ops
- `sys.vec`: monomorphic `VecU8`, `VecI32`, `VecString`
- `sys.args`, `sys.io`, `sys.mem` (unsafe/pointer helpers)

Notes:
- Collections are monomorphic (no generic `Vec<T>` yet).
- `split` uses a `u8` delimiter and returns `VecString`.

## Runtime (current)

Implemented:
- Capability handles (random u64) for System/Console/ReadFS + opaque structs
- Console print/println (+ i32 versions)
- ReadFS minting + `read_to_string` + path normalization/attenuation
- Args + stdin helpers
- String helpers (`len`, `byte_at`, `as_slice`)
- Buffer + slice support
- VecU8/I32/String and string splitting

Missing / TODO:
- Stable ABI docs
- Memory safety still manual (malloc/free, pointers)
- Capability mutability tracking removed (was unused)

## Codegen (current)

Implemented:
- Cranelift backend for expressions, control flow, match, Result values
- Runtime intrinsics mapping for sys APIs
- Out-param ABI for Result<String, E> and small Result types
- Module resolution + type lowering for sys types

Missing / TODO:
- i64 type lowering
- More structured ABI (doc + shared header)
- Optimization/lint passes not present

## Tooling (current)

CLI:
- `capc check/build/run/audit`
- `--safe-only` rejects unsafe transitive deps
- `--link-lib`/`--link-search` for extern demo

Editor:
- Tree-sitter grammar + TextMate highlighting
- VS Code extension runs `capc check` for diagnostics

## Tests (current)

- Parser snapshots (`capc/tests/parser.rs`)
- Typecheck tests for caps + unsafe boundary
- Runtime/run tests for fs, args, stdin, buffer/slice, wc demo
- Negative tests: capability forging, wrong type, missing caps

## What’s Built vs Not Built (quick list)

Built:
- Capability discipline (A + B enforcement for ReadFS)
- Safe/unsafe boundaries + audit
- Basic runtime + codegen
- Monomorphic collections + string splitting
- VS Code diagnostics (via `capc check`)

Not Built:
- Generics (Vec<T>, Result<T,E> generalized)
- i64
- Concurrency
- Linear/affine ownership or borrow checking
- Real package manager / dependency system
- Robust editor integration (go-to-def, hover)

## Next Steps (high value)

- Add `i64` end-to-end (lexer/parser/typecheck/codegen/runtime helpers).
- Decide generic strategy:
  - Keep monomorphic collections for now, or
  - Introduce generics with monomorphization (Vec<T>, Result<T,E>).
- Harden the ABI docs (document runtime symbols + calling convention).
- Consider a small `core` stdlib module separate from `sys`.
- Expand diagnostics (better spans, multiple errors).
