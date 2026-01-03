# Current status

This file is intended to reflect the current compiler/stdlib behavior in the
repository. It is based on code and tests, not older design docs.

## Language surface

- Modules declare a package safety level (safe/unsafe) and a module path.
- Items: functions, extern functions, structs, enums, and impl blocks.
- Structs can be opaque, linear, copy, and/or capability types.
- Enums support payload variants.
- Generics exist on functions, structs, enums, and impl blocks and are
  monomorphized during compilation.
- Statements: let, assign, defer, return, break, continue, if/else, while,
  for (range syntax), and expression statements.
- Expressions: literals, paths, calls, method calls, field access, indexing,
  struct literals, unary/binary ops, match, try (? operator), and grouping.

## Types and ownership

- Built-in types: i32, u32, u8, bool, unit, never.
- `string` is a stdlib struct (a view over bytes), not a compiler builtin.
- Pointers (`*T`) and borrows (`&T`) are supported in the type system.
- Affine/linear ownership rules are enforced for non-copy values (including
  capability and linear types).
- Integer literals type as i32; char literals are u8.

## Standard library and runtime

- Stdlib modules live under `stdlib/sys` and include: args, buffer, bytes,
  console, fs, io, math, mem, net, option, result, stdin, string, system,
  unsafe_ptr, vec.
- Many sys::* functions are runtime intrinsics; their `.cap` bodies are stubs
  and are replaced at codegen time. The intrinsic registry is in
  `capc/src/codegen/intrinsics.rs`.
- Runtime-backed intrinsics currently include:
  - RootCap minting for console, fs/filesystem, alloc, args, stdin, net.
  - Console printing and assert.
  - Filesystem ops (read/list/exists/open/close/join) and read handles.
  - Net ops (listen/connect/accept/read/write/close).
  - Args and stdin accessors.
  - Buffer/Alloc malloc/free/casts.
  - Math wrap helpers and byte whitespace checks.

## ABI and codegen

- Codegen targets native code via Cranelift.
- Non-opaque struct returns are lowered via sret out-parameters.
- Result<T, E> with struct payloads is lowered with out-parameters.
- Opaque/capability types are passed as handles.

## Known limitations and gaps

- i64 is parsed but rejected by the current backend; only 32-bit integer types
  are supported.
- Inline-by-value struct returns are not implemented (sret only).
- Vec element types are restricted to u8, i32, string, or type parameters.
- Variable shadowing is not currently modeled in lowering.
- break/continue are not supported inside expression-context matches.
- String literal escapes are limited to \n, \r, \t, \\, and \".
  Char literals additionally support \xNN escapes.
- No floating-point types are implemented.
