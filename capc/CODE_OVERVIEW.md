# Capc Code Overview

This document explains how the capc compiler is structured, how the main pieces
fit together, and what data flows between them.

The goal: make it easy for a new reader to orient themselves quickly.

## Big Picture

The compiler pipeline is straightforward:

```
+-----------+    +--------+    +--------+    +--------+    +--------+    +---------+
|  source   | -> | lexer  | -> | parser | -> | typeck | -> |  HIR   | -> | codegen |
+-----------+    +--------+    +--------+    +--------+    +--------+    +---------+
                                                               |
                                                               v
                                                          object (.o)
```

Key idea: type checking produces HIR (typed, resolved), and codegen trusts HIR.

## Top-Level Entry Points

The CLI (`capc`) ties everything together:

capc/src/main.rs
  - parse module
  - load stdlib + user modules
  - type_check_program(...) -> HIR
  - build_object(...) -> .o
  - link runtime + .o -> executable

## AST -> HIR -> Cranelift

AST lives in capc/src/ast.rs and is produced by the parser. It is syntax-first:
spans, surface constructs, and names that are not yet resolved. HIR lives in
capc/src/hir.rs and is produced by the type checker; it is fully typed and
symbol-resolved. Codegen consumes HIR and emits Cranelift IR (CLIF), which is
then serialized into an object file.

Put another way:
- AST answers: “What did the user write?”
- HIR answers: “What does it mean, with types and resolved symbols?”
- CLIF answers: “What machine-level operations implement it?”

Data flow:

```
              +------------------+
              | AST (capc/ast.rs)|
              +------------------+
                       |
                       | type_check_program
                       v
              +------------------+
              | HIR (capc/hir.rs)|
              +------------------+
                       |
                       | build_object
                       v
              +------------------+
              |  Cranelift IR    |
              +------------------+
                       |
                       v
              +------------------+
              |   object (.o)    |
              +------------------+
```

## Type Checker Layout

The type checker is split into modules under capc/src/typeck/:

```
+-----------------+    +-----------------+    +-----------------+    +-----------------+
| collect.rs      | -> | check.rs        | -> | lower.rs        | -> | HIR (hir.rs)    |
| indexing        |    | typing + moves  |    | HIR lowering    |    | (typed IR)      |
+-----------------+    +-----------------+    +-----------------+    +-----------------+
         ^                        ^                       ^
         |                        |                       |
         +---------- mod.rs ------+-----------------------+
            shared types + entry points
```

Notes:
- collect.rs builds indices (structs/enums/functions).
- check.rs enforces typing and move/linear rules.
- lower.rs converts the typed AST into HIR.

## Codegen Layout

Codegen is split under capc/src/codegen/:

```
+-----------------+    +-----------------+    +-----------------+
| layout.rs       | -> | emit.rs         | -> | object (.o)     |
| sizes/offsets   |    | HIR -> CLIF     |    | build_object    |
+-----------------+    +-----------------+    +-----------------+
         ^                        ^
         |                        |
         +---------- mod.rs ------+
            orchestration + shared types
```

Notes:
- layout.rs computes struct layouts and enum discriminants.
- emit.rs turns HIR into Cranelift IR and handles ABI lowering.
- mod.rs wires everything together in build_object(...).

## Runtime Interface

The runtime lives in `runtime/` and is linked in by the CLI.
The stdlib provides Capable-level wrappers for runtime intrinsics.

Call flow:

```
Capable code -> stdlib wrapper -> runtime intrinsic -> host OS
```

Example:

```
sys::console::Console.print
  -> sys.console.Console__print (stdlib wrapper)
  -> capable_rt_console_print (runtime function)
```

## Capability Attenuation + Move Rules (Compiler)

Move-only rules are enforced in typeck/check.rs. The basic model:

- Unrestricted: copyable values (ints, bools, etc.)
- Affine: move-only, drop allowed (caps/handles)
- Linear: move-only, must be consumed on all paths

Path-sensitive rules are applied at if/match joins. Loops are conservative.

## Module Resolution

The loader handles module discovery and validates module paths.

```
+-------------+    +-------------------+    +------------------+
| file paths  | -> | loader.rs         | -> | ast::Module      |
+-------------+    +-------------------+    +------------------+
```

The parser produces an AST Module, and `use` declarations are resolved into
fully-qualified paths in the type checker.

## Helpful Files

- capc/src/main.rs
- capc/src/lib.rs
- capc/src/ast.rs
- capc/src/parser.rs
- capc/src/typeck/mod.rs
- capc/src/codegen/mod.rs
- capc/src/hir.rs
- runtime/
- stdlib/
