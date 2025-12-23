# Project Progress

## Milestones

- M0: Core language skeleton (parser/AST, typecheck, stdlib wiring, basic tests) -> done
- M1: Codegen for basic programs + module loader -> done
- M2: Runtime + sys capability plumbing -> done (console + ReadFS enforcement + fs error kinds)
- M3: Scoped filesystem capability -> done (runtime checks + normalization tests)
- M4: Safe/unsafe boundary -> done (extern + safe-only + audit + link flags)

## Components

- Frontend (lexer/parser/AST): done (M0)
- Module loader + module graph: done (M1)
- Typechecker (opaque structs, enums, Result + match expr typing, extern safety): done (M4)
- Stdlib sources (sys.system/sys.console/sys.fs): done (M1)
- Codegen (functions/let/return/if/while/boolean ops/match stmt+expr, Result values): done (M2)
- Stdlib helpers (string/bytes + split + collections): done (M4)
- Runtime:
  - startup + console print/println: done (M2)
  - ReadFS minting + read_to_string + path checks: done (M2)
  - collections + string split intrinsics: done (M4)
- CLI (capc check/build/run/audit, safe-only, link flags) + justfile: done (M4)
- Tests (positive/negative programs, runtime run tests, externs, path normalization): done (M4)
