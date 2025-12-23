# Project Progress

## Milestones

- M0: Core language skeleton (parser/AST, typecheck, stdlib wiring, basic tests) -> done
- M1: Codegen for basic programs + module loader -> done
- M2: Runtime + sys capability plumbing -> done (console + ReadFS enforcement + fs error kinds)

## Components

- Frontend (lexer/parser/AST): done (M0)
- Module loader + module graph: done (M1)
- Typechecker (opaque structs, enums, Result + match expr typing): done (M2)
- Stdlib sources (sys.system/sys.console/sys.fs): done (M1)
- Codegen (functions/let/return/if/while/boolean ops/match stmt+expr, Result values): done (M2)
- Runtime:
  - startup + console print/println: done (M2)
  - ReadFS minting + read_to_string + path checks: done (M2)
- CLI (capc check/build/run) + justfile: done (M1)
- Tests (positive/negative programs, runtime run tests): done (M2)
