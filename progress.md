# Project Progress

## Milestones

- M0: Core language skeleton (parser/AST, typecheck, stdlib wiring, basic tests) -> done
- M1: Codegen for basic programs + module loader -> done
- M2: Runtime + sys capability plumbing -> in progress (console + ReadFS basic enforcement)

## Components

- Frontend (lexer/parser/AST): done (M0)
- Module loader + module graph: done (M1)
- Typechecker (opaque structs, enums, Result[string, FsErr]): done (M1)
- Stdlib sources (sys.system/sys.console/sys.fs): done (M1)
- Codegen (functions/let/return/if/while/boolean ops/match stmt): done (M1)
- Runtime:
  - startup + console print/println: done (M2)
  - ReadFS minting + read_to_string + path checks: done (M2, basic)
- CLI (capc check/build/run) + justfile: done (M1)
- Tests (positive/negative programs): done (M1)
