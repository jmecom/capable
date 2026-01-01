FIXME
=====

Resolved (current branch)
-------------------------
- break/continue linear-consumption checks now only validate scopes exited by
  the loop, not all scopes.
- Slice/MutSlice indexing now requires <u8> to match runtime behavior.
- Direct runtime calls now use the target's default calling convention.
- Generic-vs-comparison parsing uses a lookahead for `>` followed by `(` or `{`
  to avoid false-positive generic parsing.
