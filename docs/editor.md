# Editor Support

## LSP (diagnostics)

Run the language server:

```sh
just lsp
```

It speaks LSP over stdio. Point your editor to the `caplsp` binary and set the
file type to `cap`.

## Tree-sitter

The grammar lives in `tree-sitter-capable/`. You can generate a parser with:

```sh
cd tree-sitter-capable
tree-sitter generate
```

## Neovim example

Run `scripts/install-editor.sh` to drop the tree-sitter parser into `~/.config/nvim`
and copy the helper module to `~/.config/nvim/lua/capable`. That script also runs
`tree-sitter generate` so the parser is ready.

Then add this snippet:

```lua
require("capable").setup()
```

`caplsp` publishes `textDocument/publishDiagnostics`, so errors from `capc check`
show up as squiggles automatically.

If you need a custom tree-sitter path, set:

```lua
vim.g.capable_treesitter_dir = "/path/to/tree-sitter-capable"
```
