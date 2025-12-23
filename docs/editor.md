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

Load the parser + LSP with:

```lua
require("nvim-treesitter.parsers").get_parser_configs()
require("path.to.nvim.capable").setup()
```

`caplsp` publishes `textDocument/publishDiagnostics`, so errors from `capc check`
show up as squiggles automatically.
