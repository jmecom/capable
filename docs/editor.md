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

## Neovim

This repo now works directly as a Neovim plugin.

What it gives you:

- `*.cap` filetype detection
- built-in Vim syntax highlighting that works without tree-sitter
- comment settings for Capable buffers
- LSP startup through the `caplsp` binary
- optional `nvim-treesitter` parser registration for the bundled grammar

### Install

With `lazy.nvim`:

```lua
{
  dir = "/absolute/path/to/capable",
  config = function()
    require("capable").setup({
      caplsp_cmd = { "/absolute/path/to/caplsp" },
    })
  end,
}
```

If `caplsp` is already on your `PATH`, the default `setup()` is enough.

### Optional tree-sitter

If you use `nvim-treesitter`, the plugin registers a `capable` parser config that
points at `tree-sitter-capable/` in this repo. After installing the plugin, run:

```vim
:TSInstall capable
```

The regex-based `syntax/cap.vim` highlighting still works even if you do not use
tree-sitter.

## VS Code

1. Launch VS Code (or Cursor) with the extension in dev mode:

```sh
code --extensionDevelopmentPath=./vscode
```

The extension registers the `cap` language, basic highlighting, and runs
`capc check` for diagnostics (errors show as squiggles).

If `capc` is not on your PATH, set:

```json
{
  "capable.capcPath": "/absolute/path/to/capc"
}
```
