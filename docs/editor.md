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
