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

1. Build/install the LSP server:

```sh
just lsp
```

2. Install the extension dependencies:

```sh
cd vscode
npm install
```

3. Launch VS Code with the extension in dev mode:

```sh
code --extensionDevelopmentPath=./vscode
```

The extension registers the `cap` language, basic highlighting, and runs `caplsp`
for diagnostics (errors from `capc check` show as squiggles).
