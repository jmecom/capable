# VS Code extension (Capable)

## Dev usage

```sh
cd vscode
npm install
code --extensionDevelopmentPath=.
```

Ensure `caplsp` is built and on your PATH (`just lsp`).

## Config

If `caplsp` is not on PATH, set:

```json
{
  "capable.serverPath": "/absolute/path/to/caplsp"
}
```
