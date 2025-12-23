#!/bin/sh
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
TS_SRC="$ROOT/tree-sitter-capable"
NVIM_CONF="${XDG_CONFIG_HOME:-$HOME/.config}/nvim"
NVIM_CAST="$NVIM_CONF/tree-sitter-capable"
LUA_DIR="$NVIM_CONF/lua/capable"

echo "1. Generating tree-sitter parser..."
cd "$TS_SRC"
if command -v tree-sitter >/dev/null 2>&1; then
  tree-sitter generate
else
  echo "ERROR: tree-sitter CLI missing; install it from https://tree-sitter.github.io/tree-sitter/using-parsers"
  exit 1
fi

echo "2. Installing parser into $NVIM_CAST"
mkdir -p "$NVIM_CAST"
rm -rf "$NVIM_CAST"/*
cp -r "$TS_SRC"/* "$NVIM_CAST"/

echo "3. Copying Neovim helper module..."
mkdir -p "$LUA_DIR"
cp "$ROOT/nvim/capable.lua" "$LUA_DIR/init.lua"

cat <<'EOF'

Done! Add the following to your Neovim config:

require("nvim-treesitter.parsers").get_parser_configs().capable.install_info.url =
  vim.fn.expand("~/.config/nvim/tree-sitter-capable")
require("capable").setup()

Build the Caplsp binary (`just lsp` or `cargo run -p caplsp`) and ensure it's on your PATH;
the LSP then publishes diagnostics from `capc check`.
EOF
