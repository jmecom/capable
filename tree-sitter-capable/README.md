# tree-sitter-capable

Tree-sitter grammar for the Capable language.

## Generate the parser

```sh
tree-sitter generate
```

## Neovim

Add this repo as a parser and point `parser_config` at it, then install:

```lua
local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.capable = {
  install_info = {
    url = "/path/to/capable/tree-sitter-capable",
    files = { "src/parser.c" },
  },
  filetype = "cap",
}
```
