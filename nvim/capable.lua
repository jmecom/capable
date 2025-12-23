local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.capable = {
  install_info = {
    url = vim.fn.expand("~/Development/capable/tree-sitter-capable"),
    files = { "src/parser.c" },
    branch = "main",
  },
  filetype = "cap",
}

local lspconfig = require("lspconfig")
local root_files = {
  ".git",
  "capc.toml",
}

lspconfig.caplsp = {
  cmd = { "caplsp" },
  filetypes = { "cap" },
  root_dir = lspconfig.util.root_pattern(unpack(root_files)),
}

return {
  setup = function()
    require("lspconfig").caplsp.setup({})
  end,
}
