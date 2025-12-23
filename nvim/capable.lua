local function treesitter_dir()
  if vim.g.capable_treesitter_dir ~= nil and vim.g.capable_treesitter_dir ~= "" then
    return vim.g.capable_treesitter_dir
  end
  return vim.fn.stdpath("config") .. "/tree-sitter-capable"
end

local function register_parser()
  local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
  parser_config.capable = {
    install_info = {
      url = treesitter_dir(),
      files = { "src/parser.c" },
      branch = "main",
    },
    filetype = "cap",
  }
end

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
    register_parser()
    require("lspconfig").caplsp.setup({})
  end,
}
