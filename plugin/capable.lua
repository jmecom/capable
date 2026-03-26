if vim.g.loaded_capable_nvim == 1 then
  return
end

vim.g.loaded_capable_nvim = 1

require("capable").setup(vim.g.capable_nvim_config or {})
