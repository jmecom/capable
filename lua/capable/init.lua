local M = {}

local defaults = {
  lsp = true,
  treesitter = true,
  caplsp_cmd = { "caplsp" },
  filetypes = { "cap" },
  root_markers = { ".git", "justfile", "Cargo.toml" },
  on_attach = nil,
  capabilities = nil,
}

local state = {
  config = nil,
  setup_done = false,
}

local function plugin_root()
  local source = debug.getinfo(1, "S").source
  if source:sub(1, 1) == "@" then
    source = source:sub(2)
  end
  return vim.fs.dirname(vim.fs.dirname(vim.fs.dirname(source)))
end

local function parser_candidates()
  local root = plugin_root()
  local data = vim.fn.stdpath("data")
  return {
    data .. "/site/parser/capable.so",
    root .. "/parser/capable.so",
    root .. "/tree-sitter-capable/capable.so",
  }
end

local function register_compiled_parser()
  if not (vim.treesitter and vim.treesitter.language and vim.treesitter.language.add) then
    return false
  end

  for _, path in ipairs(parser_candidates()) do
    if vim.uv.fs_stat(path) then
      pcall(vim.treesitter.language.add, "capable", { path = path })
      return true
    end
  end

  return false
end

local function normalize_cmd(cmd)
  if type(cmd) == "string" then
    return { cmd }
  end
  return vim.deepcopy(cmd)
end

local function resolve_root(bufnr, markers)
  local name = vim.api.nvim_buf_get_name(bufnr)
  local start = name ~= "" and vim.fs.dirname(name) or vim.uv.cwd()
  return vim.fs.root(start, markers) or start or vim.uv.cwd()
end

function M.register_treesitter()
  register_compiled_parser()

  local ok, parsers = pcall(require, "nvim-treesitter.parsers")
  if not ok then
    return false
  end

  parsers.capable = parsers.capable or {}
  parsers.capable.install_info = {
    url = plugin_root() .. "/tree-sitter-capable",
    files = { "src/parser.c" },
    requires_generate_from_grammar = false,
    generate_requires_npm = false,
  }
  parsers.capable.filetype = "cap"

  if vim.treesitter and vim.treesitter.language and vim.treesitter.language.register then
    pcall(vim.treesitter.language.register, "capable", "cap")
  end

  register_compiled_parser()

  return true
end

local function start_lsp(bufnr)
  if not state.config.lsp then
    return
  end
  if vim.bo[bufnr].buftype ~= "" then
    return
  end

  local name = vim.api.nvim_buf_get_name(bufnr)
  if name == "" then
    return
  end

  vim.lsp.start({
    name = "caplsp",
    cmd = normalize_cmd(state.config.caplsp_cmd),
    root_dir = resolve_root(bufnr, state.config.root_markers),
    filetypes = state.config.filetypes,
    single_file_support = true,
    on_attach = state.config.on_attach,
    capabilities = state.config.capabilities,
  }, {
    bufnr = bufnr,
  })
end

function M.setup(opts)
  state.config = vim.tbl_deep_extend("force", state.config or defaults, opts or {})

  if state.config.treesitter then
    if not M.register_treesitter() then
      local group = vim.api.nvim_create_augroup("CapableNvimTreesitter", { clear = true })
      vim.api.nvim_create_autocmd("VimEnter", {
        group = group,
        once = true,
        callback = function()
          M.register_treesitter()
        end,
      })
      vim.api.nvim_create_autocmd("User", {
        group = group,
        pattern = "LazyDone",
        once = true,
        callback = function()
          M.register_treesitter()
        end,
      })
    end
  end

  if state.setup_done then
    return M
  end

  local group = vim.api.nvim_create_augroup("CapableNvim", { clear = true })
  vim.api.nvim_create_autocmd("FileType", {
    group = group,
    pattern = state.config.filetypes,
    callback = function(args)
      start_lsp(args.buf)
    end,
  })

  state.setup_done = true

  for _, bufnr in ipairs(vim.api.nvim_list_bufs()) do
    if vim.api.nvim_buf_is_loaded(bufnr) and vim.bo[bufnr].filetype == "cap" then
      start_lsp(bufnr)
    end
  end

  return M
end

return M
