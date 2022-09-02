local telescope = require('telescope.builtin')

local function find_file_or_test_file()
  local filename_with_no_ext = vim.fn.expand('%:r'):gsub('.*/', '')
  local test_file_pattern = '[-_]?[tT]est'

  local fzf_query
  if filename_with_no_ext:find(test_file_pattern) then
    fzf_query = filename_with_no_ext:gsub(test_file_pattern, '')
  else
    fzf_query = filename_with_no_ext .. "test"
  end

  -- local command = "call fzf#vim#gitfiles('.', {'options': '--query " .. fzf_query .. "'})"
  -- vim.api.nvim_exec(command, {})
  telescope.find_files({ default_text = fzf_query })
end

local opts = { noremap = true, silent = true, buffer = true }
vim.keymap.set('n', '<leader>a', find_file_or_test_file, opts)
