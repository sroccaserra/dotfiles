local telescope = require 'telescope.builtin'

local function find_file_or_test_file()
  local filename_with_no_ext = vim.fn.expand('%:r'):gsub('.*/', '')
  local test_file_pattern = '[-_]?[tT]est'

  local fuzzy_query
  if filename_with_no_ext:find(test_file_pattern) then
    fuzzy_query = filename_with_no_ext:gsub(test_file_pattern, '')
  else
    fuzzy_query = filename_with_no_ext .. "test"
  end

  telescope.find_files({ default_text = fuzzy_query })
end

return {
  fn = find_file_or_test_file
}
