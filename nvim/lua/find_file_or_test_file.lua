local function find_file_or_test_file()
  local filename_with_no_ext = vim.fn.expand('%:r'):gsub('.*/', '')
  local test_file_pattern = '[-_]?[tT]est'

  local fuzzy_query
  if filename_with_no_ext:find(test_file_pattern) then
    fuzzy_query = "'" .. filename_with_no_ext:gsub(test_file_pattern, '')
  else
    fuzzy_query = "'" .. filename_with_no_ext .. " 'test"
  end

  vim.fn['fzf#vim#files']('.', { source = "rg -l ''", options = '--query "' .. fuzzy_query ..'"' })
end

return {
  fn = find_file_or_test_file
}
