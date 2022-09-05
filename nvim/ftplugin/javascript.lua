local find_file_or_test_file = require('find_file_or_test_file').fn

local opts = { noremap = true, silent = true, buffer = true }
vim.keymap.set('n', '<leader>a', find_file_or_test_file, opts)
