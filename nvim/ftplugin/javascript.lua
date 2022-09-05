local my_lib = require('my_lib')

local opts = { noremap = true, silent = true, buffer = true }
vim.keymap.set('n', '<leader>a', my_lib.find_file_or_test_file, opts)
