vim.cmd [[ source ~/.vimrc ]]
vim.cmd [[ set rtp+=$HOME/dotfiles/nvim ]]

require 'my_telescope_config'
require 'my_lsp_config'
require 'my_tree-sitter_config'
