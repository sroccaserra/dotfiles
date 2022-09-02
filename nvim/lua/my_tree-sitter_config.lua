require 'nvim-treesitter.configs'.setup {
  ensure_installed = { "go", "haskell", "java", "javascript", "lua", "python", "ruby", "typescript", "vim" },
  sync_install = false,
  -- auto_install = true,
  -- ignore_install = { "javascript" },

  highlight = {
    enable = true,
    -- disable = { "help" },
    additional_vim_regex_highlighting = false,
  },


  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "gnn",
      node_incremental = "grn",
      scope_incremental = "grc",
      node_decremental = "grm",
    },
  },

  indent = {
    enable = true,
  },
}

vim.cmd [[ set foldmethod=expr ]]
vim.cmd [[ set foldexpr=nvim_treesitter#foldexpr() ]]
vim.cmd [[ set nofoldenable ]]
