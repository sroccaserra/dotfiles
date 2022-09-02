require('telescope').setup {
  defaults = {
    file_ignore_patterns = { "package-lock.json" },
    mappings = {
      i = {
        ["<C-j>"] = "move_selection_next",
        ["<C-k>"] = "move_selection_previous",
      },
    }
  }
}
