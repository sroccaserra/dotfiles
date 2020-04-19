function v --description='open fzf then selection in vim'
  set files (git ls-files | fzf --multi --height=40%) && vim $files
end
