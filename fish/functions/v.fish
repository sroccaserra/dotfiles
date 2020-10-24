function v --description='open fzf then selection in vim'
    git ls-files | fzf --multi | xargs -o vim
end
