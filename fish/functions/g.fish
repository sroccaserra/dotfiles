function g --description='open fzf then inserts selection in commandline'
    git ls-files | fzf | read foo
    if [ "$foo" ]
        commandline -i "$foo"
    end
end
