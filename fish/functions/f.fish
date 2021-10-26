function f --description='open fzf then paste selection in commandline'
    git ls-files | fzf | read foo
    if [ "$foo" ]
        commandline "$foo"
    end
end
