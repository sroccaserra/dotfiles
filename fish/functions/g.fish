function g --description='open fzf then inserts selection in commandline'
    if [ (git rev-parse --git-dir 2> /dev/null) ]
        git ls-files | fzf | read foo
    else
        fzf | read foo
    end
    if [ "$foo" ]
        echo -n "$foo" | pbcopy
        commandline -i "$foo"
    end
end
