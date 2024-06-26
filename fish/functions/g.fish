function g --description='open fzf then inserts selection in commandline'
    if type -q rg
        rg --files | fzf | read file
    else if [ (git rev-parse --git-dir 2> /dev/null) ]
        git ls-files | fzf | read file
    else
        fzf | read file
    end
    if [ "$file" ]
        echo -n "$file" | pbcopy
        commandline -i "$file"
    end
end
