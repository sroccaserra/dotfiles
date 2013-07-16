# -*- mode: shell -*-
# vi: syn=sh:

if [ -x /usr/bin/dircolors ]
then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias cgrep='grep --color=always'
    if [[ -n "`command -v colormake`" ]]
    then
        alias make=colormake
    fi
fi

alias df='df -h'
alias du='du -h'
alias emacs='TERM=xterm-256color emacs'

alias rgrep='grep -r --exclude-dir=\.svn --exclude-dir=\.git'
