if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias cgrep='grep --color=always'
fi

alias df='df -h'
alias du='du -h'

alias less='less -RFX' # use grep --color=always to pass colors to less
alias rgrep='grep -r --exclude-dir=\.svn --exclude-dir=\.git'
