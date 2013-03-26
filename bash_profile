# -*- mode: shell -*-
# vi: syn=sh:

export EDITOR=vim

export LESS="-iFRSX"
export GREP_OPTIONS="--color=auto"

export LD_LIBRARY_PATH=/usr/local/mysql/lib:/usr/local/lib:/usr/lib
export C_INCLUDE_PATH=/usr/local/include:/usr/include
export CPLUS_INCLUDE_PATH=$C_INCLUDE_PATH

if [ -d /usr/lib/jvm/java-6-sun ]
then
    export JAVA_HOME="/usr/lib/jvm/java-6-sun"
fi

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
        . "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# [ -z "$TMUX" ] && export TERM=xterm-256color
