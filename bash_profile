# -*- mode: shell -*-
# vi: syn=sh:

export EDITOR=vim

export LESS="-iFRSX"
export GREP_OPTIONS="--color=auto"

export PATH=$PATH:$HOME/bin

# export LD_LIBRARY_PATH=/usr/local/mysql/lib:/usr/local/lib:/usr/lib:/lib
export C_INCLUDE_PATH=/usr/local/include:/usr/include
export CPLUS_INCLUDE_PATH=$C_INCLUDE_PATH

if [ -d /usr/lib/jvm/java-6-sun ]
then
    export JAVA_HOME="/usr/lib/jvm/java-6-sun"
fi

[ -z "$TMUX" ] && export TERM=xterm-256color
