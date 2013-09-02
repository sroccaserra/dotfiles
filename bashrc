# -*- mode: shell -*-
# vi: filetype=sh:

PS1='\[\033[00m\]\n\[\033[00;32m\]\u@\h\[\033[00m\] $? \[\033[01;33m\]\w\[\033[00m\]\n\[\033[00;32m\]\$\[\033[00m\] '

export PATH=$HOME/local/bin:$PATH 

[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

if [[ -d $HOME/.rvm ]]
then
    PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
fi

# virtualenv wrapper
if [[ -d $HOME/.virtualenvs ]]
then
    export WORKON_HOME=$HOME/.virtualenvs
fi
if [[ -e /usr/bin/virtualenvwrapper.sh ]]
then
    source /usr/bin/virtualenvwrapper.sh 
fi

test -f /etc/bash_completion.d/git && source /etc/bash_completion.d/git
case "$TERM" in
screen*) 
    GIT_PS1_SHOWDIRTYSTATE=true
    PROMPT_COMMAND='echo -ne "\033k$(__git_ps1)\033"'
    ;;
esac

test -f $HOME/.bash_aliases && source $HOME/.bash_aliases

##########################
# Formerly in bash_profile

export EDITOR=vim

export LESS="-iFRSX"
export GREP_OPTIONS="--color=auto"

export PATH=$PATH:$HOME/bin

export C_INCLUDE_PATH=/usr/local/include:/usr/include
export CPLUS_INCLUDE_PATH=$C_INCLUDE_PATH

if [[ -d /usr/lib/jvm/java-6-sun ]]
then
    export JAVA_HOME="/usr/lib/jvm/java-6-sun"
fi
