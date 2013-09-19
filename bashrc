# -*- mode: shell -*-
# vi: filetype=sh:

test -f /etc/bash_completion.d/git && source /etc/bash_completion.d/git

git_ps1_maybe() {
    if test -n "`type __git_ps1`"
    then
        [[ ! "$(pwd)" = /vagrant/* ]] && GIT_PS1_SHOWDIRTYSTATE=true
        __git_ps1
    fi
}

# \e[0;32m = green, \e[1;33m = bold yellow, \e[1;36 = bold cyan, \e[m = reset
PS1='\n\[\e[0;32m\]\u@\h\[\e[m\] $? \[\e[1;33m\]\w\[\e[1;36m\]$(git_ps1_maybe)\n\[\e[0;32m\]\$\[\e[m\] '

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
