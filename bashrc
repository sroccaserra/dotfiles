# if [[ -n `command -v tmux` && "$TERM" != "screen-256color" ]]
# then
#     tmux attach-session -t "$USER" || tmux new-session -s "$USER"
#     exit
# fi
 
PS1='\[\033[00m\]\n\[\033[00;32m\]\u@\h\[\033[00m\] $? \[\033[01;33m\]\w\[\033[00m\]\n\[\033[00;32m\]\$\[\033[00m\] '

export PATH=$HOME/local/bin:$PATH 

[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

if [[ -d $HOME/.rvm ]]
then
    PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
fi

# virtualenv wrapper
if [ -d $HOME/.virtualenvs ] ; then
    export WORKON_HOME=$HOME/.virtualenvs
fi
if [ -e /usr/bin/virtualenvwrapper.sh ] ; then
    source /usr/bin/virtualenvwrapper.sh 
fi

case "$TERM" in
screen*) 
    GIT_PS1_SHOWDIRTYSTATE=true
    PROMPT_COMMAND='echo -ne "\033k$(__git_ps1)\033"'
    ;;
esac

