if [[ -n `command -v tmux` && "$TERM" != "screen-256color" ]]
then
    tmux attach-session -t "$USER" || tmux new-session -s "$USER"
    exit
fi

PS1='\[\033[00m\]\n\[\033[00;32m\]\u@\h\[\033[00m\] \[\033[01;33m\]\w\[\033[00m\]\n\$ '

export PATH=$HOME/local/bin:$PATH 

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
 
