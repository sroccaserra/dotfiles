# sroccaserra

if [[ -d $HOME/.rvm ]]
then
    PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
fi

if [[ "$TERM" != "screen-256color" ]]
then
    tmux attach-session -t "$USER" || tmux new-session -s "$USER"
    exit
fi
