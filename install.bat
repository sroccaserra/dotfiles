@echo off

if not defined HOME (
    setx HOME "%USERPROFILE%"
)

if not exist %HOME%\.vim\bundle\vundle (
    git clone https://github.com/gmarik/vundle.git %HOME%/.vim/bundle/vundle
)

if not exist %HOME%\.vimrc (
    echo source ~/dotfiles/vimrc > %HOME%\.vimrc
)

if not exist %HOME%\.emacs (
    echo (load-file "~/dotfiles/emacs"^) > %HOME%\.emacs
)

pause

