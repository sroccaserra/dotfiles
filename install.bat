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

if not exist %HOME%\.ssh (
    if exist %HOME%\Dropbox\.ssh (
        mklink /D %HOME%\.ssh %HOME%\Dropbox\.ssh
    )
)

if x"%PATH:\git\bin=%"==x"%PATH%" (
    echo Suggestion: You should add Git\bin to your path.
)

if x"%PATH:\vim\=%"==x"%PATH%" (
    echo Suggestion: You should add Vim to your path.
)

if x"%PATH:emacs=%"==x"%PATH%" (
    echo Suggestion: You should add Emacs to your path.
)

pause

