@echo off

set PWD=%~dp0

if not defined HOME (
    setx HOME "%USERPROFILE%"
)

for %%X in (git.exe) do (set GIT=%%~$PATH:X)
if not defined GIT (
    echo Please add Git to your path.
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

for %%p in (emacs smart-tab) do (
    if not exist "%HOME%\developer\%%p" (
        git clone "git@github.com:sroccaserra/%%p.git" "%HOME%/developer/%%p"
    ) else (
        echo %%p ok.
    )
)

for %%X in (curl.exe) do (set CURL=%%~$PATH:X)
if not defined CURL (
    echo Suggestion: You should add curl to your path. (Good for Vundle)
)

if x"%PATH:\vim\=%"==x"%PATH%" (
    echo Suggestion: You should add Vim to your path.
)

for %%X in (emacs.exe) do (set EMACS=%%~$PATH:X)
if not defined EMACS (
    echo Suggestion: You should add Emacs to your path.
) else (
    for %%E in (%EMACS%) do (
        copy "%PWD%\emacsw.bat" "%%~dpE"
    )
)

for %%X in (es.exe) do (set EVERYTHING=%%~$PATH:X)
if not defined EVERYTHING (
    echo Suggestion: You should add Everything ^(http://www.voidtools.com^) to your path.
)

echo.
pause

