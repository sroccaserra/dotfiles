@echo off

set PWD=%~dp0

if not exist "%HOME%\.ssh" (
    if exist "%HOME%\Dropbox\.ssh" (
        mklink /D "%HOME%\.ssh" "%HOME%\Dropbox\.ssh"
    )
)

for %%X in (emacs.exe) do (set EMACS=%%~$PATH:X)
if not defined EMACS (
    echo Suggestion: You should add Emacs to your path.
) else (
    for %%E in ("%EMACS%") do (
        echo copy "%PWD%emacsw.bat" "%%~dpE"
        copy "%PWD%emacsw.bat" "%%~dpE"
    )
)

for %%X in (es.exe) do (set EVERYTHING=%%~$PATH:X)
if not defined EVERYTHING (
    echo Suggestion: You should add Everything ^(http://www.voidtools.com^) to your path.
)

echo.
pause

