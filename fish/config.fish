#!/usr/bin/fish

set fish_greeting

if status --is-interactive
    if test -x '/usr/games/fortune'
        echo
        /usr/games/fortune -s
    end
end

set -x EDITOR vim
set -x LESS "-iFRSX"

set -u fish_user_paths ~/bin

############
# Git config
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_color_branch yellow

set __fish_git_prompt_char_dirtystate '⚡'
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_stashstate '↩'
set __fish_git_prompt_char_upstream_ahead '↑'
set __fish_git_prompt_char_upstream_behind '↓'

############

