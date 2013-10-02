#!/usr/bin/fish

set -x EDITOR vim

set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_color_branch yellow

set __fish_git_prompt_char_dirtystate '⚡'
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_stashstate '↩'
set __fish_git_prompt_char_upstream_ahead '↑'
set __fish_git_prompt_char_upstream_behind '↓'

function fish_prompt
    set last_status $status
    
    set_color normal
    printf '\n%s@%s' (whoami) (hostname)
    
    if test 0 -ne $last_status
        set_color red
    end
    printf ' %d' $last_status

    set_color $fish_color_cwd
    printf ' %s' (prompt_pwd)
    
    set_color normal
    if test (pwd | grep '^/vagrant\b')
        set -ge __fish_git_prompt_showdirtystate
    else
        set -g __fish_git_prompt_showdirtystate 'yes'
    end
    printf '%s\n$ ' (__fish_git_prompt)

    set_color normal
end

status --is-login; and status --is-interactive; and exec byobu-launcher
