function fish_prompt
    set last_status $status
    
    # Who I am
    set_color yellow
    printf '\n%s@%s' (whoami) (hostname)
    
    # last status
    set_color normal
    printf ' ('
    if test 0 -ne $last_status
        set_color red
    end
    printf '%d' $last_status
    set_color normal
    printf ')'

    # cwd
    set_color $fish_color_cwd
    printf ' %s' (prompt_pwd)
    
    # Git status
    set_color normal
    if test (pwd | grep '^/vagrant\b')
        set -ge __fish_git_prompt_showdirtystate
    else
        set -g __fish_git_prompt_showdirtystate 'yes'
    end
    printf '%s' (__fish_git_prompt)

    # prompt symbol
    printf '\n> '
end
