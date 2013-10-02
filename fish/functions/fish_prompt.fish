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
