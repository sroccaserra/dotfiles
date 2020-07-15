# fish command line shell - http://fishshell.com

set fish_greeting

if status --is-interactive
    if not contains -- $sroccaserra_dotfiles/fish/functions $fish_function_path
        set -p fish_function_path $sroccaserra_dotfiles/fish/functions
    end

    set -gx EDITOR vim
    set -gx LESS "-iFRSXM"

    set -u fish_user_paths ~/bin $PATH

    if test -x '/usr/games/fortune'
        echo
        if test -x '/usr/games/cowsay'
            set animal (basename (find /usr/share/cowsay/ -name '*.cow' | sort -R | head -n 1) | sed 's/.cow$//')
            /usr/games/fortune -s | /usr/games/cowsay -f "$animal"
        else
            /usr/games/fortune -s
        end
    end
end

alias rgrep="grep -r"
alias igrep="grep -i"

############
# Git config
set __fish_git_prompt_showupstream 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_color_branch cyan

set __fish_git_prompt_char_dirtystate '🚧'
set __fish_git_prompt_char_stagedstate '→'
set __fish_git_prompt_char_stashstate '📝'
set __fish_git_prompt_char_upstream_ahead '↑'
set __fish_git_prompt_char_upstream_behind '↓'
############
