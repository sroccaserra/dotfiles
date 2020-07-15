# fish command line shell - http://fishshell.com

set fish_greeting

if status --is-interactive
    if not contains -- $sroccaserra_dotfiles/fish/functions $fish_function_path
        set -p fish_function_path $sroccaserra_dotfiles/fish/functions
    end

    set -gx EDITOR vim
    set -gx LESS "-iFRSXM"

    set -u fish_user_paths ~/bin $PATH
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
