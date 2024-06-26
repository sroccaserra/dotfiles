# Use fish_key_reader to find key names
function fish_user_key_bindings
    if test ~/.config/fish/functions/fish_user_key_bindings.fish
        source ~/.config/fish/functions/fish_user_key_bindings.fish
    end
    bind \e\eOA 'history-token-search-backward'
    bind \cX\cE 'edit_command_buffer'
    bind \cG 'g'
    bind \cC 'commandline ""'
    bind \cN 'commandline -i (date -I)'
end
