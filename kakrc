add-highlighter global/ show-matching
add-highlighter global/trailing-whitespace regex '\h+$' 0:Error

colorscheme gruvbox-dark

map global insert <c-left> '<a-;>b'
map global insert <c-right> '<a-;>e'
map global normal '#' :comment-line<ret>

set-face global InsertCursor default,green+B

hook global ModeChange .*:.*:insert %{
    set-face window PrimaryCursor InsertCursor
    set-face window PrimaryCursorEol InsertCursor
}

hook global ModeChange .*:insert:.* %{ try %{
    unset-face window PrimaryCursor
    unset-face window PrimaryCursorEol
} }

hook global WinCreate ^[^*]+$ %{
    add-highlighter window/ number-lines -hlcursor
}

hook global InsertChar ù %{ try %{
      exec -draft hH <a-k>ùù<ret> d
      exec -with-hooks <esc>
} }

hook global RegisterModified '/' %{
    add-highlighter -override global/search regex "%reg{/}" 0:+u
}

hook global WinSetOption filetype=markdown %{
    add-highlighter window/ wrap -word
}

evaluate-commands %sh{
    if [ -n "$SSH_TTY" ]; then
        copy='printf "\033]52;;%s\033\\" $(base64 | tr -d "\n") > /dev/tty'
        paste='printf "paste unsupported through ssh"'
        backend="OSC 52"
    else
        case $(uname) in
            Linux)
                if [ -n "$WAYLAND_DISPLAY" ]; then
                    copy="wl-copy -p"; paste="wl-paste -p"; backend=Wayland
                else
                    copy="xclip -i"; paste="xclip -o"; backend=X11
                fi
                ;;
            Darwin)  copy="pbcopy"; paste="pbpaste"; backend=OSX ;;
        esac
    fi
    printf "map global user -docstring 'paste (after) from clipboard' p '<a-!>%s<ret>'\n" "$paste"
    printf "map global user -docstring 'paste (before) from clipboard' P '!%s<ret>'\n" "$paste"
    printf "map global user -docstring 'yank to primary' y '<a-|>%s<ret>:echo -markup %%{{Information}copied selection to %s primary}<ret>'\n" "$copy" "$backend"
    printf "map global user -docstring 'yank to clipboard' Y '<a-|>%s<ret>:echo -markup %%{{Information}copied selection to %s clipboard}<ret>'\n" "$copy -selection clipboard" "$backend"
    printf "map global user -docstring 'replace from clipboard' R '|%s<ret>'\n" "$paste"
}
