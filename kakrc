add-highlighter global/ show-matching
add-highlighter global/ show-whitespaces

colorscheme gruvbox-dark

map global insert <c-left> '<a-;>b'
map global insert <c-right> '<a-;>e'

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
}}

hook global RegisterModified '/' %{
    add-highlighter -override global/search regex "%reg{/}" 0:+u
}

hook global WinSetOption filetype=markdown %{
    add-highlighter window/ wrap -word
}
