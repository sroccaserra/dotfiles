add-highlighter global/ show-matching

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
      exec <esc>
}}

hook global RegisterModified '/' %{
    add-highlighter -override global/search regex "%reg{/}" 0:+u
}
