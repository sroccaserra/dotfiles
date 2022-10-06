add-highlighter global/ number-lines
colorscheme gruvbox-dark

map global insert <c-left> '<a-;>b'
map global insert <c-right> '<a-;>e'

hook global InsertChar ù %{ try %{
      exec -draft hH <a-k>ùù<ret> d
      exec <esc>
}}
