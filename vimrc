" See https://github.com/skwp/dotfiles for vundle plugins & conf ideas
" vi: set filetype=vim:

set nocompatible
filetype off

if !has("win32")
    set shell=/bin/bash
endif

set rtp+=~/.vim/bundle/Vundle.vim/
call vundle#begin()

Plugin 'gmarik/Vundle.vim'

Plugin 'ap/vim-css-color'
Plugin 'bogado/file-line'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'dag/vim-fish'
Plugin 'dense-analysis/ale'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'elixir-editors/vim-elixir'
Plugin 'ervandew/supertab'
Plugin 'FooSoft/vim-argwrap'
Plugin 'junegunn/fzf'
Plugin 'junegunn/fzf.vim'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'luochen1990/rainbow' " Rainbow Parentheses
Plugin 'mustache/vim-mustache-handlebars'
Plugin 'pangloss/vim-javascript'
Plugin 'Raimondi/delimitMate'
Plugin 'samsaga2/vim-z80'
Plugin 'Shougo/neocomplcache'
Plugin 'tpope/vim-fireplace' " Clojure
Plugin 'udalov/kotlin-vim'
Plugin 'vim-scripts/a.vim'
Plugin 'vim-scripts/asmM6502.vim'
Plugin 'vim-scripts/vim-auto-save'

call vundle#end()
filetype plugin indent on

set rtp+=/usr/local/opt/fzf
let g:fzf_preview_window = ''

let g:ctrlp_match_window = 'top,order:ttb,min:1,max:32,results:32'
let g:ctrlp_cmd = 'CtrlPMRU'
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']
let g:ctrlp_custom_ignore = { 'dir': 'node_modules' }
let g:ctrlp_clear_cache_on_exit = 0

let g:fuzzy_ignore = "*.class,*.pyc,*.log,*.o"

let g:neocomplcache_auto_completion_start_length = 3
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_underbar_completion = 1
let g:neocomplcache_max_list = 5
let g:neocomplcache_enable_insert_char_pre = 1

let g:ale_fixers = { 'javascript': ['eslint'] }
let g:ale_linters = {'c': ['clang'], 'cpp': ['clang', 'g++']}
let g:ale_cpp_cc_options = '-std=c++17 -Wall -pedantic'
let g:ale_c_cc_options = '-std=c18 -Wall -Wextra -Wpedantic -Werror -Iinclude'
let g:ale_c_parse_makefile = 0

let g:auto_save = 1
let g:auto_save_in_insert_mode = 0
let g:netrw_liststyle = 3

let g:rainbow_active = 1

let macvim_skip_colorscheme=1
colorscheme default
set background=dark

set grepprg=rg\ --vimgrep\ --sort-files\ --max-columns\ 120

""" Begin shared with root

filetype plugin indent on

let mapleader=" "

set autoindent
set autoread
set backspace=2
set colorcolumn=120
set diffopt+=iwhite
set encoding=utf-8
set expandtab
set exrc
set fileformat=unix
"set gdefault
set guioptions-=T       " Turn off useless toolbar
set guioptions-=m       " Turn off useless toolbar
set hlsearch
set hidden
set history=10000
"set ignorecase
set incsearch
set laststatus=2        " Always want statusline
set lazyredraw          " Don't display macro steps
set linebreak
set list
set listchars=tab:»\ ,trail:•
set mouse=a
set nobackup
set nocompatible
set noswapfile
set nowrap
set nowritebackup
set number
set ruler
set scrolloff=8
set secure
set sidescrolloff=15
set sidescroll=1
set shiftwidth=4
set shortmess=atoOTS
set showcmd
set showmode
set smartindent
set smarttab
set softtabstop=4
set spelllang=fr
set tabstop=4
set whichwrap+=<,>,[,]
set wildignore+=*.pyc,*.o,*.class,log/**
set wildignore+=*.bmp,*.gif,*.ico,*.jpg,*.png,*.ico
set wildignore+=*.pdf,*.psd
set wildignore+=**/node_modules/**
set wildignore+=**/.git/**
set wildignore+=**/tmp/**
set wildignore+=**/dist/**
set wildignore+=**/.idea/**
set wildmenu
set wildmode=list:longest,full

set path+=src

autocmd Filetype asm setlocal shiftwidth=8
autocmd Filetype asm setlocal softtabstop=8
autocmd Filetype asm setlocal tabstop=8
autocmd Filetype asm setlocal path+=include

autocmd Filetype z80 setlocal path+=include

autocmd Filetype clojure let b:delimitMate_quotes="\""
autocmd FileType clojure nested NeoComplCacheLock

autocmd Filetype lua setlocal tabstop=2
autocmd Filetype lua setlocal shiftwidth=2
autocmd Filetype lua setlocal softtabstop=2

autocmd Filetype markdown setlocal wrap

autocmd BufNewFile,BufRead *.muc set filetype=mucom88
autocmd Filetype mucom88 setlocal makeprg=miniplay

autocmd Filetype typescript setlocal suffixesadd+=.ts
autocmd Filetype typescript setlocal tabstop=2
autocmd Filetype typescript setlocal shiftwidth=2
autocmd Filetype typescript setlocal softtabstop=2

autocmd Filetype yaml setlocal tabstop=2
autocmd Filetype yaml setlocal shiftwidth=2
autocmd Filetype yaml setlocal softtabstop=2

autocmd Filetype asmM6502 setlocal tabstop=4
autocmd Filetype asmM6502 setlocal shiftwidth=4
autocmd Filetype asmM6502 setlocal softtabstop=4

autocmd BufNewFile,BufRead *.p8 set filetype=lua
autocmd BufNewFile,BufRead *.nx set filetype=basic

let asmM6502Regex = '^\s*processor 6502'

au BufNewFile,BufRead *.asm
    \ if (getline(1) =~? asmM6502Regex || getline(2) =~? asmM6502Regex || getline(3) =~? asmM6502Regex) |
    \   set filetype=asmM6502 |
    \ else |
    \   set filetype=asm |
    \ endif


syntax on

""" End shared with root

:au FocusLost * silent! wa

nmap <leader>b :Buffers<CR>
nmap <leader>p :Files<CR>
nmap <leader>g :GFiles<CR>
nmap <leader>h :History<CR>
nmap <leader>: :History:<CR>
nmap <leader>/ :History/<CR>
nmap <leader>e :Explore<CR>
nmap <leader>f :ALEFix<CR>
nmap <leader>n :ALENext<CR>
nmap <leader>d :ijump <C-R><C-W><CR>
vmap <leader>c "*y

nmap <leader>t :let lastTestFile=expand('%')<CR>:silent make % <bar> redraw!<CR>:cwindow<CR>
nmap <leader>l :execute ':make ' . lastTestFile<CR>

imap jk <esc>
cmap jk <esc>

cmap <C-A> <Home>

autocmd VimEnter * iunmap <leader>ih
autocmd VimEnter * iunmap <leader>ihn
autocmd VimEnter * iunmap <leader>is

digraph oo 9702 " WHITE BULLET 0x25E6 digraph

" if &term =~ '256color'
"     " Disable Background Color Erase (BCE) so that color schemes
"     " work properly when Vim is used inside tmux and GNU screen.
"     " See also http://snk.tuxfamily.org/log/vim-256color-bce.html
"     set t_ut=
" endif
" 
" if has("gui_running")
"     if has("win32")
"         autocmd GUIEnter * simalt ~n
"     else
"         set lines=999 columns=999
"     endif
" endif
