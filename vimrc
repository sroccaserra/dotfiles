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

Plugin 'Lokaltog/vim-easymotion'
Plugin 'ervandew/supertab'
Plugin 'Raimondi/delimitMate'
Plugin 'Shougo/neocomplcache'
Plugin 'bogado/file-line'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'dag/vim-fish'
Plugin 'vim-scripts/vim-auto-save'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'mustache/vim-mustache-handlebars'
Plugin 'w0rp/ale'
Plugin 'pangloss/vim-javascript'
Plugin 'FooSoft/vim-argwrap'
Plugin 'tpope/vim-fireplace' " Clojure
Plugin 'luochen1990/rainbow' " Rainbow Parentheses

call vundle#end()
filetype plugin indent on

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
autocmd FileType clojure nested NeoComplCacheLock

let g:ale_fixers = { 'javascript': ['eslint'] }
let g:ale_linters = {'c': ['clang'], 'cpp': ['clang', 'g++']}
let g:ale_cpp_clang_options = '-std=c++17 -Wall -pedantic'
let g:ale_cpp_gcc_options = '-std=c++17 -Wall -pedantic'

let g:auto_save = 1
let g:auto_save_in_insert_mode = 0
let g:netrw_liststyle = 3

let g:rainbow_active = 1

let macvim_skip_colorscheme=1
colorscheme default
set background=dark
set noantialias

set grepprg=rg\ --vimgrep\ --sort-files\ --max-columns\ 120

""" Begin shared with root

filetype plugin indent on

let mapleader=" "

set autoindent
set autoread
set backspace=2
set diffopt+=iwhite
set encoding=utf-8
set expandtab
set fileformat=unix
set gdefault
set guioptions-=T       " Turn off useless toolbar
set guioptions-=m       " Turn off useless toolbar
set hlsearch
set ignorecase
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
set wildmenu
set wildmode=list:longest,full

autocmd Filetype asm setlocal shiftwidth=8
autocmd Filetype asm setlocal softtabstop=8
autocmd Filetype asm setlocal tabstop=8

autocmd Filetype javascript setlocal suffixesadd=.js
autocmd Filetype typescript setlocal suffixesadd=.ts
autocmd Filetype markdown setlocal wrap

autocmd Filetype yaml setlocal tabstop=2
autocmd Filetype yaml setlocal shiftwidth=2
autocmd Filetype yaml setlocal softtabstop=2

autocmd Filetype typescript setlocal tabstop=2
autocmd Filetype typescript setlocal shiftwidth=2
autocmd Filetype typescript setlocal softtabstop=2

autocmd Filetype javascript setlocal tabstop=2
autocmd Filetype javascript setlocal shiftwidth=2
autocmd Filetype javascript setlocal softtabstop=2

autocmd Filetype lua setlocal tabstop=2
autocmd Filetype lua setlocal shiftwidth=2
autocmd Filetype lua setlocal softtabstop=2

autocmd BufNewFile,BufRead *.p8 set filetype=lua
autocmd BufNewFile,BufRead *.nx set filetype=basic

syntax on

""" End shared with root

:au FocusLost * silent! wa

nmap <leader>e :Explore<CR>
digraph oo 9702 " WHITE BULLET 0x25E6 digraph

if &term =~ '256color'
    " Disable Background Color Erase (BCE) so that color schemes
    " work properly when Vim is used inside tmux and GNU screen.
    " See also http://snk.tuxfamily.org/log/vim-256color-bce.html
    set t_ut=
endif

if has("gui_running")
    if has("win32")
        autocmd GUIEnter * simalt ~n
    else
        set lines=999 columns=999
    endif
endif

