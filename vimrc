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

Plugin 'altercation/vim-colors-solarized'
" Note: with powerline, you need one of these too:
" https://github.com/ashwin/vim-powerline/tree/develop/font
" (works on PuTTY)
Plugin 'bling/vim-airline'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'jnurmine/Zenburn'
Plugin 'scrooloose/nerdtree'
Plugin 'ervandew/supertab'
Plugin 'tpope/vim-fugitive'
Plugin 'Raimondi/delimitMate'
Plugin 'Shougo/neocomplcache'
Plugin 'bogado/file-line'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'dag/vim-fish'
Plugin 'mxw/vim-jsx'
Plugin 'lambdatoast/elm.vim'
Plugin 'vim-scripts/vim-auto-save'
Plugin 'editorconfig/editorconfig-vim'

call vundle#end()
filetype plugin indent on

let g:ctrlp_match_window = 'top,order:ttb,max:20'
let g:ctrlp_cmd = 'CtrlPMRU'
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']
let g:fuzzy_ignore = "*.class,*.pyc,*.log,*.o"
let g:Powerline_symbols = 'fancy'
let g:neocomplcache_auto_completion_start_length = 3
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_underbar_completion = 1
let g:neocomplcache_max_list = 5
let g:neocomplcache_enable_insert_char_pre = 1
let g:airline_powerline_fonts = 1
let g:auto_save = 1
let g:auto_save_in_insert_mode = 0

let g:netrw_liststyle = 3

let NERDTreeIgnore = ['\.pyc$', '\.o$', '\.class$']
let NERDTreeMinimalUI=1
set guifont=Meslo\ LG\ S\ Regular\ for\ Powerline:h14;DejaVu\ Sans\ Mono\ for\ Powerline
set grepprg=rg\ --vimgrep\ --sort-files\ --max-columns\ 120

""" Begin shared with root

filetype plugin indent on

let mapleader=" "

set autoindent
set autoread
set backspace=2
set background=dark
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
autocmd Filetype markdown setlocal wrap

autocmd Filetype yaml setlocal tabstop=2
autocmd Filetype yaml setlocal shiftwidth=2
autocmd Filetype yaml setlocal softtabstop=2

autocmd BufNewFile,BufRead *.p8 set filetype=lua
autocmd BufNewFile,BufRead *.p8 setlocal tabstop=2
autocmd BufNewFile,BufRead *.p8 setlocal shiftwidth=2
autocmd BufNewFile,BufRead *.p8 setlocal softtabstop=2

syntax on

""" End shared with root

:au FocusLost * silent! wa

nmap <leader>e :Explore<CR>
nmap <leader>t :NERDTreeToggle<CR>
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

