execute pathogen#infect()

let g:fuzzy_ignore = "*.class,*.pyc,*.log,*.o"

let NERDTreeIgnore = ['\.pyc$', '\.o$', '\.class$']
nmap <leader>d :NERDTreeToggle<CR>

""" Begin shared with root

filetype plugin indent on

let mapleader=","

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
set tabstop=4
set viminfo+=%
set wildignore+=*.pyc,*.o,*.class,log/**
set wildmenu
set wildmode=list:longest,full

syntax on

""" End shared with root

colorscheme zenburn

