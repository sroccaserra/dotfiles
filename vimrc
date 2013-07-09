execute pathogen#infect()
let g:fuzzy_ignore = "*.class,*.pyc,*.log,*.o"
let NERDTreeIgnore = ['\.pyc$', '\.o$', '\.class$']
let NERDTreeDirArrows = 1
colorscheme zenburn

""" Shared with root

filetype plugin indent on
set autoindent
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
set nopaste
set noswapfile
set nowrap
set nowritebackup
set number
set ruler
set shiftwidth=4
set smartindent
set smarttab
set softtabstop=4
set tabstop=4
set viminfo+=%
set wildignore+=*.pyc,*.o,*.class
set wildmenu
set wildmode=list:longest,full
syntax on

