" See https://github.com/skwp/dotfiles for vundle plugins & conf ideas
" vi: set filetype=vim:

set nocompatible
filetype off
language en_US.UTF-8

if !has('win32')
    set shell=/usr/bin/env\ bash
endif

set rtp+=~/.vim/bundle/Vundle.vim/
call vundle#begin()

Plugin 'gmarik/Vundle.vim'

Plugin '907th/vim-auto-save'
Plugin 'bogado/file-line'
Plugin 'christoomey/vim-tmux-navigator'
Plugin 'dag/vim-fish'
Plugin 'dense-analysis/ale'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'ervandew/supertab'
Plugin 'itchyny/vim-haskell-indent'
Plugin 'jgdavey/tslime.vim'
Plugin 'junegunn/fzf'
Plugin 'junegunn/fzf.vim'
Plugin 'leafgarland/typescript-vim'
"Plugin 'luochen1990/rainbow' " Rainbow Parentheses
Plugin 'mustache/vim-mustache-handlebars'
Plugin 'ocaml/vim-ocaml'
Plugin 'pangloss/vim-javascript'
Plugin 'Raimondi/delimitMate'
Plugin 'samsaga2/vim-z80'
" Plugin 'takac/vim-hardtime'
Plugin 'tmux-plugins/vim-tmux-focus-events'
Plugin 'tpope/vim-abolish'
Plugin 'tpope/vim-fireplace' " Clojure
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'vim-scripts/a.vim'
Plugin 'vim-scripts/asmM6502.vim'
Plugin 'vim-test/vim-test'

call vundle#end()
filetype plugin indent on

set rtp+=/usr/local/opt/fzf
let g:fzf_preview_window = ''

let g:ale_linters = { 'c': ['clang'], 'cpp': ['clang', 'g++'] } ", 'ocaml': ['ocaml-lsp'] }

let g:ale_cpp_cc_options = '-std=c++17 -Wall -pedantic'
let g:ale_c_cc_options = '-std=c18 -Wall -Wextra -Wpedantic -Werror -Iinclude'
let g:ale_c_parse_makefile = 0

let g:ale_java_javac_sourcepath = '.:src:test'
let g:ale_java_javac_classpath = getcwd() . '/junit.jar:' . getcwd() . '/lib/junit.jar'

let g:auto_save = 1
let g:auto_save_in_insert_mode = 0
let g:hardtime_default_on = 1

" let g:netrw_altv = 1
" let g:netrw_banner = 0
" let g:netrw_browse_split = 4
let g:netrw_liststyle = 3
let g:netrw_winsize = 25

let g:rainbow_active = 1

let test#strategy = "tslime"

let g:tslime_always_current_session = 1
let g:tslime_always_current_window = 1

packadd! matchit

if !exists('colorscheme_autocomd_loaded')
    let colorscheme_autocmd_loaded = 1
    augroup MyColors
        autocmd!
        autocmd ColorScheme *
                    \ highlight ColorColumn ctermbg=236 guibg=#393939 |
                    \ highlight Folded ctermbg=235 guibg=#393939 |
                    \ highlight clear CursorLineNr |
                    \ highlight CursorLineNr ctermbg=darkgray
    augroup END
endif
let macvim_skip_colorscheme=1
colorscheme default
set background=dark

set grepprg=rg\ --vimgrep\ --sort-files\ --max-columns\ 120

""" Begin shared with root

filetype plugin indent on

let mapleader=' '

set autoindent
set autoread
set backspace=indent,eol,start
set colorcolumn=120
set complete-=i
if !has('nvim')
    set cursorline
    set cursorlineopt=number
endif
set diffopt+=iwhite
set display+=lastline
set encoding=utf-8
set expandtab
" set exrc
set fileformat=unix
set formatoptions+=j
set guioptions-=T       " Turn off useless toolbar
set guioptions-=m       " Turn off useless toolbar
set hidden
set history=1000
if has('nvim')
    set inccommand=nosplit
endif
set incsearch
set langmenu=en_US.UTF-8
set laststatus=2        " Always want statusline
set lazyredraw          " Don't display macro steps
set linebreak
set list
set listchars=tab:>\ ,trail:•,extends:>,precedes:<,nbsp:+
set mouse=a
set nobackup
set noswapfile
set nowrap
set nowritebackup
set nrformats-=octal
set number
set ruler
set scrolloff=1
set secure
set sessionoptions-=options
set sidescroll=1
set sidescrolloff=5
set shiftwidth=4
set shortmess=atoOTS
set showcmd
set showmode
set smartindent
set smarttab
set softtabstop=4
set spelllang=fr
set tabpagemax=50
set tabstop=4
if !has('nvim') && &ttimeoutlen == -1
    set ttimeout
    set ttimeoutlen=100
endif
set viewoptions-=options
set viminfo^=!
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

if !exists('autocmd_loaded')
    let autocmd_loaded = 1

    autocmd Filetype asm setlocal shiftwidth=8
    autocmd Filetype asm setlocal softtabstop=8
    autocmd Filetype asm setlocal tabstop=8
    autocmd Filetype asm setlocal path+=include

    autocmd Filetype z80 setlocal path+=include

    autocmd Filetype clojure let b:delimitMate_quotes='"'

    autocmd FileType xml,html,html.handlebars let b:delimitMate_matchpairs = "(:),[:],{:}"

    autocmd Filetype lua setlocal tabstop=2
    autocmd Filetype lua setlocal shiftwidth=2
    autocmd Filetype lua setlocal softtabstop=2

    autocmd Filetype markdown setlocal wrap

    autocmd BufNewFile,BufRead *.muc set filetype=mucom88
    autocmd Filetype mucom88 setlocal makeprg=miniplay

    autocmd Filetype yaml setlocal tabstop=2
    autocmd Filetype yaml setlocal shiftwidth=2
    autocmd Filetype yaml setlocal softtabstop=2

    autocmd Filetype asmM6502 setlocal tabstop=8
    autocmd Filetype asmM6502 setlocal shiftwidth=8
    autocmd Filetype asmM6502 setlocal softtabstop=8

    autocmd BufNewFile,BufRead *.p8 set filetype=lua
    autocmd BufNewFile,BufRead *.nx set filetype=basic

    autocmd BufNewFile,BufRead *.tal set filetype=uxntal

    let asmM6502Regex = '^\s*processor 6502'

    autocmd BufNewFile,BufRead *.asm
                \ if (getline(1) =~? asmM6502Regex || getline(2) =~? asmM6502Regex || getline(3) =~? asmM6502Regex) |
                \   set filetype=asmM6502 |
                \ else |
                \   set filetype=asm |
                \ endif

    autocmd FocusLost * silent! wa
endif

syntax on

""" End shared with root

nnoremap n nzzzv
nnoremap N Nzzzv
nnoremap * *zzzv
nnoremap J mzJ`z
nnoremap <C-D> <C-D>zz
nnoremap <expr> k (v:count > 5 ? "m'" . v:count : "") . 'k'
nnoremap <expr> j (v:count > 5 ? "m'" . v:count : "") . 'j'

vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

nnoremap <leader>/ :History/<CR>
nnoremap <leader>: :History:<CR>
nnoremap <leader>B vaBV
vnoremap <leader>B aBV
nnoremap <leader>b :Buffer<CR>
nnoremap <Leader>c :let @+=expand('%')<CR>
nnoremap <leader>e :Explore<CR>
nnoremap <leader>g :GFiles<CR>
nnoremap <leader>h :History<CR>
nnoremap <leader>j :cnext<CR>zzzv
nnoremap <leader>k :cprevious<CR>zzzv
nnoremap <leader>p :Lexplore %:p:h<CR>
nnoremap <leader>r *Ncgn
nnoremap <leader>s :GFiles?<CR>
nnoremap <leader>v :r !xclip -selection clipboard -o<CR>
nnoremap <leader>z :tabnew %<CR>

nnoremap <leader><leader>c :ALECodeAction<CR>
nnoremap <leader><leader>d :ALEGoToDefinition<CR>
nnoremap <leader><leader>D :ALEDetail<CR>
nnoremap <leader><leader>F :ALEFindReferences -relative<CR>
nnoremap <leader><leader>f :ALEFix<CR>
nnoremap <leader><leader>n :ALENext<CR>
nnoremap <leader><leader>r :ALERename<CR>

vnoremap <leader><leader>c :ALECodeAction<CR>

vnoremap <leader>c "+y
vnoremap <leader>p "_dP

nnoremap <leader>t :TestNearest<CR>
nnoremap <leader>l :TestLast<CR>

" nnoremap <leader>t :let LAST_TEST_FILE=expand('%')<CR>:silent make % <bar> redraw!<CR>:cwindow<CR>
" nnoremap <leader>l :execute ':make ' . LAST_TEST_FILE<CR>

nnoremap <leader>' :execute 'buffer' getpos("'" . nr2char(getchar()) )[0]<cr>

inoremap jk <esc>
cnoremap jk <esc>

" Undo break points
inoremap <C-U> <C-G>u<C-U>
inoremap <C-W> <C-G>u<C-W>
" inoremap , ,<C-G>u
" inoremap . .<C-G>u
" inoremap ! !<C-G>u
" inoremap ? ?<C-G>u

cnoremap <C-A> <Home>

iabbr ajd <C-R>=strftime("%Y-%m-%d")<CR>

autocmd VimEnter * iunmap <leader>ih
autocmd VimEnter * iunmap <leader>ihn
autocmd VimEnter * iunmap <leader>is

xnoremap @ :<C-u>call ExecuteMacroOverVisualRange()<CR>

" Not needed, use this instead?
" :'<,'>normal @q
" :'<,'>normal! @q
function! ExecuteMacroOverVisualRange()
    echo "@".getcmdline()
    execute ":'<,'>normal @".nr2char(getchar())
endfunction

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

let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
let &t_SR = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=2\x7\<Esc>\\"
let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"

if has("autocmd")
    au VimEnter,InsertLeave * silent execute '!echo -ne "\e[1 q"' | redraw!
    au InsertEnter,InsertChange *
      \ if v:insertmode == 'i' |
      \   silent execute '!echo -ne "\e[5 q"' | redraw! |
      \ elseif v:insertmode == 'r' |
      \   silent execute '!echo -ne "\e[3 q"' | redraw! |
      \ endif
    au VimLeave * silent execute '!echo -ne "\e[ q"' | redraw!
endif
