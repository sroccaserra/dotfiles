" vi: set filetype=vim:

set nocompatible
language en_US.UTF-8
set shell=/usr/bin/env\ bash

call plug#begin()

Plug '907th/vim-auto-save'
Plug 'bogado/file-line'
Plug 'christoomey/vim-tmux-navigator'
Plug 'dag/vim-fish', { 'for': 'fish' }
Plug 'editorconfig/editorconfig-vim'
Plug 'elixir-editors/vim-elixir', { 'for': 'elixir' }
Plug 'ervandew/supertab'
Plug 'gruvbox-community/gruvbox'
Plug 'itchyny/vim-haskell-indent', { 'for': 'haskell' }
Plug 'jgdavey/tslime.vim'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'junegunn/vim-emoji'
Plug 'karolbelina/uxntal.vim'
Plug 'leafgarland/typescript-vim'
Plug 'mustache/vim-mustache-handlebars'
Plug 'Raimondi/delimitMate'
Plug 'samsaga2/vim-z80'
" Plug 'takac/vim-hardtime'
Plug 'tmux-plugins/vim-tmux-focus-events'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'vim-scripts/a.vim'
Plug 'vim-scripts/asmM6502.vim'
Plug 'vim-test/vim-test'

if has('nvim')
    Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
    Plug 'nvim-treesitter/nvim-treesitter-context'
    Plug 'neovim/nvim-lspconfig'
    Plug 'nvim-lua/plenary.nvim'
    Plug 'nvim-telescope/telescope.nvim'
    Plug 'ThePrimeagen/harpoon'
else
    Plug 'dense-analysis/ale'
    Plug 'fatih/vim-go', { 'for': 'go' } ", { 'do': ':GoUpdateBinaries' }
    Plug 'pangloss/vim-javascript', { 'for': 'javascript' }
endif

call plug#end()

set rtp+=/usr/local/opt/fzf
set rtp+=$HOME/dotfiles/vim

function! s:build_quickfix_list(lines)
  call setqflist(map(copy(a:lines), '{ "filename": v:val }'))
  copen
  cc
endfunction

let g:fzf_action = {
  \ 'ctrl-q': function('s:build_quickfix_list'),
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }
let g:fzf_preview_window = ''

if !has('nvim')
    let g:ale_linters = { 'c': ['clang'], 'cpp': ['clang', 'g++'], 'elixir': ['elixir-ls'], 'go': ['gopls'] }
    let g:ale_fixers = { 'elixir': ['mix_format'] }


    let g:ale_cpp_cc_options = '-std=c++17 -Wall -pedantic'
    let g:ale_c_cc_options = '-std=c18 -Wall -Wextra -Wpedantic -Werror -Iinclude'
    let g:ale_c_parse_makefile = 0
endif

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

let g:go_fmt_fail_silently = 1

set completefunc=emoji#complete

packadd! matchit

if has('nvim')
    colorscheme gruvbox
else
    let macvim_skip_colorscheme=1
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
    colorscheme default
endif
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
set cursorline
set cursorlineopt=number
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
set listchars=tab:\|\ ,trail:•,extends:>,precedes:<,nbsp:+
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

    autocmd Filetype asm setlocal shiftwidth=4
    autocmd Filetype asm setlocal softtabstop=4
    autocmd Filetype asm setlocal tabstop=4
    autocmd Filetype asm setlocal path+=include

    autocmd Filetype asmM6502 setlocal tabstop=8
    autocmd Filetype asmM6502 setlocal shiftwidth=8
    autocmd Filetype asmM6502 setlocal softtabstop=8

    autocmd Filetype z80 setlocal path+=include

    if has('nvim')
        autocmd BufNewFile,BufRead *.fs setlocal filetype=forth
        autocmd Filetype go setlocal noexpandtab
    endif

    autocmd Filetype help nnoremap <buffer> gd <C-]>

    autocmd Filetype clojure let b:delimitMate_quotes='"'
    autocmd Filetype scheme let b:delimitMate_quotes='"'
    autocmd Filetype scheme setlocal lispwords+=library

    autocmd FileType xml,html,html.handlebars let b:delimitMate_matchpairs = "(:),[:],{:}"

    autocmd Filetype markdown setlocal wrap
    autocmd Filetype ale-preview setlocal wrap

    autocmd BufNewFile,BufRead *.muc set filetype=mucom88
    autocmd Filetype mucom88 setlocal makeprg=miniplay

    autocmd Filetype yaml setlocal tabstop=2
    autocmd Filetype yaml setlocal shiftwidth=2
    autocmd Filetype yaml setlocal softtabstop=2

    autocmd BufNewFile,BufRead *.p8 set filetype=lua
    autocmd BufNewFile,BufRead *.nx set filetype=basic

    autocmd BufNewFile,BufRead *.tal set filetype=uxntal

    autocmd BufNewFile,BufRead *.ex set filetype=elixir
    autocmd BufNewFile,BufRead *.exs set filetype=elixir

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

nnoremap ù :noh<CR>
nnoremap <BS> <C-^>
nnoremap <leader>/ :History/<CR>
nnoremap <leader>: :History:<CR>
nnoremap <leader>B vaBV
vnoremap <leader>B aBV
nnoremap <leader>b :Buffer<CR>
nnoremap <Leader>c :let @+=expand('%')<CR>
"nnoremap <leader>e :Explore<CR>
if has('nvim')
    nnoremap <leader>ff <cmd>Telescope find_files<cr>
    nnoremap <leader>fg <cmd>Telescope live_grep<cr>
    nnoremap <leader>fb <cmd>Telescope buffers<cr>
    nnoremap <leader>fh <cmd>Telescope help_tags<cr>
endif
nnoremap <leader>zf :call fzf#vim#files('.', { 'source': 'rg --files ' })<CR>
nnoremap <leader>zg :GFiles<CR>
nnoremap <leader>zh :History<CR>
" nnoremap <leader>j :cnext<CR>zzzv
" nnoremap <leader>k :cprevious<CR>zzzv
nnoremap <leader>p :Lexplore %:p:h<CR>
nnoremap <leader>r *Ncgn
nnoremap <leader>s :GFiles?<CR>
nnoremap <leader>v :set paste<CR>mvo<C-R>+<ESC>'vj:set nopaste<CR>
" nnoremap <leader>z :tabnew %<CR>

if has('nvim')
    nnoremap <leader><leader>g <cmd>Telescope live_grep<cr>

    nnoremap <leader><leader>c <Cmd>lua vim.lsp.buf.code_action()<CR>
    vnoremap <leader><leader>c <Cmd>lua vim.lsp.buf.code_action()<CR>
    nnoremap <leader><leader>D <Cmd>lua vim.diagnostic.open_float()<CR>
    nnoremap <leader><leader>d <Cmd>lua vim.lsp.buf.definition()<CR>
    nnoremap <leader><leader>F <Cmd>lua vim.lsp.buf.references()<CR>
    nnoremap <leader><leader>f <Cmd>lua vim.lsp.buf.formatting()<CR>
    nnoremap <leader><leader>h <Cmd>lua vim.lsp.buf.hover()<CR>
    nnoremap <leader><leader>i <Cmd>lua vim.lsp.buf.implementation()<CR>
    nnoremap <leader><leader>n <Cmd>lua vim.diagnostic.goto_next()<CR>
    nnoremap <leader><leader>r <Cmd>lua vim.lsp.buf.rename()<CR>
    nnoremap <leader><leader>s <Cmd>lua vim.lsp.buf.signature_help()<CR>

    nnoremap <silent> <leader>h <Cmd>lua require("harpoon.ui").toggle_quick_menu()<CR>
    nnoremap <silent> <leader>a <Cmd>lua require("harpoon.mark").add_file()<CR>
    nnoremap <silent> <leader>j <Cmd>lua require("harpoon.ui").nav_file(1)<CR>
    nnoremap <silent> <leader>k <Cmd>lua require("harpoon.ui").nav_file(2)<CR>
    nnoremap <silent> <leader>l <Cmd>lua require("harpoon.ui").nav_file(3)<CR>
    nnoremap <silent> <leader>m <Cmd>lua require("harpoon.ui").nav_file(4)<CR>
else
    nnoremap <leader><leader>c :ALECodeAction<CR>
    nnoremap <leader><leader>d :ALEGoToDefinition<CR>
    nnoremap <leader><leader>D :ALEDetail<CR>
    nnoremap <leader><leader>F :ALEFindReferences -relative<CR>
    nnoremap <leader><leader>f :ALEFix<CR>
    nnoremap <leader><leader>n :ALENext<CR>
    nnoremap <leader><leader>r :ALERename<CR>

    vnoremap <leader><leader>c :ALECodeAction<CR>
endif

vnoremap <leader>c "+y
vnoremap <leader>p "_dP

nnoremap <leader>e :s/:\([^:]\+\):/\=emoji#for(submatch(1), submatch(0))/g<CR>
vnoremap <leader>e :s/:\([^:]\+\):/\=emoji#for(submatch(1), submatch(0))/g<CR>

nnoremap <leader>tt :TestNearest<CR>
nnoremap <leader>tl :TestLast<CR>

" nnoremap <leader>t :let LAST_TEST_FILE=expand('%')<CR>:silent make % <bar> redraw!<CR>:cwindow<CR>
" nnoremap <leader>l :execute ':make ' . LAST_TEST_FILE<CR>

nnoremap <leader>' :execute 'buffer' getpos("'" . nr2char(getchar()) )[0]<CR>

" 11 letter pairs that don't appear adjoined in either direction:
" bq, cj, cv, fz, gq, jq, jv, jx, kq, pq and xz
" Or: just use Ctrl-[ (on azerty keyboard, Ctrl-^ acts like Ctrl-[ or <Esc>)
inoremap ùù <Esc>
cnoremap ùù <Esc>
" inoremap gq <Esc>
" inoremap qg <Esc>
" cnoremap gq <Esc>
" cnoremap qg <Esc>
" inoremap sf <esc>
" cnoremap sf <esc>
" inoremap <Space><Space> <Esc>
" cnoremap <Space><Space> <Esc>

" Undo break points
inoremap <C-U> <C-G>u<C-U>
inoremap <C-W> <C-G>u<C-W>
" inoremap , ,<C-G>u
" inoremap . .<C-G>u
" inoremap ! !<C-G>u
" inoremap ? ?<C-G>u

inoremap <silent> <C-l> <Nop>

cnoremap <C-A> <Home>

iabbr ajd <C-R>=strftime("%Y-%m-%d")<CR>

" iabbr :bulb: 💡
" iabbr :check: ✅
" iabbr :circle: ⭕
" iabbr :construction: 🚧
" iabbr :cross: ❌
" iabbr :crying: 😢
" iabbr :eyes: 👀
" iabbr :facepalm: 🤦
" iabbr :fear: 😨
" iabbr :fire: 🔥
" iabbr :memo: 📝
" iabbr :recycle: ♻️
" iabbr :smile: 🙂
" iabbr :surprise: 😮
" iabbr :tada: 🎉
" iabbr :thinking: 🤔
" iabbr :warning: ⚠️

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

if &term =~ '^screen'
    " tmux will send xterm-style keys when its xterm-keys option is on
    execute "set <xUp>=\e[1;*A"
    execute "set <xDown>=\e[1;*B"
    execute "set <xRight>=\e[1;*C"
    execute "set <xLeft>=\e[1;*D"
endif
