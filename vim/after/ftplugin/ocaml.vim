setlocal tabstop=2
setlocal shiftwidth=2
setlocal softtabstop=2

if !exists("s:ocaml_conf_loaded")
    let s:ocaml_conf_loaded = 1

    let g:opambin = substitute(system('opam config var bin'),'\n$','','''')
    execute "set rtp+=" . g:opambin

    call ale#linter#Define('ocaml',{
                \ 'name':'ocaml-lsp',
                \ 'lsp': 'stdio',
                \ 'executable': 'ocamllsp',
                \ 'command': '%e',
                \ 'project_root': function('ale#handlers#ols#GetProjectRoot')
                \})
endif
