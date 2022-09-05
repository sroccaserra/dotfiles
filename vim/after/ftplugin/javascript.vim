setlocal suffixesadd+=.js,.ts
setlocal tabstop=2
setlocal shiftwidth=2
setlocal softtabstop=2
setlocal makeprg=NODE_ENV=test\ npx\ mocha\ --exit "\ --color
setlocal errorformat=%E%.%#Error:\ %m,%Z%*[^(](%f:%l:%c),%-G%.%#

" Note: trigger these abbrevs with a ', like dd' as the quote will stay after
" the expension.
iabbr <buffer> dd describe(, function() {<CR>});<esc>kela
iabbr <buffer> ii it(, function() {<CR>});<esc>ka

iabbr <buffer> DD describe(, () => {<CR>});<esc>kela
iabbr <buffer> II it(, () => {<CR>});<esc>ka

setlocal include=\\<require(.\\zs[^'\\"]\\+\\ze

let g:ale_fixers = { 'javascript': ['eslint'] }
let g:ale_linters = { 'javascript': ['eslint', 'tsserver'] }

function! FindFileOrTestFile()
    let fileName = substitute(expand('%:r'), '.*/', '', '')
    let testSuffixRegex = '[_-]\?[tT]est$'
    if (fileName =~ testSuffixRegex)
        let fuzzyQuery = '''' . substitute(fileName, testSuffixRegex, '', '')
    else
        let fuzzyQuery = '''' . fileName . ' ''test'
    endif
    call fzf#vim#files('.', { 'source': "rg -l ''", 'options': '--query "' . fuzzyQuery . '"'})
endfunction

if !has('nvim')
    noremap <buffer> <leader>a :call FindFileOrTestFile()<CR>
endif
noremap <buffer> <leader>z :g/^\s*it\>/normal jvaBzf<CR>
noremap <buffer> K :ALEHover<CR>
