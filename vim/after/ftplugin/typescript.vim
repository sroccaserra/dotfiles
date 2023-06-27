setlocal suffixesadd+=.ts,.tsx,.js,.jsx,.d.ts
setlocal tabstop=2
setlocal shiftwidth=2
setlocal softtabstop=2
setlocal makeprg=NODE_ENV=test\ npx\ mocha\ --exit "\ --color
setlocal errorformat=%E%.%#Error:\ %m,%Z%*[^(](%f:%l:%c),%-G%.%#
setlocal path+=src,src/client
setlocal includeexpr=substitute(v:fname,'^/','','')

" Note: trigger these abbrevs with a ', like dd' as the quote will stay after
" the expension.
iabbr <buffer> dd describe(, function() {<CR>});<esc>kela
iabbr <buffer> ii it(, function() {<CR>});<esc>ka

let g:ale_fixers = { 'typescript': ['eslint'], 'typescriptreact': ['eslint'] }

if !has('nvim')
    noremap <buffer> <leader>a :call mylib#FindFileOrTestFile()<CR>
endif
noremap <buffer> <leader>z :g/^\s*it\>/normal jvaBzf<CR>
noremap K :ALEHover<CR>
