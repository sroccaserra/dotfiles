setlocal suffixesadd+=.js,.ts
setlocal tabstop=2
setlocal shiftwidth=2
setlocal softtabstop=2
setlocal makeprg=NODE_ENV=test\ npx\ mocha\ --exit "\ --color
setlocal errorformat=%E%.%#Error:\ %m,%Z%*[^(](%f:%l:%c),%-G%.%#

" Note: trigger these abbrevs with a ', like dd' as the quote will stay after
" the expension.
iabbr <buffer> dd describe(, () => {<CR>});<esc>kela
iabbr <buffer> ii it(, () => {<CR>});<esc>ka

setlocal include=\\<require(.\\zs[^'\\"]\\+\\ze

let g:ale_fixers = { 'javascript': ['eslint'] }

function! AlternateJSFile()
  let fileName = substitute(expand('%:r'), '.*/', '', '')
  let testSuffixRegex = '[_-]test$'
  let testFilePattern = '"'''.fileName.' ''test"'
  return (fileName =~ testSuffixRegex) ? substitute(fileName, testSuffixRegex, '', '') : testFilePattern
endfunction

noremap <buffer> <leader>a :call fzf#vim#gitfiles('.', {'options': '--query '.AlternateJSFile()})<CR>
noremap <buffer> <leader>z :g/^\s*it\>/normal jvaBzf<CR>
