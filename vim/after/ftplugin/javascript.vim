setlocal suffixesadd+=.js,.ts
setlocal tabstop=2
setlocal shiftwidth=2
setlocal softtabstop=2
setlocal makeprg=NODE_ENV=test\ npx\ mocha\ --exit "\ --color
setlocal errorformat=%E%.%#Error:\ %m,%Z%*[^(](%f:%l:%c),%-G%.%#

" Note: trigger these abbrevs with a ', like dd' as the quote will stay after
" the expension.
iabbr dd describe(, () => {<CR>});<esc>kela
iabbr ii it(, () => {<CR>});<esc>ka

setlocal include=\\<require(.\\zs[^'\\"]\\+\\ze

function! AlternateJSFile()
  let fileName = substitute(expand('%:r'), '.*/', '', '')
  let testSuffixRegex = '[_-]test$'
  let testFilePattern = '"'''.fileName.' ''test"'
  return (fileName =~ testSuffixRegex) ? substitute(fileName, testSuffixRegex, '', '') : testFilePattern
endfunction

noremap <leader>a :call fzf#vim#gitfiles('.', {'options': '--query '.AlternateJSFile()})<CR>
noremap <leader>z :g/^\s*it\>/normal jvaBzf<CR>
