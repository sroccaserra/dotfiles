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

function! AlternateJSFile()
  let fileName = substitute(expand('%:r'), '.*/', '', '')
  let testSuffixRegex = '[_-]test$'
  return substitute(fileName, testSuffixRegex, '', '')
endfunction

function! AlternateJSTestFile()
  let fileName = substitute(expand('%:r'), '.*/', '', '')
  let testSuffixRegex = '[_-]test$'
  let testFilePattern = '"'''.fileName.' ''test"'
  return (fileName =~ testSuffixRegex) ? substitute(fileName, testSuffixRegex, '', '') : testFilePattern
endfunction

noremap <buffer> <leader>a :call fzf#vim#gitfiles('.', {'options': '--query '.AlternateJSFile()})<CR>
noremap <buffer> <leader>z :g/^\s*it\>/normal jvaBzf<CR>
noremap K :ALEHover<CR>
