setlocal suffixesadd+=.js
setlocal tabstop=2
setlocal shiftwidth=2
setlocal softtabstop=2
setlocal makeprg=NODE_ENV=test\ npx\ mocha\ --exit "\ --color
setlocal errorformat=%E%.%#Error:\ %m,%Z%*[^(](%f:%l:%c),%-G%.%#

iabbr dd describe(, () => {<CR>});<esc>kela
iabbr ii it(, () => {<CR>});<esc>ka

setlocal include=\\<require(.\\zs[^'\\"]\\+\\ze

function! AlternateJSFile()
  let fileName = substitute(expand('%:r'), '.*/', '', '')
  return (fileName =~ "_test") ? substitute(fileName, '_test$', '', '') : fileName.'_test'
endfunction

noremap <leader>a :call fzf#vim#gitfiles('.', {'options': '--query '.AlternateJSFile()})<CR>
