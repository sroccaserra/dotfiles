function! AlternateFile()
  return substitute(expand('%:r'), '.*/', '', '')
endfunction

noremap <buffer> <leader>a :call fzf#vim#gitfiles('.', {'options': '--query '.AlternateFile()})<CR>
