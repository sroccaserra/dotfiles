setlocal suffixesadd+=.hs
setlocal path+=src

setlocal tabstop=2
setlocal shiftwidth=2
setlocal softtabstop=2

" usage: end the ii abrev with a <CR>
iabbr <buffer> ii import Text.ParserCombinators.ReadP<CR>

" usage: end the mm abrev with a space
iabbr <buffer> mm main = interact $ show . partOne . (map parseLine) . lines<CR><CR>
            \partOne = id<CR><CR>
            \parseLine = fst . last . readP_to_S parser<CR><CR>
            \parser =
