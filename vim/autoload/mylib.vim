function mylib#FindFileOrTestFile()
    let filenameWithNoExt = substitute(expand('%:r'), '.*/', '', '')
    let testFilePattern = '[_-]\?[tT]est$'

    if (filenameWithNoExt =~ testFilePattern)
        let fuzzyQuery = "'" .. substitute(filenameWithNoExt, testFilePattern, '', '')
    else
        let fuzzyQuery = "'" .. filenameWithNoExt .. " 'test"
    endif

    call fzf#vim#files('.', { 'source': 'rg --files', 'options': '--query "' .. fuzzyQuery .. '"' })
endfunction
