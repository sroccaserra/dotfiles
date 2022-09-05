function mylib#FindFileOrTestFile()
    let fileName = substitute(expand('%:r'), '.*/', '', '')
    let testSuffixRegex = '[_-]\?[tT]est$'

    if (fileName =~ testSuffixRegex)
        let fuzzyQuery = '''' . substitute(fileName, testSuffixRegex, '', '')
    else
        let fuzzyQuery = '''' . fileName . ' ''test'
    endif

    call fzf#vim#files('.', { 'source': "rg -l ''", 'options': '--query "' . fuzzyQuery . '"'})
endfunction
