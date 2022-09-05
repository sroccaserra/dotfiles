vim9script

def mylib#FindFileOrTestFile(): void
    const filenameWithNoExt = substitute(expand('%:r'), '.*/', '', '')
    const testFilePattern = '[_-]\?[tT]est$'

    var fuzzyQuery: string
    if (filenameWithNoExt =~ testFilePattern)
        fuzzyQuery = "'" .. substitute(filenameWithNoExt, testFilePattern, '', '')
    else
        fuzzyQuery = "'" .. filenameWithNoExt .. " 'test"
    endif

    call fzf#vim#files('.', { source: 'rg --files', options: '--query "' .. fuzzyQuery .. '"' })
enddef
