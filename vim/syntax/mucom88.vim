" Vim syntax file
" Language: MUCOM88 MML variant - https://github.com/onitama/mucom88/wiki/MMLReference

if exists("b:current_syntax")
  finish
endif

syn keyword voiceNames A B C D E F G H I J K

syn match comment "#.*$"
syn match comment ";.*$"

let b:current_syntax = "mucom88"

hi def link voiceNames Constant
hi def link comment Comment
