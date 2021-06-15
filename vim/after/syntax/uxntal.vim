" Vim syntax file
" Language: Uxntal ~ https://wiki.xxiivv.com/site/uxntal.html

if exists('b:current_syntax')
  finish
endif

syntax match uxntalKeyword '\<\(BRK\|LIT\|NOP\|POP\|DUP\|SWP\|OVR\|ROT\|EQU\|NEQ\|GTH\|LTH\|JMP\|JCN\|JSR\|STH\|LDZ\|STZ\|LDR\|STR\|LDA\|STA\|DEI\|DEO\|ADD\|SUB\|MUL\|DIV\|AND\|ORA\|EOR\|SFT\)[2rk]\{0,3\}\>'

syntax match uxntalNumber '#[0-9a-f]\{2\}\>'
syntax match uxntalNumber '#[0-9a-f]\{4\}\>'
syntax match uxntalNumber '\<[0-9a-f]\+\>'

syntax match uxntalPad '[|$][0-9a-f]\+\>'

syntax match uxntalString '"\S\+'
syntax match uxntalLabel '[@.&,;]\S\+'
syntax match uxntalWord '[%]\S\+'

syntax region uxntalComment start='($' start='(\s' end='^)' end='\s)'

let b:current_syntax = 'uxntal'

hi def link uxntalKeyword Function
hi def link uxntalNumber Number
hi def link uxntalPad Special
hi def link uxntalString String
hi def link uxntalLabel Label
hi def link uxntalWord Type
hi def link uxntalComment Comment
