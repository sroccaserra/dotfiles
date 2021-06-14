" Vim syntax file
" Language: Uxntal ~ https://wiki.xxiivv.com/site/uxntal.html

if exists('b:current_syntax')
  finish
endif

syntax keyword uxntalKeyword BRK LIT NOP POP DUP SWP OVR ROT EQU NEQ GTH LTH JMP JCN JSR STH LDZ STZ LDR STR LDA STA DEI DEO ADD SUB MUL DIV AND ORA EOR SFT
syntax keyword uxntalKeyword BRK2 LIT2 NOP2 POP2 DUP2 SWP2 OVR2 ROT2 EQU2 NEQ2 GTH2 LTH2 JMP2 JCN2 JSR2 STH2 LDZ2 STZ2 LDR2 STR2 LDA2 STA2 DEI2 DEO2 ADD2 SUB2 MUL2 DIV2 AND2 ORA2 EOR2 SFT2
syntax keyword uxntalKeyword BRKk LITk NOPk POPk DUPk SWPk OVRk ROTk EQUk NEQk GTHk LTHk JMPk JCNk JSRk STHk LDZk STZk LDRk STRk LDAk STAk DEIk DEOk ADDk SUBk MULk DIVk ANDk ORAk EORk SFTk

syntax match uxntalNumber '#[0-9a-f]\{2\}\>'
syntax match uxntalNumber '#[0-9a-f]\{4\}\>'
syntax match uxntalNumber '\<[0-9a-f]\+\>'

syntax match uxntalPad '[|$][0-9a-f]\+\>'

syntax match uxntalString '"\S\+'
syntax match uxntalLabel '[@.&,;]\S\+'
syntax match uxntalWord '[%]\S\+'

syntax region uxntalComment start='(' end=')'

let b:current_syntax = 'uxntal'

hi def link uxntalKeyword Function
hi def link uxntalNumber Number
hi def link uxntalPad Special
hi def link uxntalString String
hi def link uxntalLabel Label
hi def link uxntalWord Type
hi def link uxntalComment Comment
