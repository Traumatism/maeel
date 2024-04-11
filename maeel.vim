" Vim syn file
" Language: maeel

if exists("b:current_syn")
  finish
endif


let s:keepcpo = &cpo
set cpo&vim
syn case match

syn keyword maeelTodo     TODO FIXME NOTE
syn keyword maeelKeywords List Mapper end drop swap dup rot over
syn region  maeelComment start="#" end="$"
syn region  maeelString start=/\v"/ skip=/\v\\./ end=/\v"/ contains=maeelEscapes
syn match   maeelEscapes display contained "\\[nt\"]"
syn keyword maeelTypeNames list
syn match   maeelOperator "[+/%=~>*<&!?]"
syn match   maeelNumber "-\=\<\d\+\>"
syn match   maeelFloat "-\=\<\d\+\.\d\+\>"
syn keyword maeelBoolean true false
syn match   maeelStatement "fun\s*\(inline\)\?\s*" nextgroup=maeelFunction skipwhite
syn keyword maeelStatement for while ifelse match matchp
syn match   maeelFunction "\h\w*" display contained

hi def link maeelFunction Function
hi def link maeelTodos Todo
hi def link maeelKeywords Keyword
hi def link maeelComment Comment
hi def link maeelString String
hi def link maeelNumber Number
hi def link maeelFloat Float
hi def link maeelTypeNames Type
hi def link maeelChar Character
hi def link maeelEscapes SpecialChar
hi def link maeelOperator Operator
hi def link maeelBoolean Boolean
hi def link maeelStatement Statement

let b:current_syn = "maeel"

