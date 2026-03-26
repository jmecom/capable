if exists("b:current_syntax")
  finish
endif

syntax case match

syntax keyword capKeyword package module use pub extern fn let if else while for in return struct enum impl match break continue defer try unsafe safe opaque linear copy capability true false
syntax keyword capBuiltin unit
syntax keyword capType i32 u32 u8 bool string Result

syntax match capComment "//.*$"
syntax region capString start=+"+ skip=+\\\\\|\\"+ end=+"+
syntax match capNumber "\<\d\+u8\>\|\<\d\+\>"

syntax match capFunctionDecl "\<fn\>\s\+\zs[A-Za-z_][A-Za-z0-9_]*"
syntax match capTypeDecl "\<\(struct\|enum\|impl\)\>\s\+\zs[A-Za-z_][A-Za-z0-9_:]*"
syntax match capModulePath "\<[A-Za-z_][A-Za-z0-9_]*\(::[A-Za-z_][A-Za-z0-9_]*\)\+"
syntax match capField "\.\zs[A-Za-z_][A-Za-z0-9_]*"

highlight default link capKeyword Keyword
highlight default link capBuiltin Constant
highlight default link capType Type
highlight default link capComment Comment
highlight default link capString String
highlight default link capNumber Number
highlight default link capFunctionDecl Function
highlight default link capTypeDecl Type
highlight default link capModulePath Include
highlight default link capField Identifier

let b:current_syntax = "cap"
