[
  "module"
  "package"
  "safe"
  "unsafe"
  "use"
  "pub"
  "extern"
  "fn"
  "let"
  "if"
  "else"
  "while"
  "return"
  "struct"
  "enum"
  "impl"
  "opaque"
  "match"
  "true"
  "false"
] @keyword

"unit" @constant.builtin

[
  (int_lit)
  (u8_lit)
] @number

(string_lit) @string

(comment) @comment

(identifier) @variable

(function_decl name: (identifier) @function)
(extern_function_decl name: (identifier) @function)
(method_decl name: (identifier) @function)

(struct_decl name: (identifier) @type)
(enum_decl name: (identifier) @type)

(field name: (identifier) @property)
(struct_field name: (identifier) @property)
