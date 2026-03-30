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
  "linear"
  "copy"
  "capability"
  "match"
  "true"
  "false"
] @keyword

"unit" @constant.builtin

[
  (int_lit)
  (u8_lit)
  (i64_lit)
  (u64_lit)
] @number

((type_path (identifier) @type.builtin)
 (#match? @type.builtin "^(i32|i64|u32|u64|u8|bool|string|Result)$"))

(string_lit) @string

(comment) @comment

(identifier) @variable

(function_decl name: (identifier) @function)
(extern_function_decl name: (identifier) @function)
(method_decl name: (identifier) @function)

(struct_decl name: (identifier) @type)
(enum_decl name: (identifier) @type)

(field (identifier) @property)
(struct_field (identifier) @property)
