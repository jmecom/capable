const PREC = {
  call: 9,
  unary: 8,
  multiplicative: 7,
  additive: 6,
  compare: 5,
  equality: 4,
  and: 3,
  or: 2,
};

module.exports = grammar({
  name: "capable",

  extras: ($) => [/\s/, $.comment],

  word: ($) => $.identifier,

  conflicts: ($) => [[ $.return_stmt ], [ $.type_path, $.path_expr ]],

  rules: {
    source_file: ($) =>
      seq(
        optional($.package_decl),
        $.module_decl,
        repeat($.use_decl),
        repeat($._item)
      ),

    package_decl: ($) => seq("package", choice("safe", "unsafe")),

    module_decl: ($) => seq("module", $.module_path),

    use_decl: ($) => seq("use", $.module_path),

    module_path: ($) => seq($.identifier, repeat(seq("::", $.identifier))),

    _item: ($) =>
      choice(
        $.function_decl,
        $.extern_function_decl,
        $.struct_decl,
        $.enum_decl,
        $.impl_block
      ),

    function_decl: ($) =>
      seq(
        optional("pub"),
        "fn",
        field("name", $.identifier),
        $.param_list,
        "->",
        field("return_type", $.type),
        field("body", $.block)
      ),

    extern_function_decl: ($) =>
      seq(
        optional("pub"),
        "extern",
        "fn",
        field("name", $.identifier),
        $.param_list,
        "->",
        field("return_type", $.type),
        optional(";")
      ),

    struct_decl: ($) =>
      seq(
        repeat(choice("pub", "linear", "copy", "opaque")),
        "struct",
        field("name", $.identifier),
        "{",
        optional($.field_list),
        "}"
      ),

    enum_decl: ($) =>
      seq(
        optional("pub"),
        "enum",
        field("name", $.identifier),
        "{",
        optional($.enum_variants),
        "}"
      ),

    impl_block: ($) =>
      seq(
        "impl",
        field("target", $.type),
        "{",
        repeat($.method_decl),
        "}"
      ),

    method_decl: ($) =>
      seq(
        optional("pub"),
        "fn",
        field("name", $.identifier),
        $.param_list,
        "->",
        field("return_type", $.type),
        field("body", $.block)
      ),

    field_list: ($) =>
      seq($.field, repeat(seq(",", $.field)), optional(",")),

    field: ($) => seq($.identifier, ":", $.type),

    enum_variants: ($) =>
      seq($.enum_variant, repeat(seq(",", $.enum_variant)), optional(",")),

    enum_variant: ($) =>
      seq($.identifier, optional(seq("(", $.type, ")"))),

    param_list: ($) =>
      seq("(", optional(seq($.param, repeat(seq(",", $.param)), optional(","))), ")"),

    param: ($) =>
      choice(
        seq(field("name", $.identifier), ":", field("type", $.type)),
        field("name", $.self_param)
      ),

    self_param: ($) => "self",

    type: ($) =>
      choice(
        seq("*", $.type),
        seq($.type_path, optional($.type_args))
      ),

    type_path: ($) => seq($.identifier, repeat(seq("::", $.identifier))),

    type_args: ($) => seq("[", $.type, repeat(seq(",", $.type)), optional(","), "]"),

    block: ($) => seq("{", repeat($.statement), "}"),

    statement: ($) =>
      choice(
        $.let_stmt,
        $.assign_stmt,
        $.return_stmt,
        $.if_stmt,
        $.while_stmt,
        $.expr_stmt
      ),

    let_stmt: ($) =>
      seq(
        "let",
        $.identifier,
        optional(seq(":", $.type)),
        "=",
        $.expression,
        optional(";")
      ),

    assign_stmt: ($) => seq($.identifier, "=", $.expression, optional(";")),

    return_stmt: ($) => seq("return", optional($.expression), optional(";")),

    if_stmt: ($) =>
      seq(
        "if",
        $.expression,
        $.block,
        optional(seq("else", $.block))
      ),

    while_stmt: ($) => seq("while", $.expression, $.block),

    expr_stmt: ($) => seq($.expression, optional(";")),

    expression: ($) =>
      choice(
        $.match_expr,
        $.binary_expr,
        $.unary_expr,
        $.call_expr,
        $.struct_literal,
        $.path_expr,
        $.literal,
        $.grouping
      ),

    match_expr: ($) =>
      seq(
        "match",
        $.expression,
        "{",
        repeat(seq($.match_arm, optional(","))),
        "}"
      ),

    match_arm: ($) => seq($.pattern, "=>", $.block),

    pattern: ($) =>
      choice(
        $.literal,
        $.pattern_call,
        $.path_expr,
        "_"
      ),

    pattern_call: ($) =>
      seq($.path_expr, "(", optional($.identifier), ")"),

    call_expr: ($) =>
      prec(
        PREC.call,
        seq(field("function", $.expression), $.arg_list)
      ),

    arg_list: ($) =>
      seq("(", optional(seq($.expression, repeat(seq(",", $.expression)), optional(","))), ")"),

    struct_literal: ($) =>
      seq(
        $.type_path,
        "{",
        optional(seq($.struct_field, repeat(seq(",", $.struct_field)), optional(","))),
        "}"
      ),

    struct_field: ($) => seq($.identifier, ":", $.expression),

    path_expr: ($) =>
      seq($.identifier, repeat(choice(seq("::", $.identifier), seq(".", $.identifier)))),

    unary_expr: ($) =>
      prec(PREC.unary, seq(choice("!", "-"), $.expression)),

    binary_expr: ($) =>
      choice(
        prec.left(PREC.or, seq($.expression, "||", $.expression)),
        prec.left(PREC.and, seq($.expression, "&&", $.expression)),
        prec.left(PREC.equality, seq($.expression, choice("==", "!="), $.expression)),
        prec.left(PREC.compare, seq($.expression, choice("<", "<=", ">", ">="), $.expression)),
        prec.left(PREC.additive, seq($.expression, choice("+", "-"), $.expression)),
        prec.left(PREC.multiplicative, seq($.expression, choice("*", "/"), $.expression))
      ),

    grouping: ($) => seq("(", $.expression, ")"),

    literal: ($) =>
      choice(
        $.int_lit,
        $.u8_lit,
        $.string_lit,
        "true",
        "false",
        "unit"
      ),

    int_lit: ($) => /[0-9]+/,
    u8_lit: ($) => /[0-9]+u8/,
    string_lit: ($) => /"([^"\\]|\\.)*"/,

    identifier: ($) => /[a-zA-Z_][a-zA-Z0-9_]*/,

    comment: ($) => /\/\/[^\n]*/,
  },
});
