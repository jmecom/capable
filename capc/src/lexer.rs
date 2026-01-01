use logos::Logos;

use crate::ast::{Span, Spanned};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
    pub text: String,
}

#[derive(Logos, Debug, Clone, PartialEq, Eq)]
#[logos(skip r"[ \t\r\n]+")]
#[logos(skip r"//[^\n]*")]
pub enum TokenKind {
    #[regex(r"///[^\n]*", priority = 3)]
    DocLine,
    #[token("module")]
    Module,
    #[token("package")]
    Package,
    #[token("safe")]
    Safe,
    #[token("unsafe")]
    Unsafe,
    #[token("use")]
    Use,
    #[token("pub")]
    Pub,
    #[token("extern")]
    Extern,
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("defer")]
    Defer,
    #[token("return")]
    Return,
    #[token("struct")]
    Struct,
    #[token("enum")]
    Enum,
    #[token("impl")]
    Impl,
    #[token("opaque")]
    Opaque,
    #[token("capability")]
    Capability,
    #[token("copy")]
    Copy,
    #[token("linear")]
    Linear,
    #[token("match")]
    Match,
    #[token("true")]
    True,
    #[token("false")]
    False,

    #[token("->")]
    Arrow,
    #[token("=>")]
    FatArrow,
    #[token("==")]
    EqEq,
    #[token("!=")]
    NotEq,
    #[token("<=")]
    Lte,
    #[token(">=")]
    Gte,
    #[token("&&")]
    AndAnd,
    #[token("||")]
    OrOr,
    #[token("&")]
    Ampersand,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("=")]
    Eq,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("!")]
    Bang,
    #[token("?")]
    Question,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("::", priority = 3)]
    ColonColon,
    #[token(":")]
    Colon,
    #[token(",")]
    Comma,
    #[token("..", priority = 3)]
    DotDot,
    #[token(".")]
    Dot,
    #[token(";")]
    Semi,
    #[token("_", priority = 2)]
    Underscore,

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", priority = 1)]
    Ident,
    #[regex(r"[0-9]+")]
    Int,
    #[regex(r#"\"([^\"\\]|\\.)*\""#)]
    Str,

    Error,
}

pub fn lex(source: &str) -> Vec<Token> {
    let mut lexer = TokenKind::lexer(source);
    let mut tokens = Vec::new();
    while let Some(result) = lexer.next() {
        let kind = result.unwrap_or(TokenKind::Error);
        let span = lexer.span();
        let text = source[span.clone()].to_string();
        let token = Token {
            kind,
            span: Span::new(span.start, span.end),
            text,
        };
        tokens.push(token);
    }
    tokens
}

pub fn to_ident(token: &Token) -> Spanned<String> {
    Spanned::new(token.text.clone(), token.span)
}
