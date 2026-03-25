mod exprs;
mod items;
mod patterns;
mod stmts;
mod types;

use crate::ast::*;
use crate::error::ParseError;
use crate::lexer::{lex, to_ident, Token, TokenKind};

pub fn parse_module(source: &str) -> Result<Module, ParseError> {
    let tokens = lex(source);
    let mut parser = Parser::new(tokens, source.len());
    parser.parse_module()
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
    eof_span: Span,
    next_expr_id: u32,
}

impl Parser {
    fn peek_kind_raw(&self) -> Option<TokenKind> {
        self.tokens.get(self.index).map(|t| t.kind.clone())
    }

    fn peek_span_raw(&self) -> Span {
        self.tokens
            .get(self.index)
            .map(|t| t.span)
            .unwrap_or(self.eof_span)
    }

    fn take_doc_comments(&mut self) -> Option<String> {
        let mut docs = Vec::new();
        loop {
            match self.peek_kind_raw() {
                Some(TokenKind::DocLine) => {
                    let token = self.bump().unwrap();
                    let text = token.text.trim_start_matches("///");
                    docs.push(text.trim_start().to_string());
                }
                _ => break,
            }
        }
        if docs.is_empty() {
            None
        } else {
            Some(docs.join("\n").trim().to_string())
        }
    }

    fn new(tokens: Vec<Token>, source_len: usize) -> Self {
        let eof_span = Span::new(source_len, source_len);
        Self {
            tokens,
            index: 0,
            eof_span,
            next_expr_id: 0,
        }
    }

    fn fresh_expr_id(&mut self) -> ExprId {
        let id = ExprId(self.next_expr_id);
        self.next_expr_id += 1;
        id
    }

    fn parse_module(&mut self) -> Result<Module, ParseError> {
        let mut package = PackageSafety::Safe;
        let start_span = self.peek_span_raw();
        if self.peek_kind() == Some(TokenKind::Package) {
            let pkg_span = self.bump().unwrap().span;
            let safety = match self.bump_kind()? {
                TokenKind::Safe => PackageSafety::Safe,
                TokenKind::Unsafe => PackageSafety::Unsafe,
                other => {
                    return Err(self.error_at(
                        pkg_span,
                        format!("expected `safe` or `unsafe`, found {other:?}"),
                    ));
                }
            };
            package = safety;
        }

        self.expect(TokenKind::Module)?;
        let name = self.parse_path()?;

        let mut uses = Vec::new();
        while self.peek_kind() == Some(TokenKind::Use) {
            uses.push(self.parse_use()?);
        }

        let mut items = Vec::new();
        while self.peek_kind_raw().is_some() {
            let doc = self.take_doc_comments();
            if self.peek_kind_raw().is_none() {
                break;
            }
            items.push(self.parse_item(doc)?);
        }

        let end_span = if let Some(token) = self.tokens.last() {
            token.span
        } else {
            self.eof_span
        };

        Ok(Module {
            package,
            name,
            uses,
            items,
            span: Span::new(start_span.start, end_span.end),
        })
    }

    fn expect(&mut self, kind: TokenKind) -> Result<Token, ParseError> {
        match self.bump() {
            Some(token) if token.kind == kind => Ok(token),
            Some(token) => Err(self.error_at(token.span, format!("expected {kind:?}, found {:?}", token.kind))),
            None => Err(self.error_current(format!("expected {kind:?}, found end of input"))),
        }
    }

    fn expect_ident(&mut self) -> Result<Ident, ParseError> {
        match self.peek_kind() {
            Some(TokenKind::Ident) => Ok(to_ident(&self.bump().unwrap())),
            Some(other) => Err(self.error_current(format!("expected identifier, found {other:?}"))),
            None => Err(self.error_current("unexpected end of input".to_string())),
        }
    }

    fn bump(&mut self) -> Option<Token> {
        let token = self.tokens.get(self.index).cloned();
        if token.is_some() {
            self.index += 1;
        }
        token
    }

    fn bump_kind(&mut self) -> Result<TokenKind, ParseError> {
        self.bump()
            .map(|token| token.kind)
            .ok_or_else(|| self.error_current("unexpected end of input".to_string()))
    }

    fn maybe_consume(&mut self, kind: TokenKind) -> Option<Token> {
        if self.peek_kind() == Some(kind) {
            self.bump()
        } else {
            None
        }
    }

    fn peek_kind(&self) -> Option<TokenKind> {
        self.peek_kind_raw()
    }

    fn peek_span(&self) -> Span {
        self.peek_span_raw()
    }

    fn error_current(&self, message: String) -> ParseError {
        ParseError::new(message, self.peek_span())
    }

    fn error_at(&self, span: Span, message: String) -> ParseError {
        ParseError::new(message, span)
    }

    fn peek_token(&self, offset: usize) -> Option<&Token> {
        self.tokens.get(self.index + offset)
    }
}

fn infix_binding_power(op: &BinaryOp) -> (u8, u8) {
    match op {
        BinaryOp::Or => (1, 2),
        BinaryOp::And => (3, 4),
        BinaryOp::BitOr => (5, 6),
        BinaryOp::BitXor => (7, 8),
        BinaryOp::BitAnd => (9, 10),
        BinaryOp::Eq | BinaryOp::Neq => (11, 12),
        BinaryOp::Lt | BinaryOp::Lte | BinaryOp::Gt | BinaryOp::Gte => (13, 14),
        BinaryOp::Shl | BinaryOp::Shr => (15, 16),
        BinaryOp::Add | BinaryOp::Sub => (17, 18),
        BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => (19, 20),
    }
}

fn postfix_binding_power(kind: &TokenKind) -> Option<u8> {
    match kind {
        TokenKind::Dot | TokenKind::LParen | TokenKind::LBracket | TokenKind::Question => Some(23),
        _ => None,
    }
}

fn unescape_string(text: &str) -> Result<String, String> {
    let mut out = String::new();
    let mut chars = text[1..text.len() - 1].chars();
    while let Some(ch) = chars.next() {
        if ch != '\\' {
            out.push(ch);
            continue;
        }
        let Some(esc) = chars.next() else {
            return Err("unterminated escape".to_string());
        };
        match esc {
            '\\' => out.push('\\'),
            '"' => out.push('"'),
            'n' => out.push('\n'),
            'r' => out.push('\r'),
            't' => out.push('\t'),
            '0' => out.push('\0'),
            other => return Err(format!("unsupported escape \\{other}")),
        }
    }
    Ok(out)
}

fn unescape_char(text: &str) -> Result<u8, String> {
    let mut chars = text.chars();
    if chars.next() != Some('\'') || text.len() < 2 {
        return Err("missing quotes".to_string());
    }
    let Some(ch) = chars.next() else {
        return Err("empty char literal".to_string());
    };
    let value = if ch == '\\' {
        let Some(esc) = chars.next() else {
            return Err("invalid escape".to_string());
        };
        match esc {
            'n' => b'\n',
            'r' => b'\r',
            't' => b'\t',
            '\\' => b'\\',
            '\'' => b'\'',
            'x' => {
                let hi = chars
                    .next()
                    .ok_or_else(|| "invalid hex escape".to_string())?;
                let lo = chars
                    .next()
                    .ok_or_else(|| "invalid hex escape".to_string())?;
                let hex = format!("{hi}{lo}");
                u8::from_str_radix(&hex, 16).map_err(|_| "invalid hex escape".to_string())?
            }
            other => return Err(format!("unsupported escape \\{other}")),
        }
    } else {
        let code = ch as u32;
        if code > 255 {
            return Err("char literal out of range".to_string());
        }
        code as u8
    };
    Ok(value)
}

fn unit_type_at(span: Span) -> Type {
    let ident = Spanned::new("unit".to_string(), span);
    let path = Path {
        id: ExprId(u32::MAX),
        segments: vec![ident],
        span,
    };
    Type::Path { path, args: Vec::new(), span }
}
