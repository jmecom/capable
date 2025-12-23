#![allow(unused_assignments)]

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::ast::Span;

#[derive(Debug, Error, Diagnostic)]
#[error("{message}")]
#[allow(unused)]
pub struct ParseError {
    message: String,
    #[label]
    span: SourceSpan,
}

impl ParseError {
    pub fn new(message: String, span: Span) -> Self {
        Self {
            message,
            span: (span.start, span.end - span.start).into(),
        }
    }
}

#[derive(Debug, Error, Diagnostic)]
#[error("{message}")]
#[allow(unused)]
pub struct TypeError {
    message: String,
    #[label]
    span: SourceSpan,
}

impl TypeError {
    pub fn new(message: String, span: Span) -> Self {
        Self {
            message,
            span: (span.start, span.end - span.start).into(),
        }
    }
}
