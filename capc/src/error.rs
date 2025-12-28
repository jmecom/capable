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
    span_raw: Span,
}

impl ParseError {
    pub fn new(message: String, span: Span) -> Self {
        Self {
            message,
            span: (span.start, span.end - span.start).into(),
            span_raw: span,
        }
    }

    pub fn with_context(mut self, context: impl AsRef<str>) -> Self {
        let prefix = context.as_ref();
        if !prefix.is_empty() {
            self.message = format!("{prefix}: {}", self.message);
        }
        self
    }

    pub fn span(&self) -> Span {
        self.span_raw
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}

#[derive(Debug, Error, Diagnostic)]
#[error("{message}")]
#[allow(unused)]
pub struct TypeError {
    message: String,
    #[label]
    span: SourceSpan,
    span_raw: Span,
}

impl TypeError {
    pub fn new(message: String, span: Span) -> Self {
        Self {
            message,
            span: (span.start, span.end - span.start).into(),
            span_raw: span,
        }
    }

    pub fn with_context(mut self, context: impl AsRef<str>) -> Self {
        let prefix = context.as_ref();
        if !prefix.is_empty() {
            self.message = format!("{prefix}: {}", self.message);
        }
        self
    }

    pub fn span(&self) -> Span {
        self.span_raw
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}
