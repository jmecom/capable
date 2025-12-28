#![allow(unused_assignments)]

use miette::{Diagnostic, SourceSpan};
use thiserror::Error;

use crate::ast::Span;

/// Prefix an error message with context for consistent diagnostics.
pub fn format_with_context(context: impl AsRef<str>, message: impl AsRef<str>) -> String {
    let prefix = context.as_ref();
    let message = message.as_ref();
    if prefix.is_empty() {
        message.to_string()
    } else {
        format!("{prefix}: {message}")
    }
}

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
        self.message = format_with_context(context, &self.message);
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
        self.message = format_with_context(context, &self.message);
        self
    }

    pub fn span(&self) -> Span {
        self.span_raw
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}
