#![allow(unused_assignments)]

use miette::{Diagnostic, NamedSource, SourceSpan};
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
    #[source_code]
    source_code: Option<NamedSource<String>>,
    #[label]
    span: SourceSpan,
    span_raw: Span,
}

impl ParseError {
    pub fn new(message: String, span: Span) -> Self {
        Self {
            message,
            source_code: None,
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

    pub fn has_source(&self) -> bool {
        self.source_code.is_some()
    }

    pub fn with_source(
        mut self,
        name: impl Into<String>,
        source: impl Into<String>,
    ) -> Self {
        self.source_code = Some(NamedSource::new(name.into(), source.into()));
        self
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
    module_name: Option<String>,
    #[source_code]
    source_code: Option<NamedSource<String>>,
    #[label]
    span: SourceSpan,
    span_raw: Span,
}

impl TypeError {
    pub fn new(message: String, span: Span) -> Self {
        Self {
            message,
            module_name: None,
            source_code: None,
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

    pub fn has_source(&self) -> bool {
        self.source_code.is_some()
    }

    pub fn in_module(mut self, module_name: impl Into<String>) -> Self {
        self.module_name = Some(module_name.into());
        self
    }

    pub fn module_name(&self) -> Option<&str> {
        self.module_name.as_deref()
    }

    pub fn with_source(
        mut self,
        name: impl Into<String>,
        source: impl Into<String>,
    ) -> Self {
        self.source_code = Some(NamedSource::new(name.into(), source.into()));
        self
    }

    pub fn message(&self) -> &str {
        &self.message
    }
}
