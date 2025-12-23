use std::collections::HashMap;

use lsp_server::{Connection, Message, Notification, Request};
use lsp_types::{
    Diagnostic, DiagnosticSeverity, InitializeResult, Position, PublishDiagnosticsParams, Range,
    ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
};

use capc::{load_stdlib, load_user_modules_transitive, parse_module, type_check_program};
use capc::ast::Span;
use capc::error::{ParseError, TypeError};

fn main() {
    let (connection, io_threads) = Connection::stdio();

    let capabilities = ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
        ..ServerCapabilities::default()
    };
    let initialize_result = InitializeResult {
        capabilities,
        ..InitializeResult::default()
    };
    if let Err(err) = connection.initialize(serde_json::to_value(initialize_result).unwrap()) {
        eprintln!("initialize failed: {err}");
        return;
    }

    let mut state = ServerState::default();

    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req).unwrap_or(false) {
                    break;
                }
                handle_request(req, &connection);
            }
            Message::Notification(notif) => {
                if let Err(err) = handle_notification(&mut state, notif, &connection) {
                    eprintln!("notification error: {err}");
                }
            }
            Message::Response(_) => {}
        }
    }

    io_threads.join().unwrap();
}

#[derive(Default)]
struct ServerState {
    open_files: HashMap<Url, String>,
}

fn handle_request(req: Request, connection: &Connection) {
    let response = lsp_server::Response {
        id: req.id,
        result: None,
        error: None,
    };
    let _ = connection.sender.send(Message::Response(response));
}

fn handle_notification(
    state: &mut ServerState,
    notif: Notification,
    connection: &Connection,
) -> Result<(), String> {
    match notif.method.as_str() {
        "textDocument/didOpen" => {
            let params: lsp_types::DidOpenTextDocumentParams =
                serde_json::from_value(notif.params).map_err(|err| err.to_string())?;
            let uri = params.text_document.uri;
            let text = params.text_document.text;
            state.open_files.insert(uri.clone(), text);
            publish_diagnostics(state, &uri, connection);
        }
        "textDocument/didChange" => {
            let params: lsp_types::DidChangeTextDocumentParams =
                serde_json::from_value(notif.params).map_err(|err| err.to_string())?;
            let uri = params.text_document.uri;
            if let Some(change) = params.content_changes.into_iter().last() {
                state.open_files.insert(uri.clone(), change.text);
                publish_diagnostics(state, &uri, connection);
            }
        }
        "textDocument/didSave" => {
            let params: lsp_types::DidSaveTextDocumentParams =
                serde_json::from_value(notif.params).map_err(|err| err.to_string())?;
            publish_diagnostics(state, &params.text_document.uri, connection);
        }
        _ => {}
    }
    Ok(())
}

fn publish_diagnostics(state: &mut ServerState, uri: &Url, connection: &Connection) {
    let text = match state.open_files.get(uri) {
        Some(text) => text.clone(),
        None => match uri.to_file_path() {
            Ok(path) => std::fs::read_to_string(path).unwrap_or_default(),
            Err(_) => String::new(),
        },
    };
    let diagnostics = analyze(uri, &text);
    let params = PublishDiagnosticsParams {
        uri: uri.clone(),
        diagnostics,
        version: None,
    };
    let _ = connection.sender.send(Message::Notification(Notification::new(
        "textDocument/publishDiagnostics".to_string(),
        params,
    )));
}

fn analyze(uri: &Url, text: &str) -> Vec<Diagnostic> {
    let module = match parse_module(text) {
        Ok(module) => module,
        Err(err) => return vec![diag_from_parse(text, &err)],
    };
    let stdlib = match load_stdlib() {
        Ok(stdlib) => stdlib,
        Err(err) => return vec![diag_from_parse(text, &err)],
    };
    let user_modules = match uri.to_file_path() {
        Ok(path) => match load_user_modules_transitive(&path, &module) {
            Ok(mods) => mods,
            Err(err) => return vec![diag_from_parse(text, &err)],
        },
        Err(_) => Vec::new(),
    };
    if let Err(err) = type_check_program(&module, &stdlib, &user_modules) {
        return vec![diag_from_type(text, &err)];
    }
    Vec::new()
}

fn diag_from_parse(text: &str, err: &ParseError) -> Diagnostic {
    let range = span_to_range(text, err.span());
    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        message: err.message().to_string(),
        ..Diagnostic::default()
    }
}

fn diag_from_type(text: &str, err: &TypeError) -> Diagnostic {
    let range = span_to_range(text, err.span());
    Diagnostic {
        range,
        severity: Some(DiagnosticSeverity::ERROR),
        message: err.message().to_string(),
        ..Diagnostic::default()
    }
}

fn span_to_range(text: &str, span: Span) -> Range {
    let start = offset_to_position(text, span.start);
    let end = offset_to_position(text, span.end);
    Range { start, end }
}

fn offset_to_position(text: &str, offset: usize) -> Position {
    let mut line = 0u32;
    let mut col = 0u32;
    let mut idx = 0usize;
    for ch in text.chars() {
        if idx >= offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += ch.len_utf16() as u32;
        }
        idx += ch.len_utf8();
    }
    Position { line, character: col }
}
