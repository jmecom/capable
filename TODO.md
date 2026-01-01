Done:
- Result<T, E> + is_ok/is_err/ok/err/unwrap_* implemented in stdlib (panic uses never type).
- Stdlib APIs updated to use Vec<T> (compiler maps Vec<u8>/Vec<i32>/Vec<string> to VecU8/VecI32/VecString).
- Codex reviewed Claude-generated commits.
