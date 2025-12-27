# Sample Programs (Golden Outputs)

These programs are stable references for language behavior and output.

## 1) Config Loader

- `examples/config_loader/config_loader.cap`
- Expected output:
  ```
  key: host
  value: localhost
  key: port
  value: 8080
  key: log_level
  value: info
  config ok
  ```

## 2) Filesystem Attenuation

- `tests/programs/fs_attenuation.cap`
- Expected output contains:
  - `Hello from config/app.txt`

## 3) Word Count (File)

- `tests/programs/wc_file.cap`
- Expected output contains:
  - `lines:`
  - `words:`
  - `bytes:`

## 4) Word Count (Stdin)

- `tests/programs/wc_stdin.cap`
- Expected output contains:
  - `lines:`
  - `words:`
  - `bytes:`
  - `stdin ok`
