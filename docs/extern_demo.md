# Extern Demo

This demo shows how to link an external static library into a Capable build using
`--link-search` and `--link-lib`.

## Build the demo library

From repo root:

```bash
make -C examples/extern_demo
```

This produces `examples/extern_demo/libextern_demo.a`.

## Build and run the Capable program

```bash
cargo run -p capc -- run \
  --link-search examples/extern_demo \
  --link-lib extern_demo \
  examples/extern_demo/extern_demo.cap
```

You should see:

```
hello from extern demo
capable still works
```

## Notes

- The extern function is declared as `extern fn demo_log(msg: string) -> unit`.
- `demo_log` expects a raw pointer + length; the compiler lowers `string` as a struct with a `bytes` slice `{ ptr, len }`.
- Only `package unsafe` modules may declare `extern`.
