# Defer

`defer` schedules a function or method call to run when the current function
returns. Defers run in last-in, first-out order.

```cap
fn example(c: Console) -> unit {
  c.println("start")
  defer c.println("first")
  defer c.println("second")
  c.println("end")
  return ()
}
```

Output order:

```
start
end
second
first
```

## Semantics

- Defers run on every return path, including implicit `unit` returns.
- Arguments are evaluated at the `defer` site and captured for later use.
- Defers are executed in LIFO order.

## Restrictions (current)

- `defer` is only allowed at the top level of a function body (not inside `if`,
  `while`, `for`, or `match` blocks).
- The deferred expression must be a function or method call.
- Defers are not run if the function traps (e.g., `panic()`).
