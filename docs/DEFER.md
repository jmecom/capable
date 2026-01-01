# Defer

`defer` schedules a function or method call to run when the current scope
exits. Defers run in last-in, first-out order.

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

- Defers run when the current scope ends, including on `return`, `break`,
  `continue`, or normal fallthrough.
- Arguments are evaluated at the `defer` site and captured for later use.
- Defers are executed in LIFO order.

## Restrictions (current)

- The deferred expression must be a function or method call.
- Defers are not run if the function traps (e.g., `panic()`).
