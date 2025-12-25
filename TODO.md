Here’s a **single merged, actionable list** (codegen + parser + typechecker), prioritized, with **concrete fixes**. I’m assuming your goal is “make the compiler predictable” more than “add features fast.”

---

## P0 — correctness / stops you from building on sand

### P0.1 Make `Unit` a real zero-sized value everywhere (one invariant)

Right now you have (or had) “unit as `iconst 0`” leaking into places it shouldn’t, plus you added `ValueRepr::Unit`.

**Rule:**

* `TyKind::Unit` flattens to **zero ABI values**
* `ValueRepr::Unit` flattens to **[]**
* Any context that needs an actual CLIF value must reject Unit (or decide semantics).

**Concrete edits (codegen):**

* `flatten_value(ValueRepr::Unit) -> vec![]`
* `value_from_params(TyKind::Unit)` should return `ValueRepr::Unit` (not `iconst 0`)
* `value_from_results(TyKind::Unit)` should return `ValueRepr::Unit`
* `store_local(ValueRepr::Unit)` should store `LocalValue::Value(ValueRepr::Unit)` (don’t allocate a stack slot)
* `to_b1(Unit)` should be an error (or define unit-as-false, but that’s weird)
* Any match arms where you currently do `ValueRepr::Single(_)` vs `Pair` vs `Result` must add `Unit` explicitly.

This change removes a *lot* of “why is there an i32 here?” ABI mismatch pain.

---

### P0.2 Fix control-flow terminators (don’t emit after return/jump)

Your emitter often does:

* emit body
* maybe jump
* then keep emitting in same block

This can create verifier errors or silently-bad IR if you insert after a terminator.

**Concrete rule:** after emitting a statement that terminates the current block, **stop emitting further statements into that block**.

**Concrete approach:**

* Make `emit_stmt` return an enum like `Flow::Continues | Flow::Terminated`, and in `emit_block` stop when terminated.
* Or lighter: in each place you emit a `return_`, immediately `return Ok(())` from the surrounding emission function.

Places this matters: `Stmt::Return`, `if` branch bodies, `while` body, match arms.

---

### P0.3 Fix scoping in typechecker (block-scoped `let`, outer `assign` mutates)

Your earlier feedback is dead-on: cloning a single `HashMap` is wrong because it rolls back assignments too.

**Concrete fix: scope stack.**
Use:

```rust
struct Scopes(Vec<HashMap<String, Ty>>);
```

* `push_scope()` on entering `{ ... }`
* `pop_scope()` on exit
* `insert_local(name, ty)` into top scope
* `lookup(name)` searches from top -> bottom
* `assign(name, ty)` updates the nearest scope that contains it (or errors)

**Where to apply:**

* `check_block`: push/pop
* `if`/`while` bodies: just call `check_block` (it handles scope)
* `match` arms: push a scope for bindings per arm (or clone stack head only)

This single change will prevent “branch lets leak” and “inner lets overwrite outer permanently.”

---

### P0.4 Return checking: pick a simple rule now

Your “has_return” approach is incorrect as described.

**Concrete fix (recommended for now):**
For non-unit functions: **require the last statement in the outer function block is `return expr;`**. No flow analysis yet.

* In `check_fn`: if ret != unit, error unless last stmt is `Return(Some(_))`.
* You can still allow early returns; you just don’t *prove* coverage.

This is a huge simplification and prevents “false success.”

---

## P1 — correctness + quality-of-life (makes debugging/adding features sane)

### P1.1 Fix `if` / `while` scoping in codegen (lexical, non-leaking)

You already implemented “don’t mutate locals across branches” as a technique. That’s good.

**Concrete rule:**

* In `Stmt::If`: snapshot `locals` once, clone per branch, restore after merge.
* In `Stmt::While`: the body should not permanently add new `let` bindings to the outer `locals` (lexical), but assignments should still work (since those refer to existing slots). The easiest: use a cloned `locals` when emitting the body.

This matches the typechecker’s intended semantics once you add scope-stack there.

---

### P1.2 Fix match expression value merging (and include Unit)

Your match-expression emitter picks a “result shape” from the first arm, allocates stack slots, stores values, loads at merge. That’s fine as a v0 technique.

But you must enforce:

* all arms return same *shape* (single/pair/unit) and types
* Unit must be handled (0 slots)

**Concrete changes:**

* If an arm value is `Unit`, set `ResultKind::Single`? No—better: add `ResultKind::Unit` or handle “0 values” as a special case.
* If `values.len() == 0`, don’t allocate slots and just jump to merge.
* At merge, if kind is Unit, return `ValueRepr::Unit`.

Also: you should seal `next_block`s properly or ensure you always switch correctly; your match builder is close, but it’s easy to forget sealing blocks that become current_block.

---

### P1.3 Fix ResultString / ResultOut ABI sizes (callsite correctness)

You already asked “where is this?” — it’s in `emit_expr` in the `Expr::Call` path, in the block:

```rust
if abi_sig.ret == TyKind::ResultString { ... }
```

**Concrete fix summary (what to keep):**

* `slot_ptr` size must be `ptr_ty.bytes()`
* `slot_len` size is 8 (you load/store I64)
* `slot_err` size is 4 (I32)
* load `ptr` as `ptr_ty` (you already do)

Also do the same kind of “use ptr_ty.bytes()” for any other stack slots you allocate for pointer-sized things.

---

### P1.4 Fix string literal error formatting / escaping bugs

You had these formatting bugs:

* `format!("unsupported escape \\{{other}}")` prints `{other}` literally
* `format!("invalid string literal: {{message}}")` prints `{message}` literally

**Concrete fix:** use `{var}` not `{{var}}`.

Also: if you continue parsing numeric suffixes in the parser, error messages should reference the correct span of the suffix token.

---

## P2 — structural (keeps you from fighting yourself as features grow)

### P2.1 Make span retrieval a single shared API (remove duplicate SpanExt)

You have two `SpanExt` traits in different modules. That’s confusing and will cause accidental imports and wrong calls.

**Concrete fix:**

* Put `fn span(&self) -> Span` as an inherent method on AST nodes (`impl Expr { fn span(&self) -> Span { ... } }`, same for Pattern, Stmt, etc.)
* Delete the duplicate traits.

This matters because span bugs are already showing up.

---

### P2.2 Fix match arm + struct literal field spans (diagnostics accuracy)

**Concrete fixes:**

* match arm span start should come from pattern span start, not the `match` keyword start.
* struct literal field span start should come from field name span start, not the `{` start.

This makes all future “why is the caret highlighting the whole match?” go away.

---

### P2.3 Parser: prepare for postfix operators (`.` methods, `?`, indexing)

You already noted it: today you special-case calls in the Pratt loop. When you add methods, you’ll want:

* parse prefix/primary
* then a loop for postfix ops: `(` call, `.` member/method, `[` index, `?` try, etc.

**Concrete plan:**

* Factor “postfix parsing” into a loop after primary:

  * while next token starts a postfix op, apply it to `lhs`.

Don’t implement methods now if you don’t want to—just structure it so you won’t rewrite the parser twice.

---

### P2.4 Typechecker: unify name resolution for types and values

Right now:

* types use `StdlibIndex` + special casing
* functions use `UseMap` and string-join

This will bite you in modules/methods.

**Concrete fix:**
Define a `Resolver` that resolves a `Path` to a fully-qualified symbol for:

* types
* functions/values
* enum variants

Even if it’s still “string-based,” keep the rule consistent.

---

## If you want the “next batch” after you finish P0/P1

Once P0 + P1 are done, the next highest-leverage thing is:

### Emit typed output from typechecker for codegen

You already compute types in the typechecker but discard them.

**Concrete approach:**
Return a `TypeInfo` from typecheck:

```rust
struct TypeInfo {
  expr_ty: HashMap<ExprId, Ty>,
  binding_ty: HashMap<BindingId, Ty>,
}
```

Then codegen stops guessing about whether something is `Unit` / `String` / `Result` by shape.

This is the bridge that makes codegen stop being “shape inference by coincidence.”

