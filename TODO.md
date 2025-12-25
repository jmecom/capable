Here’s your same document, updated to explicitly cover **what we need for methods** (parser + typechecker + codegen), without changing the overall structure.

---

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
* `value_from_params(TyKind::Unit)` returns `ValueRepr::Unit` (not `iconst 0`)
* `value_from_results(TyKind::Unit)` returns `ValueRepr::Unit`
* `store_local(ValueRepr::Unit)` stores `LocalValue::Value(ValueRepr::Unit)` (no slot)
* `to_b1(Unit)` should be an error (don’t invent unit-as-bool)
* Any match arms that pattern-match `ValueRepr` must add `Unit`.

This removes a ton of “why is there an i32 here?” ABI mismatch pain.

---

### P0.2 Fix control-flow terminators (don’t emit after return/jump)

Your emitter often does:

* emit body
* maybe jump/return
* then keep emitting in same block

**Rule:** after emitting a terminator, **stop emitting further statements into that block**.

**Concrete approach:**

* Make `emit_stmt` return `Flow::Continues | Flow::Terminated`, and stop emitting when terminated.
* Or lighter: after any `return_` / unconditional `jump`, immediately `return Ok(())` from the surrounding emission helper.

Places this matters: `Stmt::Return`, if branches, while body, match arms.

---

### P0.3 Fix scoping in typechecker (block-scoped `let`, outer `assign` mutates)

Cloning a single `HashMap` is wrong because it rolls back assignments too.

**Concrete fix: scope stack.**

```rust
struct Scopes(Vec<HashMap<String, Ty>>);
```

* `push_scope()` entering `{ ... }`
* `pop_scope()` on exit
* `insert_local(name, ty)` into top scope
* `lookup(name)` searches top -> bottom
* `assign(name, ty)` updates nearest scope that contains it (or errors)

Apply to:

* `check_block`: push/pop
* `if`/`while` bodies: just call `check_block`
* `match` arms: push a scope for bindings per arm

---

### P0.4 Return checking: pick a simple rule now

Your “has_return” approach is incorrect.

**Concrete fix (recommended for now):**
For non-unit functions: require outer function block ends with `return expr;`.

* In `check_fn`: if ret != unit, error unless last stmt is `Return(Some(_))`.
* Early returns still allowed; you’re just not proving coverage yet.

---

### P0.5 Methods prerequisite: add a proper postfix parse loop (so `.` isn’t special-cased)

This is the thing that makes methods “not a rewrite”.

**Goal:** parse `.` as a postfix op on *expressions*, not as “part of a path” everywhere.

**Concrete parser change:**
After you parse the initial prefix/primary expression, do a loop:

* if next token is `(` → call (what you already do)
* if next token is `.` → member/method access
* (later: `?`, indexing `[]`, etc.)

This avoids the current trap where `.` is only “module path glue” and becomes impossible to extend cleanly.

---

## P1 — correctness + quality-of-life (makes debugging/adding features sane)

### P1.1 Fix `if` / `while` scoping in codegen (lexical, non-leaking)

Same approach you already used: don’t mutate `locals` while emitting branches.

* In `Stmt::If`: snapshot once, clone per branch, restore after merge.
* In `Stmt::While`: emit body with a cloned locals view so `let` inside doesn’t leak outward; assignments still work because they target existing slots.

This should match your typechecker semantics once scope-stack lands.

---

### P1.2 Fix match expression value merging (and include Unit)

Your “result shape + stack slots + load at merge” approach is fine for v0, but enforce:

* all arms return same shape (unit/single/pair) and types
* Unit means **0 slots**

Concrete changes:

* support “0 values”: no slots, just jump to merge
* at merge: if kind is Unit → return `ValueRepr::Unit`

---

### P1.3 Fix ResultString / ResultOut ABI sizes (callsite correctness)

* `slot_ptr` size must be `ptr_ty.bytes()`
* `slot_len` is 8 (I64)
* `slot_err` is 4 (I32)
* load pointer as `ptr_ty` (you already do)

Also use `ptr_ty.bytes()` for any pointer-sized explicit slots elsewhere.

---

### P1.4 Fix string literal error formatting / escaping bugs

* `format!("unsupported escape \\{{other}}")` prints `{other}` literally → use `{other}`
* same for `{message}` formatting

---

### P1.5 Methods prerequisite: typecheck must resolve method calls (don’t make codegen guess)

Methods require the receiver’s type to resolve the callee. If codegen tries to “figure it out” from syntax, you’ll keep hitting walls.

**Concrete rule:** by the time codegen runs, every call site must already know the final symbol/ABI.

Minimal path:

* Add AST node: `Expr::MethodCall { recv, name, args }` (or `Postfix::Dot + optional call`)
* In the typechecker, resolve it and **desugar** to a normal call:

  * `recv.foo(a,b)` ⇒ `foo(recv, a, b)` but with a *resolved* symbol (see below)

Output from typechecker must include resolution info, not just types.

---

## P2 — structural (keeps you from fighting yourself as features grow)

### P2.1 Make span retrieval a single shared API (remove duplicate SpanExt)

Put `span()` on AST nodes (inherent methods or one shared trait in `ast`), delete duplicates.

---

### P2.2 Fix match arm + struct literal field spans (diagnostics accuracy)

* match arm span start from pattern span start (not `match`)
* struct literal field span start from field name span start (not `{`)

---

### P2.3 Parser: prepare for postfix operators (`.` methods, `?`, indexing)

You already special-case call in the Pratt loop. Methods will be smoother if you commit to:

* parse primary/prefix
* then apply a loop of postfix operators, highest precedence

Don’t implement `?`/`[]` now; just structure it so you won’t rewrite again.

---

### P2.4 Typechecker: unify name resolution for types and values

Right now types vs values resolve via different ad-hoc rules. Methods add a third kind: “member resolution on a type”.

**Concrete fix:** a single `Resolver` that can:

* resolve type paths
* resolve value/function paths
* resolve enum variants

Even if it returns strings, keep the rules consistent.

---

### P2.5 Methods design: pick a v0 lowering rule (so it’s implementable)

You need *one* clear rule before coding.

**Recommended v0 (lowest churn):**

* Methods are just sugar for functions where the receiver is the first argument.
* The typechecker rewrites `recv.method(args...)` into a plain `Call` to a fully-qualified function symbol (and validates the receiver type).

You’ll need one of these lookup mechanisms:

* **By receiver type name**: `TypeName::method` maps to a function symbol (string-based is fine)
* **By module namespace**: `sys.console.println(console, "hi")` but allow sugar `console.println("hi")` (typechecker rewrites to the namespace function)

Either way: **resolution happens in typecheck**, not in codegen.

---

## After P0/P1: the highest leverage bridge for methods

### Emit typed + resolved output from typechecker for codegen

You already compute types; you just throw them away. For methods, you must also preserve call resolution.

Minimal:

```rust
struct TypeInfo {
  expr_ty: HashMap<ExprId, Ty>,
  resolved_calls: HashMap<ExprId, ResolvedCallee>,
  // where ResolvedCallee contains fully-qualified symbol + lowered signature/ABI info
}
```

Then codegen is dumb:

* `Expr::Call` / desugared method call already has a `ResolvedCallee`
* no more “try to figure out what this means” inside codegen

This is the change that turns “methods are cross-cutting pain” into “methods are a parser + typechecker feature”.

---

