# Static traits plan

This plan is for a minimal, compile-time only trait system (no trait objects,
no dynamic dispatch, no vtables). The goal is to enable generic constraints and
method resolution for traits, while keeping the implementation small.

## Goals

- Define traits and implement them for types.
- Allow trait bounds on generic parameters (functions, structs, enums, impls).
- Use traits for method lookup in generic contexts.
- No dynamic dispatch, no trait objects, no runtime metadata.

## Non-goals

- `dyn Trait` or any form of runtime trait objects.
- Default methods that require dynamic dispatch.
- Negative bounds or specialization.
- Coherence across crates or complex orphan rules (single-project scope).

## Proposed syntax

- Trait declaration:
  trait Display {
    fn fmt(self, c: Console) -> unit
  }

- Implementing a trait:
  impl Display for i32 {
    fn fmt(self, c: Console) -> unit { ... }
  }

- Bounds on generics:
  fn show<T: Display>(x: T, c: Console) -> unit { x.fmt(c) }

## Milestones

1) Parser/AST/HIR support
- Add `trait` item with method signatures (no bodies).
- Add `impl Trait for Type` blocks with method bodies.
- Add generic bounds syntax `T: Trait` in type parameter lists.
- Decide where trait paths live (`use` resolution like types).

2) Name resolution + type collection
- Collect trait declarations into a trait map.
- Collect impl blocks keyed by (trait, type).
- Enforce basic coherence: at most one impl per (trait, concrete type) in the
  current compilation unit.

3) Type checking rules
- When type checking a generic with bounds, verify constraints are satisfied at
  call sites (monomorphized instantiations).
- Method lookup order: inherent methods first, then trait methods if bounds
  prove the trait.
- Ensure trait impl methods match the declared signature exactly.

4) Monomorphization + codegen
- Treat trait methods as regular functions with a mangled name including
  (trait, type) to avoid collisions.
- For a constrained call `x.fmt(...)`, resolve to the concrete impl at
  monomorphization time (no runtime indirection).

5) Standard library hooks (minimal)
- Add a tiny trait or two in stdlib (e.g., `eq`, `hash`) for dogfooding.
- Update the hash-map demo to use `Hash` once the trait exists.

6) Tests
- Parser snapshot for trait/impl/bounds syntax.
- Typecheck tests for:
  - missing impl
  - duplicate impl
  - signature mismatch
  - method resolution via trait bound
- Run-time test that exercises a trait-bound generic call.

## Risks / open questions

- How to represent trait bounds on impl blocks and where to store them in HIR.
- Rule for overlapping impls when type params are involved (keep simple: no
  overlapping impls allowed unless identical after monomorphization).
- Decide if trait methods can be called via UFCS (`Trait::method(x)`), or only
  via method call syntax (`x.method()`).
