# Generics

Capable supports explicit generics for structs, enums, and functions. Generics are
fully monomorphized during compilation: each concrete instantiation produces a
specialized definition in HIR before codegen.

## Syntax

Type parameters are declared with `[...]` after the name:

```cap
struct Box[T] {
  value: T,
}

fn id[T](value: T) -> T {
  return value
}
```

Type arguments are supplied at use sites:

```cap
let b = Box[i32]{ value: 42 }
let v = id[i32](b.value)
```

## Where generics are allowed

- Struct declarations: `struct Pair[T, U] { ... }`
- Enum declarations: `enum Option[T] { None, Some(T) }`
- Function declarations: `fn map[T, U](...) -> ...`
- Impl blocks: `impl[T] Box[T] { ... }`
- Calls and literals: `foo[T](...)`, `Type[T]{...}`

## Rules and constraints

- Type arguments are mandatory when a type or function has parameters.
- Type parameters cannot take type arguments of their own (`T[U]` is invalid).
- Type arguments are only allowed on calls or struct literals, not bare paths.
- `&T` references cannot be stored in structs or enums and cannot be returned.
- Reference types are only allowed as direct parameter types.

## Type checking and specialization

The type checker resolves all type parameters and validates arity at definition
and use sites. Lowering uses the typed HIR, and monomorphization runs before
codegen:

1) Type check + record fully typed HIR.
2) Monomorphize: create specialized functions/structs/enums per instantiation.
3) Codegen only sees monomorphic types.

If a generic type escapes into codegen without specialization, it is a compiler
error (generic parameters must be fully resolved before codegen).

## Implementation notes

This is implemented as an explicit monomorphization pass, not a polymorphic
backend.

- **AST/HIR surface**: type parameters are stored on functions, structs, enums,
  and impl blocks. Call and struct-literal nodes carry explicit type arguments.
- **Type checker**: `Ty::Param` represents generic parameters; all parameter and
  type argument arity is validated in `typeck::collect` and `typeck::check`.
  Type substitution is applied for field access, struct literals, and calls.
- **Typed lowering**: lowering reads the recorded type table and reuses the
  resolved `Ty` for expression types so HIR is fully typed.
- **Monomorphizer** (`capc/src/typeck/monomorphize.rs`):
  - Enqueues function instantiations encountered in calls.
  - Specializes structs/enums referenced by concrete type arguments.
  - Rewrites call targets and `Ty::Path` nodes to the specialized names.
- **Name mangling**: specialized definitions get a stable, type-based suffix
  (e.g., `Box__i32`).
- **Codegen**: assumes monomorphic input; `Ty::Param` is rejected as a hard
  error if it reaches codegen.

## Example

```cap
module generics_basic
use sys::system

struct Box[T] { value: T }

fn id[T](value: T) -> T { return value }

pub fn main(rc: RootCap) -> i32 {
  let b = Box[i32]{ value: 42 }
  let v = id[i32](b.value)
  rc.mint_console().print_i32(v)
  return 0
}
```
