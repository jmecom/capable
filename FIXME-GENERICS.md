# FIXME: Generic Collections Need Trait Bounds

## Problem

`Vec<T>` currently only supports `u8`, `i32`, and `string` element types. Custom structs are rejected:

```
error: Vec only supports u8, i32, and string element types
```

This prevents natural patterns like:

```rust
copy struct Entry {
  key: i32,
  value: i32
}

let entries = alloc.vec_new<Entry>()  // ERROR: not allowed
```

## Root Cause

The Vec implementation has methods that assume primitive operator semantics:

```rust
// From stdlib/sys/vec.cap
impl<T> Vec<T> {
    pub fn contains(self, value: T) -> bool {
        ...
        if (v == value) {  // assumes == works on T
            return true
        }
        ...
    }

    pub fn map_add(self, delta: T) -> Vec<T> {
        ...
        out.push(v + delta)  // assumes + works on T
        ...
    }
}
```

These methods use `==` and `+` directly, which only work for primitives. The type checker blocks `Vec<CustomStruct>` to prevent runtime failures.

## Solution: Trait Bounds on Impl Blocks

We need to support **partial trait bounds on impl blocks** so that:

1. Basic Vec operations (push, pop, get, set) work for any `T`
2. Methods requiring equality only exist when `T: Eq`
3. Methods requiring ordering only exist when `T: Ord`

### Proposed Syntax

```rust
// Base impl - works for all T
impl<T> Vec<T> {
    pub fn push(self, x: T) -> Result<unit, AllocErr> { ... }
    pub fn pop(self) -> Result<T, VecErr> { ... }
    pub fn get(self, i: i32) -> Result<T, VecErr> { ... }
    pub fn set(self, i: i32, x: T) -> Result<unit, VecErr> { ... }
    pub fn len(self) -> i32 { ... }
}

// Eq-bounded impl - only for T: Eq
impl<T: Eq> Vec<T> {
    pub fn contains(self, value: T) -> bool { ... }
    pub fn index_of(self, value: T) -> Result<i32, VecErr> { ... }
    pub fn count(self, value: T) -> i32 { ... }
}

// Numeric-bounded impl - only for numeric types
impl<T: Add> Vec<T> {
    pub fn map_add(self, delta: T) -> Vec<T> { ... }
}
```

### Required Traits

```rust
trait Eq {
    fn eq(self, other: Self) -> bool;
}

trait Ord: Eq {
    fn cmp(self, other: Self) -> i32;  // -1, 0, 1
}

trait Add {
    fn add(self, other: Self) -> Self;
}
```

### Builtin Implementations

The compiler should auto-generate trait impls for primitives:

```rust
impl Eq for i32 {
    fn eq(self, other: i32) -> bool { return self == other }
}

impl Eq for u8 {
    fn eq(self, other: u8) -> bool { return self == other }
}

impl Eq for string {
    fn eq(self, other: string) -> bool { return self == other }
}
```

## Implementation Steps

### Phase 1: Impl Block Trait Bounds
- [ ] Parse trait bounds on impl blocks: `impl<T: Trait> Type<T> { ... }`
- [ ] During method resolution, check if the concrete type satisfies the bounds
- [ ] Only make bounded methods available when bounds are satisfied

### Phase 2: Operator Traits
- [ ] Define `Eq`, `Ord`, `Add`, `Sub`, `Mul`, `Div` traits in stdlib
- [ ] Compiler generates impls for primitive types
- [ ] Desugar `a == b` to `a.eq(b)` when `a` is not a primitive (or always?)

### Phase 3: Derive Macros (Optional)
- [ ] Add `#[derive(Eq)]` to auto-generate equality for structs
- [ ] Field-by-field comparison for copy structs

## Workaround (Current)

Use parallel vectors instead of `Vec<Struct>`:

```rust
// Instead of Vec<Entry>
copy struct HashMap {
    states: Vec<i32>,   // 0=Empty, 1=Occupied, 2=Deleted
    keys: Vec<i32>,
    values: Vec<i32>,
    ...
}
```

## Files to Modify

- `capc/src/typeck/mod.rs` - Remove hardcoded Vec element type restriction (lines 916-923, 936-943)
- `capc/src/typeck/collect.rs` - Collect trait bounds on impl blocks
- `capc/src/typeck/check.rs` - Validate trait bounds during method resolution
- `stdlib/sys/vec.cap` - Split impl blocks by trait bounds
