# Generic Collections: Progress and Remaining Work

## Completed

### Phase 1: Basic Vec<T> for Custom Structs

`Vec<T>` now supports custom structs for basic operations:

```rust
copy struct Entry {
  key: i32,
  value: i32
}

let entries = alloc.vec_new<Entry>()  // WORKS!
entries.push(Entry { key: 1, value: 10 })
let e = entries.get(0)
```

**Working methods**: `push`, `pop`, `get`, `set`, `len`, `capacity`, `reserve`, `clear`, `free`

**Changes made**:
- Removed hardcoded Vec element type restriction in `typeck/mod.rs`
- Fixed monomorphization to search all modules for user-defined types
- Created `stdlib/sys/eq.cap` with `Eq` trait and impls for primitives

### Phase 2: Trait-Bounded Impl Blocks

Fixed method name mangling and added trait-bounded impl blocks for Vec:

```rust
use sys::eq

copy struct Entry { key: i32, value: i32 }

impl eq::Eq for Entry {
  fn eq(self, other: Entry) -> bool {
    return self.key == other.key && self.value == other.value
  }
}

let v = alloc.vec_new<Entry>()
v.push(Entry { key: 1, value: 10 })
v.push(Entry { key: 2, value: 20 })

// These now work with custom structs!
v.contains_eq(Entry { key: 1, value: 10 })  // true
v.count_eq(Entry { key: 1, value: 10 })     // 1
v.index_of_eq(Entry { key: 2, value: 20 })  // Ok(1)
v.filter_eq(Entry { key: 1, value: 10 })    // Vec with matching entries
```

**New Eq-based methods** (in `impl<T: eq::Eq> Vec<T>`):
- `contains_eq(value: T) -> bool`
- `count_eq(value: T) -> i32`
- `index_of_eq(value: T) -> Result<i32, VecErr>`
- `last_index_of_eq(value: T) -> Result<i32, VecErr>`
- `filter_eq(value: T) -> Vec<T>`

**Original methods** (in `impl<T> Vec<T>`) still use `==` for primitives:
- `contains`, `count`, `index_of`, `last_index_of`, `filter`

**Changes made**:
- `capc/src/typeck/mod.rs` - Added `build_type_arg_suffix()` helper for method name mangling
- `capc/src/typeck/check.rs` - Updated method resolution to try type-specific methods first
- `capc/src/typeck/lower.rs` - Updated HIR lowering to use correct method names
- `stdlib/sys/vec.cap` - Added `impl<T: eq::Eq> Vec<T>` with Eq-based methods

## Design Decisions

### Why `contains_eq` instead of replacing `contains`?

We kept the original `contains` (using `==`) for backwards compatibility and added new
`*_eq` variants that use the `Eq` trait. This means:

1. Primitives (i32, u8, bool) can use either `contains` or `contains_eq`
2. Custom structs must use `contains_eq` (and implement `eq::Eq`)
3. No breaking changes to existing code

### Type-Specific Impl Blocks

Methods like `map_add` that use arithmetic operators are in type-specific impl blocks:

```rust
impl Vec<i32> {
  pub fn map_add(self, delta: i32) -> Vec<i32> { ... }
}

impl Vec<u8> {
  pub fn map_add(self, delta: u8) -> Vec<u8> { ... }
}
```

This avoids needing an `Add` trait (which doesn't exist yet).

## Files Modified

- `capc/src/typeck/mod.rs` - Removed Vec restriction, added `build_type_arg_suffix`
- `capc/src/typeck/check.rs` - Type-specific method resolution
- `capc/src/typeck/lower.rs` - Correct method name generation in HIR
- `capc/src/typeck/monomorphize.rs` - Cross-module type lookup
- `stdlib/sys/eq.cap` - `Eq` trait and primitive implementations
- `stdlib/sys/vec.cap` - Added bounded impl block with Eq-based methods
- `tests/programs/vec_custom_eq.cap` - Test for custom struct with Eq
