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

## Remaining Work

### Vec Equality Methods Still Use `==` Directly

Methods like `contains`, `filter`, `index_of`, `count` still use `==` which doesn't work for custom structs:

```rust
// Currently in vec.cap:
if (v == value) {  // doesn't work for structs
    return true
}
```

**Why it's tricky**: When we tried moving these to `impl<T: eq::Eq> Vec<T>`, we encountered issues with method name collisions between type-specific impls (e.g., `impl Vec<u8>` and `impl Vec<i32>` both wanting `map_add`).

### Solutions Needed

1. **Fix method name mangling** for type-specific impls - include type args in method name to avoid collisions
2. **Or**: Move equality methods to a separate module/struct
3. **Or**: Keep `==` for primitives, require explicit trait bound for custom types

### Operator Traits (Not Yet Implemented)

To properly use `.eq()` in Vec methods, we need:
- Trait bounds on impl blocks to work correctly
- Type checker to resolve trait methods inside bounded impls

### Example of Working Pattern

Custom structs can implement `Eq` and use it in user code:

```rust
use sys::eq

trait Eq {
  fn eq(self, other: Self) -> bool;
}

copy struct Entry { key: i32, value: i32 }

impl Eq for Entry {
  fn eq(self, other: Entry) -> bool {
    return self.key == other.key && self.value == other.value
  }
}

// Works in user code:
fn are_equal<T: Eq>(a: T, b: T) -> bool {
  return a.eq(b)
}
```

## Files Modified

- `capc/src/typeck/mod.rs` - Removed hardcoded Vec element type restriction
- `capc/src/typeck/monomorphize.rs` - Added `find_type_in_all_modules` for cross-module type lookup
- `stdlib/sys/eq.cap` - New file with `Eq` trait and primitive implementations
