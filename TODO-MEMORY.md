TODO-MEMORY

1) Make Alloc real in runtime
   - Done: all allocating runtime paths take an Alloc handle and use it for
     malloc/free; current backend is libc, but the ABI keeps alloc explicit.

2) Fix Result<*T> typechecker bug
   - Done: type equivalence treats type params vs unqualified paths consistently
     in enum payload matching and Ok/Err checks.

3) Remove remaining runtime Vec intrinsics
   - Done: runtime Vec tables removed; Vec/Slice operations are pure Cap.

4) Owned string and slice memory model
   - string is a view over Slice<u8>; Text is owned Vec<u8>.
   - Safe non-stdlib modules may not return or store Slice/MutSlice to reduce
     use-after-free hazards until a real lifetime model exists.

5) Unsafe pointer API completeness
   - Done: memcpy/memmove added to sys::unsafe_ptr and lowered in codegen.
   - Done: ptr_is_null/ownership expectations documented.

6) ABI + lowering docs update
   - Done: opaque structs lowered to handles, field access is special-cased
     in codegen, sret + ResultOut behavior, alloc-passing convention, and
     Slice/string layout.
