# Problems

This document records the main design wrinkles in Capable as it exists today.

It is not a roadmap. It is a statement of the places where the language is
still more complicated, more accidental, or less settled than it should be.

## 1. The memory model is still heavy for ordinary code

Capable has a real distinction between:

- `string` as a non-owning view
- `Text` as owned/growable text
- `Vec<T>` as owned storage
- explicit frees for owned heap data

That is workable and honest, but it means routine code still needs a fair
amount of representation awareness. The language is simpler than Rust here, but
it is still not especially lightweight.

## 2. The allocator story is still mixed

Capable now has both:

- explicit `Alloc`
- a growing default-allocator surface

That is pragmatic, but the model is not fully settled. Allocation is currently
part resource handle, part policy hook, part convenience burden.

The language should eventually make this story crisp:

- default allocator for ordinary safe code
- explicit allocator for low-level or budgeted code

Until then, the stdlib will keep carrying duplicated APIs.

## 3. Expression and statement control flow are still somewhat brittle

Recent work made `let ... else` and `expr else` viable, but it also showed that
control-flow behavior was not fully uniform across parser, typechecker, and
codegen.

The language now supports these forms, but this area still needs discipline.
If more expression-oriented control-flow is added casually, complexity will
rise fast.

## 4. Traits and generics exist without a fully settled place in the language story

Traits and generics work, and they are useful, but they are not clearly part of
Capable's core identity.

The main value proposition of the language is:

- explicit authority
- small resource model
- predictable systems code

Traits and generic abstraction can help, but they can also distract from that
core if they keep expanding before the simpler story is fully stable.

## 5. The language boundary is split across compiler, stdlib stubs, and runtime intrinsics

A significant part of the real language surface is defined by the combination
of:

- stdlib `.cap` declarations
- intrinsic registration
- runtime handle tables and host functions

That is a reasonable implementation technique, but it makes some language
behavior feel more accidental than intentional. It is easy for docs, stubs, and
runtime behavior to drift unless they are kept tightly aligned.

## 6. Remote capability delegation is still unsolved work

The local model is much clearer now, but the remote story remains a separate
future project.

That is the right scope decision, but it means Capable still does not yet have
a real end-to-end answer for:

- delegated authority over the network
- revocation and lease semantics
- typed remote proxies
- constrained remote agents

The current answer is architectural intent, not shipped behavior.

## Bottom Line

Capable's biggest remaining problems are no longer "missing features".

They are mostly about choosing and enforcing a smaller number of intended ways
to write code:

- a lighter ordinary-data story
- one coherent allocation story
- a tighter boundary around abstraction features
