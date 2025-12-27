Not necessarily a huge lift if you aim for Austral-style “linearity as a capability discipline” (move-only / use-once), and don’t try to recreate Rust’s borrow checker on day 1.

The lift becomes huge when you want: aliasing plus mutation plus inference plus lifetimes. Austral mostly avoids that by making “owning” explicit and keeping borrowing tame.

Here’s a practical path for your language.

Step 0: Pick a target

Start with affine (at-most-once) rather than fully linear (exactly-once). Affine is easier and covers the “no accidental duplication of caps/buffers” goal.
	•	Affine: you may drop without using (fine for error paths).
	•	Linear: you must use exactly once (harder; needs “must call” obligations).

Most languages that say “linear” in practice start affine.

Step 1: Add a notion of “ownership kind” to types

Add something like:
	•	Copy / unrestricted: ints, bools, unit, maybe string (depending on your model)
	•	Affine: capabilities/handles, buffers, owned resources

Rules:
	•	Unrestricted values can be copied freely.
	•	Affine values cannot be copied. They can be moved (transfer ownership), and after a move the original binding is dead.

In your codebase this is likely a flag on TyKind or a QualTy { ty, qual }.

Step 2: Teach the typechecker to track consumption

You already do name-resolution/typechecking; add a “typing environment” that tracks each local binding as either:
	•	Available
	•	Moved (or Consumed)

Then:
	•	Using a moved binding is an error.
	•	Assigning an affine value to a new name moves it (unless you introduce explicit clone later).
	•	Returning an affine value moves it out.

This is mostly a dataflow pass, but you can implement it as part of typechecking.

Control-flow merge is the first real complexity

You need rules for:
	•	if: both branches must leave the environment in compatible states.
	•	For affine vars: if one branch moves it and the other doesn’t, that’s an error (or you require both to move, or neither).
	•	match: same as if over all arms.
	•	loops: simplest is to disallow moving affine vars declared outside the loop unless you can prove they’re moved consistently (you can add a conservative rule first).

This is where the work is, but it’s still far smaller than Rust lifetimes.

Step 3: Decide how methods treat self

Austral-like approach: make it explicit:
	•	fn foo(self: T, ...) -> ... consumes self if T is affine.
	•	Or allow fn foo(self: &T, ...) for borrowing later.

For v1: no borrows. Just make affine methods consume self (moves).

Step 4: Composition rules for structs/enums

If a struct contains an affine field, the struct is affine unless you do something fancy.
	•	struct S { cap: Console } ⇒ S is affine
	•	Enums: if any payload is affine, the enum is affine

Pattern matching on affine values should move the payloads out (and make the scrutinee dead).

Step 5: “Drop” / resource release model

You have options:
	1.	Explicit free only (easy): user must call alloc.free(x) etc. Affine just prevents duplication.
	2.	Destructors (bigger): automatically insert drop at scope end. Needs guaranteed “exactly once” semantics or an affine “optional drop” story. This pushes you toward linear obligations + elaboration.

I’d start with (1). You already have runtime functions for frees.

Step 6: Where this interacts with your existing security model

This fits your capability design really well:
	•	Make Handle / all caps (Console, Alloc, buffers, maybe Vec) Affine
	•	Keep primitives unrestricted
	•	Keep string either unrestricted (immutable slice-ish) or affine (owned string) depending on runtime representation

Your new “don’t treat Console as Alloc” tests are type safety. Affine types add non-duplication / non-aliasing safety on top.

How big is it?
	•	Moderate if you do: affine move-only, no borrows, conservative loops.
	•	Big if you want: references/borrows with lifetimes, mutation through aliases, inference, closures capturing affine values, and RAII drop.

If you want a concrete MVP scope, I’d do:
	1.	Affine-only annotation on Handle/caps and buffers
	2.	Move tracking in locals + if/match merge
	3.	Forbid capturing affine vars in closures (if you have closures), and forbid moving outer affine vars in loops initially
	4.	No destructors; explicit free

That gets you ~80% of the “Austral feel” without stepping into borrow-checker hell.

If you tell me what constructs your language currently has (match? loops? closures? references?), I can outline the exact dataflow rules you’ll need for each and where to put them in typeck.rs.