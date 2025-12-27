Yep — Austral’s capability system is explicitly hierarchical / attenuating: you derive a narrower capability only by providing proof of a broader one (e.g., Filesystem -> Path(root) -> Path(subdir/file)), and you can’t “go back up.”  ￼

Here’s a clean way to model the same thing in capc with what you’ve already built (affine locals + “move whole struct on affine-field move”).

1) Make “caps” affine + non-forgeable

Keep using opaque struct as “this is a capability/handle.” That gives you:
	•	cannot copy (your Token test)
	•	cannot fabricate (opaque means no public fields / no user construction unless you allow Cap{} for opaques — you probably shouldn’t)

Also: your current is_affine_type doesn’t automatically make sys.* opaque types affine. If RootCap is supposed to be the “source of authority,” it should be affine too (Austral’s root capability is special and is threaded through main).  ￼
So: add it to AFFINE_ROOTS (whatever its fully-qualified name is in your world, e.g. "sys.RootCap" or "rc.RootCap").

2) Encode attenuation as “only downward constructors”

Design the stdlib so the only way to get a capability is from a stronger one, and the API only lets you narrow.

A minimal pattern:

opaque struct RootCap
opaque struct Filesystem
opaque struct Dir
opaque struct FileRead
opaque struct FileWrite

module fs {
  // mint broad cap from root (borrow root so you can mint others too)
  pub fn filesystem(root: &RootCap) -> Filesystem

  // attenuate: Filesystem -> Dir(root)
  pub fn root_dir(fs: &Filesystem) -> Dir

  // attenuate: Dir -> Dir(subdir) (consume Dir to avoid “backtracking” unless you re-mint)
  pub fn subdir(dir: Dir, name: string) -> Dir

  // attenuate: Dir -> File caps
  pub fn open_read(dir: &Dir, name: string) -> FileRead
  pub fn open_write(dir: &Dir, name: string) -> FileWrite

  pub fn read(f: FileRead) -> string
  pub fn write(f: FileWrite, s: string) -> unit
}

Notes:
	•	Use borrows (&T) for “authority checks / minting” and moves (T) for “consuming path-like capabilities.” That matches the Austral-style feel: you can derive from a reference, but the derived things themselves are linear-ish.  ￼
	•	If you don’t have & in the language yet, you can still do attenuation with moves only, but it gets annoying (you’ll constantly lose the parent cap). Borrowing is the ergonomic escape hatch that doesn’t require Rust’s full borrow checker if you keep it simple (read-only refs, no aliasing mutation).

3) Why your Token test is not “too restrictive”

For a capability/handle, this is exactly the point:

opaque struct Token

pub fn main() -> i32 {
  let t = Token{}
  let u = t
  let v = t  // should fail
  return 0
}

It’s only “too restrictive” if Token is meant to be data (copyable value). In that case, don’t make it opaque, or give it a non-affine representation (plain struct of copyable fields).

4) The tests that actually prove “caps can’t be reused”

You want tests that cover all the ways a user might accidentally duplicate authority:

A. Local moves (baseline)
	•	move into another binding (your Token test)
	•	move via return
	•	move via function arg / method receiver

B. Control-flow joins (the hard part you just implemented)
	•	if: move in one branch then use after → should fail
	•	if: move in both branches → after join it’s moved → use after should fail
	•	match: same as if but N arms
	•	while: moving an outer affine inside loop → should fail (your conservative rule)

C. Composition (affine-by-containment)
	•	struct containing an affine field becomes affine (you already enforce by is_affine_type)
	•	enum payload containing affine becomes affine

D. Projection / field access (the “sneaky dup”)
	•	your Holder{ cap: Cap } test is perfect:
	•	let a = h.cap; let b = h.cap; must fail
	•	also add nested: h.inner.cap and h.cap.subcap shapes (to exercise your leftmost_local_in_chain)

E. Attenuation correctness (when you add it)
	•	can’t call a privileged API without the right capability type (type mismatch)
	•	can’t widen: there should be no function that maps Dir -> Filesystem in safe stdlib
	•	can’t mint without root: fs::filesystem(&fake_root) should be impossible because you can’t fabricate RootCap

If you want one “big picture” test that demonstrates the point of all this: write a tiny “untrusted dependency” module that only receives a Dir (logs directory), and verify it cannot read /etc/passwd because it can’t obtain a Filesystem or a parent path.

⸻

If you tell me what your reference/borrow syntax is (or if it exists yet), I can translate the attenuation API into exactly your AST/stdlib conventions and suggest 5–10 concrete .cap test programs (pass + fail) that lock the model in.
