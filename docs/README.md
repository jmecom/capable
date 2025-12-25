# Capable

Capable is a small systems language with "capabilities". It currently targets native code via Cranelift and links against a minimal runtime.

This is an experimental and toy project.

```capable
fn main(rc: RootCap) {
  // Mint a capability from the root
  let console = rc.mint_console();

  // Acquire a ReadFS capability at ./here.
  // We pass this capability struct to functions that require
  // one: otherwise the code won't compile. Moreover, the runtime
  // enforces the scope of the capability to allow only reading at ./here.
  let fs = rc.mint_readfs("./here");

  // Attempt to read beyond the capability's scopes: this will print "blocked".
  match fs.read_to_string("../etc/passwd") {
    Ok(_)  => console.println("BUG: escaped sandbox"),
    Err(_) => console.println("blocked"),
  }
}
```
