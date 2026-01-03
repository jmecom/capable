# Capable

Capable is a small systems language built around capabilities: values that explicitly grant permission to perform privileged operations. It currently targets native code via Cranelift and links against a minimal runtime.

This is an experimental and toy project. Inspired by [Austral](https://austral-lang.org/).

```capable
fn main(rc: RootCap) {
  // Mint a capability from the root
  let console = rc.mint_console();
  let alloc = rc.mint_alloc_default();

  // Acquire a ReadFS capability at ./here.
  // We pass this capability struct to functions that require
  // one: otherwise the code won't compile. Moreover, the runtime
  // enforces the scope of the capability to allow only reading at ./here.
  let fs = rc.mint_readfs("./here");

  // Attempt to read beyond the capability's scopes: this will print "blocked".
  match fs.read_to_string(alloc, "../etc/passwd") {
    Ok(_)  => console.println("BUG: escaped"),
    Err(_) => console.println("blocked"),
  }
}
```

Capabilities are explicit values that grant permission to perform privileged operations (filesystem, network, clock, etc.). This is in contrast to ambient authority: normally, any code running in your process can reach the outside world, which makes dependency behavior difficult to constrain or reason about.

Capable aims to reduce supply-chain risk by making authority non-ambient: if a dependency didn’t receive a capability value, it can’t do the thing. The compiler enforces this by requiring capabilities at call sites for privileged operations, and the runtime can enforce attenuation (for example, a filesystem capability scoped to a root directory that cannot be escaped). The result is a smaller blast radius and fewer “surprising” dependency updates, because new code can’t silently acquire new powers—you have to explicitly hand them over.
