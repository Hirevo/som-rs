The Simple Object Machine
=========================

This is an implementation of the [**Simple Object Machine**], written in Rust.

[**Simple Object Machine**]: https://som-st.github.io

This repository is organized as a [**Cargo workspace**], containing multiple crates (libraries).  
Here is a rundown of these different crates (as of now, layout may change in the future):

| Name                     | Description                                                   |
| ------------------------ | ------------------------------------------------------------- |
| **`som-core`**           | Core SOM types and abstractions, shared across the workspace. |
| **`som-interpreter`**    | The SOM interpreter library and binary.                       |
| **`som-lexer`**          | The SOM lexical analyzer.                                     |
| **`som-parser-text`**    | A SOM parser that works directly with text (without a lexer). |
| **`som-parser-symbols`** | A SOM parser that works with **`som-lexer`**'s output.        |

[**Cargo workspace**]: https://doc.rust-lang.org/cargo/reference/workspaces.html

How to build and run
--------------------

The interpreter is already usable, you can use it to start evaluating from a file or to have a Read-Eval-Print Loop to play around with the language.

To compile the program, you'll need to have Rust installed on your machine.  
You can find the instructions on how to install on the [**official Rust language website**].  
We recommend using whatever is the latest stable toolchain (which was 1.44 at the time of this writing).  

[**official Rust language website**]: https://www.rust-lang.org/tools/install

Once you have Rust installed, simply run:

```bash
# the '--release' flag indicates to build with optimizations enabled.
# you can remove this flag if you wish to have more debug information in the emitted binary.
cargo build --release
```

This will compile the project and take care of fetching and building dependencies for you.  
Once the build is finished, you should have a `target/` folder created in the current directory.  
You'll find the interpreter's binary at `./target/release/som-interpreter`.  

To start the REPL, you can run:

```bash
# the '-c' flag instructs the interpreter where to find the SOM standard library.
./target/release/som-interpreter -c core-lib/Smalltalk

# you can pass multiple paths to '-c' by just keeping on adding arguments.
./target/release/som-interpreter -c core-lib/Smalltalk core-lib/Examples
```

You'll get a prompt in which you can type SOM expressions and see what they get evaluated to.  
The REPL makes the latest value successfully evaluated available via the `it` variable, so you can keep poking at the result of a previous expression, like this:

```plain
(0) SOM Shell | #(4 5 6)
returned: #(4 5 6) (Array([Integer(4), Integer(5), Integer(6)]))
(1) SOM Shell | it at: 2
returned: 5 (Integer(5))
(2) SOM Shell | it timesRepeat: [ 'hello from SOM !' println ]
hello from SOM !
hello from SOM !
hello from SOM !
hello from SOM !
hello from SOM !
returned: 5 (Integer(5))
```

To evaluate from a file, simply pass the file as another argument to the interpreter.  
But, since the '-c' accepts multiple files, you might need to add the '--' argument before that file, like so:

```bash
./target/release/som-interpreter -c core-lib/Smalltalk -- core-lib/Examples/Hello.som
```

For other purposes, you can use '-h' (or '--help') to print the complete help message:

```bash
./target/release/som-interpreter -h
```

License
-------

Unless otherwise noted (below and/or in individual files), this project is licensed under either of

- the Apache License, Version 2.0 (LICENSE-APACHE or <http://www.apache.org/licenses/LICENSE-2.0>)
- the MIT License (LICENSE-MIT or <http://opensource.org/licenses/MIT>)

at your option.

The SOM core library (in the `core-lib/` folder) is licensed under SOM's own terms:

**<https://github.com/SOM-st/SOM>**

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
