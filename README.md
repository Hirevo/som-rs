The Simple Object Machine
=========================

This is an implementation of the [**Simple Object Machine**], written in Rust.

[**Simple Object Machine**]: https://som-st.github.io

This repository is organized as a [**Cargo workspace**], containing multiple crates (libraries).  
Here is a rundown of these different crates (as of now, layout may change in the future):

| Name                     | Description                                                   |
| ------------------------ | ------------------------------------------------------------- |
| **`som-core`**           | Core types and abstractions, shared across the workspace.     |
| **`som-interpreter`**    | The SOM interpreter library.                                  |
| **`som-lexer`**          | The SOM lexical analyzer.                                     |
| **`som-parser-text`**    | A SOM parser that works directly with text (without a lexer). |
| **`som-parser-symbols`** | A SOM parser that works with the lexer's output.              |

[**Cargo workspace**]: https://doc.rust-lang.org/cargo/reference/workspaces.html
