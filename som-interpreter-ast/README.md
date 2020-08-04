The SOM Interpreter
===================

This is the interpreter for the Simple Object Machine.

It is AST-based, in that it works by recursively traversing and evaluating nodes from the Abstract Syntax Tree from **`som-core`**.  

Resources are managed and tracked through reference-counting (using Rust's **`Rc`**/**`Weak`** types).  
