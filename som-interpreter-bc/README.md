The SOM Interpreter
===================

This is the interpreter for the Simple Object Machine.

It is bytecode-based, in that it works by compiling nodes from the Abstract Syntax Tree from **`som-core`** into stack-based bytecode instructions and then executing them.  

Resources are managed and tracked through reference-counting (using Rust's **`Rc`**/**`Weak`** types).  
