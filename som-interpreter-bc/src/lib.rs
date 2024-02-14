//!
//! This is the interpreter for the Simple Object Machine.
//!

use std::cell::RefCell;

use som_gc::Gc;

/// Facilities for manipulating blocks.
pub mod block;
/// Facilities for manipulating classes.
pub mod class;
/// Facilities for compiling code into bytecode.
pub mod compiler;
/// Facilities for disassembling bytecode.
pub mod disassembler;
/// Facilities for manipulating stack frames.
pub mod frame;
/// Facilities for manipulating values.
pub mod hashcode;
/// Facilities for manipulating class instances.
pub mod instance;
/// Facilities for string interning.
pub mod interner;
/// The interpreter's main data structure.
pub mod interpreter;
/// Facilities for manipulating class methods.
pub mod method;
/// Definitions for all supported primitives.
pub mod primitives;
/// The collection of all known SOM objects during execution.
pub mod universe;
/// Facilities for manipulating values.
pub mod value;

/// A reference to a GC-ed mutable object.
pub type SOMRef<T> = Gc<RefCell<T>>;
