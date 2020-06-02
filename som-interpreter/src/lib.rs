//!
//! This is the interpreter for the Simple Object Machine.
//!

use std::cell::RefCell;
use std::rc::{Rc, Weak};

/// Facilities for manipulating blocks.
pub mod block;
/// Facilities for manipulating classes.
pub mod class;
/// Facilities for evaluating nodes and expressions.
pub mod evaluate;
/// Facilities for manipulating stack frames.
pub mod frame;
/// Facilities for manipulating class instances.
pub mod instance;
/// Facilities for string interning.
pub mod interner;
/// Facilities for invoking methods and/or primitives.
pub mod invokable;
/// Definitions for all supported primitives.
pub mod primitives;
/// The interpreter's main data structure.
pub mod universe;
/// Facilities for manipulating values.
pub mod value;

/// A strong and owning reference to an object.
pub type SOMRef<T> = Rc<RefCell<T>>;
/// A weak reference to an object.
pub type SOMWeakRef<T> = Weak<RefCell<T>>;
