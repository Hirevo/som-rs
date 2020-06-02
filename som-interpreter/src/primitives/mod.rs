mod blocks;

/// Primitives for the **Array** class.
pub mod array;
/// Primitives for the **Class** class.
pub mod class;
/// Primitives for the **Double** class.
pub mod double;
/// Primitives for the **Integer** class.
pub mod integer;
/// Primitives for the **Object** class.
pub mod object;
/// Primitives for the **String** class.
pub mod string;
/// Primitives for the **Symbol** class.
pub mod symbol;
/// Primitives for the **System** class.
pub mod system;

pub use self::blocks::{block1, block2, block3};

use crate::invokable::Return;
use crate::universe::Universe;
use crate::value::Value;

/// A interpreter primitive (just a bare function pointer).
pub type PrimitiveFn = fn(universe: &mut Universe, args: Vec<Value>) -> Return;
