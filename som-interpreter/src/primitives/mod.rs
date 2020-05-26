pub mod array;
pub mod object;
pub mod double;
pub mod integer;

use crate::universe::Universe;
use crate::value::Value;

pub type PrimitiveFn = fn(universe: &mut Universe, args: Vec<Value>) -> Value;
