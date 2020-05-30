pub mod array;
pub mod double;
pub mod integer;
pub mod object;

use crate::universe::Universe;
use crate::value::Value;

pub type PrimitiveFn = fn(universe: &mut Universe, args: Vec<Value>) -> Value;
