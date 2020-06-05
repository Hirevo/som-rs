mod blocks;

/// Primitives for the **Array** class.
pub mod array;
/// Primitives for the **Class** class.
pub mod class;
/// Primitives for the **Double** class.
pub mod double;
/// Primitives for the **Integer** class.
pub mod integer;
/// Primitives for the **Method** class and the **Primitive** class.
pub mod method;
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

/// Macro for checking and destructure arguments passed to primitives.
#[macro_export]
macro_rules! expect_args {
    ($signature:expr, $args:expr, [ $( $ptrn:pat $( => $name:ident )? ),* $(,)? ]) => {
        #[allow(unused_mut)]
        let ($($(mut $name,)?)*) = {
            #[allow(unused_variables, unused_mut)]
            let mut iter = $args.into_iter();
            $(#[allow(unreachable_patterns)]
            $(let $name =)? match iter.next() {
                Some($ptrn) => {$($name)?},
                Some(_) => return Return::Exception(format!("'{}': wrong type", $signature)),
                None => return Return::Exception(format!("'{}': missing argument", $signature)),
            };)*
            ($($($name,)?)*)
        };
    };
}
