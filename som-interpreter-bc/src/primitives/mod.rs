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

use crate::interpreter::Interpreter;
use crate::universe::Universe;

/// A interpreter primitive (just a bare function pointer).
pub type PrimitiveFn = fn(interpreter: &mut Interpreter, universe: &mut Universe);

#[macro_export]
macro_rules! reverse {
    ($signature:expr, $frame:expr, [], [ $( $ptrn:pat $( => $name:ident )? ),* $(,)? ]) => {
        #[allow(unused_mut)]
        let ($($(mut $name,)?)*) = {
            $(#[allow(unreachable_patterns)]
            $(let $name =)? match $frame.borrow_mut().stack.pop() {
                Some($ptrn) => {$($name)?},
                Some(_) => panic!("'{}': wrong type", $signature),
                None => panic!("'{}': missing argument", $signature),
            };)*
            ($($($name,)?)*)
        };
    };
    ($signature:expr, $frame:expr, [ $( $first_ptrn:pat $( => $first_name:ident )? )? $(,)? ], [ $( $ptrn2:pat $( => $name2:ident )? ),* $(,)? ]) => {
        reverse!($signature, $frame, [], [ $( $first_ptrn $( => $first_name )? )? , $( $ptrn2 $( => $name2 )? ,)* ])
    };
    ($signature:expr, $frame:expr, [ $( $first_ptrn:pat $( => $first_name:ident )? )? , $( $ptrn1:pat $( => $name1:ident )? ),* $(,)? ], [ $( $ptrn2:pat $( => $name2:ident )? ),* $(,)? ]) => {
        reverse!($signature, $frame, [ $( $ptrn1 $( => $name1 )? ,)* ], [ $( $first_ptrn $( => $first_name )? ,)? $( $ptrn2 $( => $name2 )? ,)* ])
    };
}

/// Macro for checking and destructuring arguments passed to primitives.
#[macro_export]
macro_rules! expect_args {
    ($signature:expr, $frame:expr, [ $( $ptrn:pat $( => $name:ident )? ),* $(,)? ]) => {
        reverse!($signature, $frame, [ $( $ptrn $( => $name )? ,)* ], [])
    };
}
