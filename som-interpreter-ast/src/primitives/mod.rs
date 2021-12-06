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

/// Macro for checking and destructuring arguments passed to primitives.
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

pub fn get_class_primitives(
    class_name: &str,
) -> Option<&'static [(&'static str, PrimitiveFn, bool)]> {
    match class_name {
        "Array" => Some(self::array::CLASS_PRIMITIVES),
        "Block1" => Some(self::block1::CLASS_PRIMITIVES),
        "Block2" => Some(self::block2::CLASS_PRIMITIVES),
        "Block3" => Some(self::block3::CLASS_PRIMITIVES),
        "Class" => Some(self::class::CLASS_PRIMITIVES),
        "Double" => Some(self::double::CLASS_PRIMITIVES),
        "Integer" => Some(self::integer::CLASS_PRIMITIVES),
        "Method" => Some(self::method::CLASS_PRIMITIVES),
        "Primitive" => Some(self::method::CLASS_PRIMITIVES),
        "Object" => Some(self::object::CLASS_PRIMITIVES),
        "String" => Some(self::string::CLASS_PRIMITIVES),
        "Symbol" => Some(self::symbol::CLASS_PRIMITIVES),
        "System" => Some(self::system::CLASS_PRIMITIVES),
        _ => None,
    }
}

pub fn get_instance_primitives(
    class_name: &str,
) -> Option<&'static [(&'static str, PrimitiveFn, bool)]> {
    match class_name {
        "Array" => Some(self::array::INSTANCE_PRIMITIVES),
        "Block1" => Some(self::block1::INSTANCE_PRIMITIVES),
        "Block2" => Some(self::block2::INSTANCE_PRIMITIVES),
        "Block3" => Some(self::block3::INSTANCE_PRIMITIVES),
        "Class" => Some(self::class::INSTANCE_PRIMITIVES),
        "Double" => Some(self::double::INSTANCE_PRIMITIVES),
        "Integer" => Some(self::integer::INSTANCE_PRIMITIVES),
        "Method" => Some(self::method::INSTANCE_PRIMITIVES),
        "Primitive" => Some(self::method::INSTANCE_PRIMITIVES),
        "Object" => Some(self::object::INSTANCE_PRIMITIVES),
        "String" => Some(self::string::INSTANCE_PRIMITIVES),
        "Symbol" => Some(self::symbol::INSTANCE_PRIMITIVES),
        "System" => Some(self::system::INSTANCE_PRIMITIVES),
        _ => None,
    }
}
