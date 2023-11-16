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

use std::convert::TryFrom;

use anyhow::{bail, Context, Error};
use num_bigint::BigInt;

use som_gc::{Gc, GcHeap};

pub use self::blocks::{block1, block2, block3};

use crate::block::Block;
use crate::class::Class;
use crate::instance::Instance;
use crate::interner::Interned;
use crate::interpreter::Interpreter;
use crate::method::Method;
use crate::universe::Universe;
use crate::value::SOMValue;
use crate::SOMRef;

#[derive(Debug, Clone)]
pub enum StringLike {
    String(Gc<String>),
    Symbol(Interned),
}

impl TryFrom<SOMValue> for StringLike {
    type Error = Error;

    fn try_from(value: SOMValue) -> Result<Self, Self::Error> {
        value
            .as_string()
            .map(Self::String)
            .or_else(|| value.as_symbol().map(Self::Symbol))
            .context("could not resolve `SOMValue` as `String`, or `Symbol`")
    }
}

impl FromArgs for StringLike {
    fn from_args(
        interpreter: &mut Interpreter,
        _: &mut GcHeap,
        _: &mut Universe,
    ) -> Result<Self, Error> {
        let arg = interpreter
            .stack
            .pop()
            .context("message send with missing argument")?;

        arg.as_string()
            .map(Self::String)
            .or_else(|| arg.as_symbol().map(Self::Symbol))
            .context("could not resolve `SOMValue` as `String`, or `Symbol`")
    }
}

#[derive(Debug, Clone)]
pub enum DoubleLike {
    Double(f64),
    Integer(i32),
    BigInteger(Gc<BigInt>),
}

impl TryFrom<SOMValue> for DoubleLike {
    type Error = Error;

    fn try_from(value: SOMValue) -> Result<Self, Self::Error> {
        value
            .as_double()
            .map(Self::Double)
            .or_else(|| value.as_integer().map(Self::Integer))
            .or_else(|| value.as_big_integer().map(Self::BigInteger))
            .context("could not resolve `SOMValue` as `Double`, `Integer`, or `BigInteger`")
    }
}

impl FromArgs for DoubleLike {
    fn from_args(
        interpreter: &mut Interpreter,
        _: &mut GcHeap,
        _: &mut Universe,
    ) -> Result<Self, Error> {
        let arg = interpreter
            .stack
            .pop()
            .context("message send with missing argument")?;

        arg.as_double()
            .map(Self::Double)
            .or_else(|| arg.as_integer().map(Self::Integer))
            .or_else(|| arg.as_big_integer().map(Self::BigInteger))
            .context("could not resolve `SOMValue` as `Double`, `Integer`, or `BigInteger`")
    }
}

#[derive(Debug, Clone)]
pub enum IntegerLike {
    Integer(i32),
    BigInteger(Gc<BigInt>),
}

impl TryFrom<SOMValue> for IntegerLike {
    type Error = Error;

    fn try_from(value: SOMValue) -> Result<Self, Self::Error> {
        value
            .as_integer()
            .map(Self::Integer)
            .or_else(|| value.as_big_integer().map(Self::BigInteger))
            .context("could not resolve `SOMValue` as `Integer`, or `BigInteger`")
    }
}

impl FromArgs for IntegerLike {
    fn from_args(
        interpreter: &mut Interpreter,
        _: &mut GcHeap,
        _: &mut Universe,
    ) -> Result<Self, Error> {
        let arg = interpreter
            .stack
            .pop()
            .context("message send with missing argument")?;

        arg.as_integer()
            .map(Self::Integer)
            .or_else(|| arg.as_big_integer().map(Self::BigInteger))
            .context("could not resolve `SOMValue` as `Integer`, or `BigInteger`")
    }
}

pub trait FromArgs: Sized {
    fn from_args(
        interpreter: &mut Interpreter,
        heap: &mut GcHeap,
        universe: &mut Universe,
    ) -> Result<Self, Error>;
}

impl FromArgs for () {
    fn from_args(
        interpreter: &mut Interpreter,
        _: &mut GcHeap,
        _: &mut Universe,
    ) -> Result<Self, Error> {
        let arg = interpreter
            .stack
            .pop()
            .context("message send with missing argument")?;

        if arg.is_nil() {
            Ok(())
        } else {
            bail!("could not resolve `SOMValue` as `Nil`");
        }
    }
}

impl FromArgs for SOMValue {
    fn from_args(
        interpreter: &mut Interpreter,
        _: &mut GcHeap,
        _: &mut Universe,
    ) -> Result<Self, Error> {
        interpreter
            .stack
            .pop()
            .context("message send with missing argument")
    }
}

impl FromArgs for bool {
    fn from_args(
        interpreter: &mut Interpreter,
        _: &mut GcHeap,
        _: &mut Universe,
    ) -> Result<Self, Error> {
        let arg = interpreter
            .stack
            .pop()
            .context("message send with missing argument")?;
        arg.as_boolean()
            .context("could not resolve `SOMValue` as `Boolean`")
    }
}

impl FromArgs for i32 {
    fn from_args(
        interpreter: &mut Interpreter,
        _: &mut GcHeap,
        _: &mut Universe,
    ) -> Result<Self, Error> {
        let arg = interpreter
            .stack
            .pop()
            .context("message send with missing argument")?;
        arg.as_integer()
            .context("could not resolve `SOMValue` as `Integer`")
    }
}

impl FromArgs for f64 {
    fn from_args(
        interpreter: &mut Interpreter,
        _: &mut GcHeap,
        _: &mut Universe,
    ) -> Result<Self, Error> {
        let arg = interpreter
            .stack
            .pop()
            .context("message send with missing argument")?;
        arg.as_double()
            .context("could not resolve `SOMValue` as `Double`")
    }
}

impl FromArgs for Interned {
    fn from_args(
        interpreter: &mut Interpreter,
        _: &mut GcHeap,
        _: &mut Universe,
    ) -> Result<Self, Error> {
        let arg = interpreter
            .stack
            .pop()
            .context("message send with missing argument")?;
        arg.as_symbol()
            .context("could not resolve `SOMValue` as `Symbol`")
    }
}

impl FromArgs for Gc<String> {
    fn from_args(
        interpreter: &mut Interpreter,
        _: &mut GcHeap,
        _: &mut Universe,
    ) -> Result<Self, Error> {
        let arg = interpreter
            .stack
            .pop()
            .context("message send with missing argument")?;
        arg.as_string()
            .context("could not resolve `SOMValue` as `String`")
    }
}

impl FromArgs for SOMRef<Vec<SOMValue>> {
    fn from_args(
        interpreter: &mut Interpreter,
        _: &mut GcHeap,
        _: &mut Universe,
    ) -> Result<Self, Error> {
        let arg = interpreter
            .stack
            .pop()
            .context("message send with missing argument")?;
        arg.as_array()
            .context("could not resolve `SOMValue` as `Array`")
    }
}

impl FromArgs for SOMRef<Class> {
    fn from_args(
        interpreter: &mut Interpreter,
        _: &mut GcHeap,
        _: &mut Universe,
    ) -> Result<Self, Error> {
        let arg = interpreter
            .stack
            .pop()
            .context("message send with missing argument")?;
        arg.as_class()
            .context("could not resolve `SOMValue` as `Class`")
    }
}

impl FromArgs for SOMRef<Instance> {
    fn from_args(
        interpreter: &mut Interpreter,
        _: &mut GcHeap,
        _: &mut Universe,
    ) -> Result<Self, Error> {
        let arg = interpreter
            .stack
            .pop()
            .context("message send with missing argument")?;
        arg.as_instance()
            .context("could not resolve `SOMValue` as `Instance`")
    }
}

impl FromArgs for Gc<Block> {
    fn from_args(
        interpreter: &mut Interpreter,
        _: &mut GcHeap,
        _: &mut Universe,
    ) -> Result<Self, Error> {
        let arg = interpreter
            .stack
            .pop()
            .context("message send with missing argument")?;
        arg.as_block()
            .context("could not resolve `SOMValue` as `Block`")
    }
}

impl FromArgs for Gc<Method> {
    fn from_args(
        interpreter: &mut Interpreter,
        _: &mut GcHeap,
        _: &mut Universe,
    ) -> Result<Self, Error> {
        let arg = interpreter
            .stack
            .pop()
            .context("message send with missing argument")?;
        arg.as_invokable()
            .context("could not resolve `SOMValue` as `Method`")
    }
}

pub trait IntoValue {
    fn into_value(&self, heap: &mut GcHeap) -> SOMValue;
}

impl IntoValue for bool {
    fn into_value(&self, _: &mut GcHeap) -> SOMValue {
        SOMValue::new_boolean(*self)
    }
}

impl IntoValue for i32 {
    fn into_value(&self, _: &mut GcHeap) -> SOMValue {
        SOMValue::new_integer(*self)
    }
}

impl IntoValue for f64 {
    fn into_value(&self, _: &mut GcHeap) -> SOMValue {
        SOMValue::new_double(*self)
    }
}

impl IntoValue for Interned {
    fn into_value(&self, _: &mut GcHeap) -> SOMValue {
        SOMValue::new_symbol(*self)
    }
}

impl IntoValue for Gc<String> {
    fn into_value(&self, _: &mut GcHeap) -> SOMValue {
        SOMValue::new_string(self)
    }
}

impl IntoValue for SOMRef<Vec<SOMValue>> {
    fn into_value(&self, _: &mut GcHeap) -> SOMValue {
        SOMValue::new_array(self)
    }
}

impl IntoValue for SOMRef<Class> {
    fn into_value(&self, _: &mut GcHeap) -> SOMValue {
        SOMValue::new_class(self)
    }
}

impl IntoValue for SOMRef<Instance> {
    fn into_value(&self, _: &mut GcHeap) -> SOMValue {
        SOMValue::new_instance(self)
    }
}

impl IntoValue for Gc<Block> {
    fn into_value(&self, _: &mut GcHeap) -> SOMValue {
        SOMValue::new_block(self)
    }
}

impl IntoValue for Gc<Method> {
    fn into_value(&self, _: &mut GcHeap) -> SOMValue {
        SOMValue::new_invokable(self)
    }
}

pub trait Primitive<T: FromArgs>: Sized + Send + Sync + 'static {
    fn invoke(
        &self,
        interpreter: &mut Interpreter,
        heap: &mut GcHeap,
        universe: &mut Universe,
    ) -> Result<(), Error>;

    fn into_func(self) -> &'static PrimitiveFn {
        let boxed = Box::new(
            move |interpreter: &mut Interpreter, heap: &mut GcHeap, universe: &mut Universe| {
                self.invoke(interpreter, heap, universe)
            },
        );
        Box::leak(boxed)
    }
}

macro_rules! reverse {
    ($interpreter:expr, $heap:expr, $universe:expr, [], [ $($ty:ident),* $(,)? ]) => {
        $(
            #[allow(non_snake_case)]
            let $ty = $ty::from_args($interpreter, $heap, $universe)?;
        )*
    };
    ($interpreter:expr, $heap:expr, $universe:expr, [ $ty:ident $(,)? ], [ $($ty2:ident),* $(,)? ]) => {
        reverse!($interpreter, $heap, $universe, [], [ $ty , $($ty2),* ])
    };
    ($interpreter:expr, $heap:expr, $universe:expr, [ $ty:ident , $($ty1:ident),* $(,)? ], [ $($ty2:ident),* $(,)? ]) => {
        reverse!($interpreter, $heap, $universe, [ $($ty1),* ], [ $ty , $($ty2),* ])
    };
}

macro_rules! derive_stuff {
    ($($ty:ident),* $(,)?) => {
        impl <$($ty: $crate::primitives::IntoValue),*> $crate::primitives::IntoValue for ($($ty),*,) {
            fn into_value(&self, heap: &mut ::som_gc::GcHeap) -> $crate::value::SOMValue {
                #[allow(non_snake_case)]
                let ($($ty),*,) = self;
                let mut values = Vec::default();
                $(
                    values.push($crate::primitives::IntoValue::into_value($ty, heap));
                )*
                let allocated = heap.allocate(::std::cell::RefCell::new(values));
                $crate::value::SOMValue::new_array(&allocated)
            }
        }

        impl <$($ty: $crate::primitives::FromArgs),*> $crate::primitives::FromArgs for ($($ty),*,) {
            fn from_args(interpreter: &mut $crate::interpreter::Interpreter, heap: &mut som_gc::GcHeap, universe: &mut $crate::universe::Universe) -> Result<Self, Error> {
                $(
                    #[allow(non_snake_case)]
                    let $ty = $ty::from_args(interpreter, heap, universe)?;
                )*
                Ok(($($ty),*,))
            }
        }

        impl <F, $($ty),*> $crate::primitives::Primitive<($($ty),*,)> for F
        where
            F: Fn(&mut $crate::interpreter::Interpreter, &mut som_gc::GcHeap, &mut $crate::universe::Universe, $($ty),*) -> Result<(), Error> + Send + Sync + 'static,
            $($ty: $crate::primitives::FromArgs),*,
        {
            fn invoke(&self, interpreter: &mut $crate::interpreter::Interpreter, heap: &mut som_gc::GcHeap, universe: &mut $crate::universe::Universe) -> Result<(), Error> {
                reverse!(interpreter, heap, universe, [$($ty),*], []);
                (self)(interpreter, heap, universe, $($ty),*,)
            }
        }
    };
}

impl<F> Primitive<()> for F
where
    F: Fn(&mut Interpreter, &mut GcHeap, &mut Universe) -> Result<(), Error>
        + Send
        + Sync
        + 'static,
{
    fn invoke(
        &self,
        interpreter: &mut Interpreter,
        heap: &mut GcHeap,
        universe: &mut Universe,
    ) -> Result<(), Error> {
        (self)(interpreter, heap, universe)
    }
}

derive_stuff!(_A);
derive_stuff!(_A, _B);
derive_stuff!(_A, _B, _C);
derive_stuff!(_A, _B, _C, _D);
derive_stuff!(_A, _B, _C, _D, _E);
derive_stuff!(_A, _B, _C, _D, _E, _F);

/// An interpreter primitive.
pub type PrimitiveFn = dyn Fn(&mut Interpreter, &mut GcHeap, &mut Universe) -> Result<(), Error>
    + Send
    + Sync
    + 'static;

pub fn get_class_primitives(
    class_name: &str,
) -> Option<&'static [(&'static str, &'static PrimitiveFn, bool)]> {
    match class_name {
        "Array" => Some(self::array::CLASS_PRIMITIVES.as_ref()),
        "Block1" => Some(self::block1::CLASS_PRIMITIVES.as_ref()),
        "Block2" => Some(self::block2::CLASS_PRIMITIVES.as_ref()),
        "Block3" => Some(self::block3::CLASS_PRIMITIVES.as_ref()),
        "Class" => Some(self::class::CLASS_PRIMITIVES.as_ref()),
        "Double" => Some(self::double::CLASS_PRIMITIVES.as_ref()),
        "Integer" => Some(self::integer::CLASS_PRIMITIVES.as_ref()),
        "Method" => Some(self::method::CLASS_PRIMITIVES.as_ref()),
        "Primitive" => Some(self::method::CLASS_PRIMITIVES.as_ref()),
        "Object" => Some(self::object::CLASS_PRIMITIVES.as_ref()),
        "String" => Some(self::string::CLASS_PRIMITIVES.as_ref()),
        "Symbol" => Some(self::symbol::CLASS_PRIMITIVES.as_ref()),
        "System" => Some(self::system::CLASS_PRIMITIVES.as_ref()),
        _ => None,
    }
}

pub fn get_instance_primitives(
    class_name: &str,
) -> Option<&'static [(&'static str, &'static PrimitiveFn, bool)]> {
    match class_name {
        "Array" => Some(self::array::INSTANCE_PRIMITIVES.as_ref()),
        "Block1" => Some(self::block1::INSTANCE_PRIMITIVES.as_ref()),
        "Block2" => Some(self::block2::INSTANCE_PRIMITIVES.as_ref()),
        "Block3" => Some(self::block3::INSTANCE_PRIMITIVES.as_ref()),
        "Class" => Some(self::class::INSTANCE_PRIMITIVES.as_ref()),
        "Double" => Some(self::double::INSTANCE_PRIMITIVES.as_ref()),
        "Integer" => Some(self::integer::INSTANCE_PRIMITIVES.as_ref()),
        "Method" => Some(self::method::INSTANCE_PRIMITIVES.as_ref()),
        "Primitive" => Some(self::method::INSTANCE_PRIMITIVES.as_ref()),
        "Object" => Some(self::object::INSTANCE_PRIMITIVES.as_ref()),
        "String" => Some(self::string::INSTANCE_PRIMITIVES.as_ref()),
        "Symbol" => Some(self::symbol::INSTANCE_PRIMITIVES.as_ref()),
        "System" => Some(self::system::INSTANCE_PRIMITIVES.as_ref()),
        _ => None,
    }
}
