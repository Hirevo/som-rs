use std::convert::TryFrom;

use anyhow::{bail, Context, Error};

use num_bigint::BigInt;
use som_gc::{Gc, GcHeap};

use crate::block::Block;
use crate::class::Class;
use crate::instance::Instance;
use crate::interner::Interned;
use crate::interpreter::Interpreter;
use crate::method::Method;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::SOMValue;
use crate::SOMRef;

pub trait IntoValue {
    fn into_value(&self, heap: &mut GcHeap) -> SOMValue;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Nil;

impl TryFrom<SOMValue> for Nil {
    type Error = Error;

    fn try_from(value: SOMValue) -> Result<Self, Self::Error> {
        if value.is_nil() {
            Ok(Self)
        } else {
            bail!("could not resolve `SOMValue` as `Nil`");
        }
    }
}

impl FromArgs for Nil {
    fn from_args(
        interpreter: &mut Interpreter,
        _: &mut GcHeap,
        _: &mut Universe,
    ) -> Result<Self, Error> {
        let value = interpreter
            .stack
            .pop()
            .context("message send with missing argument")?;

        Self::try_from(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct System;

impl TryFrom<SOMValue> for System {
    type Error = Error;

    fn try_from(value: SOMValue) -> Result<Self, Self::Error> {
        if value.is_nil() {
            Ok(Self)
        } else {
            bail!("could not resolve `SOMValue` as `System`");
        }
    }
}

impl FromArgs for System {
    fn from_args(
        interpreter: &mut Interpreter,
        _: &mut GcHeap,
        _: &mut Universe,
    ) -> Result<Self, Error> {
        let value = interpreter
            .stack
            .pop()
            .context("message send with missing argument")?;

        Self::try_from(value)
    }
}

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
        let value = interpreter
            .stack
            .pop()
            .context("message send with missing argument")?;

        Self::try_from(value)
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
        let value = interpreter
            .stack
            .pop()
            .context("message send with missing argument")?;

        Self::try_from(value)
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
        let value = interpreter
            .stack
            .pop()
            .context("message send with missing argument")?;

        Self::try_from(value)
    }
}

pub trait FromArgs: Sized {
    fn from_args(
        interpreter: &mut Interpreter,
        heap: &mut GcHeap,
        universe: &mut Universe,
    ) -> Result<Self, Error>;
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

impl IntoValue for Gc<BigInt> {
    fn into_value(&self, _: &mut GcHeap) -> SOMValue {
        SOMValue::new_big_integer(self)
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

pub trait Primitive<T>: Sized + Send + Sync + 'static {
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

pub trait IntoReturn {
    fn into_return(self, interpreter: &mut Interpreter, heap: &mut GcHeap) -> Result<(), Error>;
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
        impl <$($ty: $crate::convert::IntoValue),*> $crate::convert::IntoValue for ($($ty),*,) {
            fn into_value(&self, heap: &mut ::som_gc::GcHeap) -> $crate::value::SOMValue {
                #[allow(non_snake_case)]
                let ($($ty),*,) = self;
                let mut values = Vec::default();
                $(
                    values.push($crate::convert::IntoValue::into_value($ty, heap));
                )*
                let allocated = heap.allocate(::std::cell::RefCell::new(values));
                $crate::value::SOMValue::new_array(&allocated)
            }
        }

        impl <$($ty: $crate::convert::FromArgs),*> $crate::convert::FromArgs for ($($ty),*,) {
            fn from_args(interpreter: &mut $crate::interpreter::Interpreter, heap: &mut som_gc::GcHeap, universe: &mut $crate::universe::Universe) -> Result<Self, Error> {
                $(
                    #[allow(non_snake_case)]
                    let $ty = $ty::from_args(interpreter, heap, universe)?;
                )*
                Ok(($($ty),*,))
            }
        }

        impl <F, R, $($ty),*> $crate::convert::Primitive<($($ty),*,)> for F
        where
            F: Fn(&mut $crate::interpreter::Interpreter, &mut som_gc::GcHeap, &mut $crate::universe::Universe, $($ty),*) -> Result<R, Error> + Send + Sync + 'static,
            R: $crate::convert::IntoReturn,
            $($ty: $crate::convert::FromArgs),*,
        {
            fn invoke(&self, interpreter: &mut $crate::interpreter::Interpreter, heap: &mut som_gc::GcHeap, universe: &mut $crate::universe::Universe) -> Result<(), Error> {
                reverse!(interpreter, heap, universe, [$($ty),*], []);
                let result = (self)(interpreter, heap, universe, $($ty),*,)?;
                result.into_return(interpreter, heap)
            }
        }
    };
}

impl<T: IntoValue> IntoReturn for T {
    fn into_return(self, interpreter: &mut Interpreter, heap: &mut GcHeap) -> Result<(), Error> {
        interpreter.stack.push(self.into_value(heap));
        Ok(())
    }
}

impl IntoValue for SOMValue {
    fn into_value(&self, _: &mut GcHeap) -> SOMValue {
        *self
    }
}

impl IntoValue for Nil {
    fn into_value(&self, _: &mut GcHeap) -> SOMValue {
        SOMValue::NIL
    }
}

impl IntoValue for System {
    fn into_value(&self, _: &mut GcHeap) -> SOMValue {
        SOMValue::SYSTEM
    }
}

impl<T: IntoValue> IntoValue for Option<T> {
    fn into_value(&self, heap: &mut GcHeap) -> SOMValue {
        self.as_ref()
            .map_or(SOMValue::NIL, |it| it.into_value(heap))
    }
}

impl IntoReturn for () {
    fn into_return(self, _: &mut Interpreter, _: &mut GcHeap) -> Result<(), Error> {
        Ok(())
    }
}

impl IntoValue for StringLike {
    fn into_value(&self, heap: &mut GcHeap) -> SOMValue {
        match self {
            StringLike::String(value) => value.into_value(heap),
            StringLike::Symbol(value) => value.into_value(heap),
        }
    }
}

impl IntoValue for IntegerLike {
    fn into_value(&self, heap: &mut GcHeap) -> SOMValue {
        match self {
            IntegerLike::Integer(value) => value.into_value(heap),
            IntegerLike::BigInteger(value) => value.into_value(heap),
        }
    }
}

impl IntoValue for DoubleLike {
    fn into_value(&self, heap: &mut GcHeap) -> SOMValue {
        match self {
            DoubleLike::Double(value) => value.into_value(heap),
            DoubleLike::Integer(value) => value.into_value(heap),
            DoubleLike::BigInteger(value) => value.into_value(heap),
        }
    }
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
