use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet, BinaryHeap, HashMap, HashSet, LinkedList, VecDeque};
use std::num::{
    NonZeroI128, NonZeroI16, NonZeroI32, NonZeroI64, NonZeroI8, NonZeroIsize, NonZeroU128,
    NonZeroU16, NonZeroU32, NonZeroU64, NonZeroU8, NonZeroUsize,
};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::atomic::{
    AtomicBool, AtomicI16, AtomicI32, AtomicI64, AtomicI8, AtomicIsize, AtomicU16, AtomicU32,
    AtomicU64, AtomicU8, AtomicUsize,
};

pub trait Trace {
    fn trace(&self);
}

macro_rules! trivial_trace {
    ($($ty:ty),* $(,)?) => {
        $(
            impl $crate::trace::Trace for $ty {
                #[inline]
                fn trace(&self) {}
            }
        )*
    };
}

trivial_trace![
    (),
    bool,
    isize,
    usize,
    i8,
    u8,
    i16,
    u16,
    i32,
    u32,
    i64,
    u64,
    i128,
    u128,
    f32,
    f64,
    char,
    String,
    Box<str>,
    Rc<str>,
    Path,
    PathBuf,
    NonZeroIsize,
    NonZeroUsize,
    NonZeroI8,
    NonZeroU8,
    NonZeroI16,
    NonZeroU16,
    NonZeroI32,
    NonZeroU32,
    NonZeroI64,
    NonZeroU64,
    NonZeroI128,
    NonZeroU128,
    AtomicBool,
    AtomicIsize,
    AtomicUsize,
    AtomicI8,
    AtomicU8,
    AtomicI16,
    AtomicU16,
    AtomicI32,
    AtomicU32,
    AtomicI64,
    AtomicU64,
];

macro_rules! tuple_trace {
    () => {};
    ($head:ident $($X:ident)*) => {
        tuple_trace!($($X)*);
        tuple_trace!(~ $head $($X)*);
    };
    (~ $($X:ident)*) => {
        #[allow(non_snake_case)]
        impl<$($X: $crate::trace::Trace),*> Trace for ($($X,)*) {
            #[inline]
            fn trace(&self) {
                let ($($X,)*) = self;
                $($X.trace();)*
            }
        }
    };
}

tuple_trace!(A_ B_ C_ D_ E_ F_ G_ H_ I_ J_ K_ L_ M_ N_ O_ P_ Q_ S_ T_ U_ V_ W_ X_ Y_ Z_);

macro_rules! iter_1_trace {
    ($($ty:ty),* $(,)?) => {
        $(
            impl<T: $crate::trace::Trace> $crate::trace::Trace for $ty {
                #[inline]
                fn trace(&self) {
                    for it in self.into_iter() {
                        it.trace();
                    }
                }
            }
        )*
    };
}

iter_1_trace!(
    Vec<T>,
    VecDeque<T>,
    LinkedList<T>,
    HashSet<T>,
    BTreeSet<T>,
    BinaryHeap<T>
);

macro_rules! iter_2_trace {
    ($($ty:ty),* $(,)?) => {
        $(
            impl<K: $crate::trace::Trace, V: $crate::trace::Trace> $crate::trace::Trace for $ty {
                #[inline]
                fn trace(&self) {
                    for (k, v) in self.into_iter() {
                        k.trace();
                        v.trace();
                    }
                }
            }
        )*
    };
}

iter_2_trace!(HashMap<K, V>, BTreeMap<K, V>);

impl<T: Trace> Trace for &[T] {
    #[inline]
    fn trace(&self) {
        for it in self.into_iter() {
            it.trace();
        }
    }
}

impl<T: Trace, const N: usize> Trace for [T; N] {
    #[inline]
    fn trace(&self) {
        for it in self.into_iter() {
            it.trace();
        }
    }
}

impl<T: Trace> Trace for RefCell<T> {
    #[inline]
    fn trace(&self) {
        self.borrow().trace();
    }
}

impl<T: Trace> Trace for Option<T> {
    #[inline]
    fn trace(&self) {
        if let Some(value) = self {
            value.trace();
        }
    }
}

impl<T: Trace> Trace for *const T {
    #[inline]
    fn trace(&self) {}
}

impl<T: Trace> Trace for *mut T {
    #[inline]
    fn trace(&self) {}
}
