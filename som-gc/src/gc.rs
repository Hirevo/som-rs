use std::cell::Cell;
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::rc::Rc;

use crate::gc_box::GcBox;
use crate::trace::Trace;

/// Represent a handle to a GC-allocated value.
pub struct Gc<T>
where
    T: Trace + 'static,
{
    /// The pointer to the referenced `GcBox<T>`.
    pub(crate) ptr: Cell<NonNull<GcBox<T>>>,
    // needed for drop-related reasons.
    pub(crate) marker: PhantomData<Rc<T>>,
}

impl<T> Clone for Gc<T>
where
    T: Trace + 'static,
{
    fn clone(&self) -> Self {
        Self {
            ptr: self.ptr.clone(),
            marker: PhantomData,
        }
    }
}

impl<T> Trace for Gc<T>
where
    T: Trace + 'static,
{
    fn trace(&self) {
        let ptr = unsafe { self.ptr.get().as_mut() };
        if !ptr.is_marked() {
            ptr.mark();
            ptr.value.trace();
        }
    }
}

impl<T> Deref for Gc<T>
where
    T: Trace + 'static,
{
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.get().as_ref() }
    }
}
