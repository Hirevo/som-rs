use std::borrow::Borrow;
use std::cell::Cell;
use std::fmt;
use std::hash::Hash;
use std::ops::Deref;
use std::ptr::NonNull;

use crate::gc_box::GcBox;
use crate::trace::Trace;

/// Represent a handle to a GC-allocated value.
#[repr(C)]
pub struct Gc<T>
where
    T: Trace + 'static,
{
    /// The pointer to the referenced `GcBox<T>`.
    pub(crate) ptr: Cell<NonNull<GcBox<T>>>,
}

impl<T> Gc<T>
where
    T: Trace + 'static,
{
    #[inline]
    pub fn as_mut_ptr(&self) -> *const T {
        unsafe { &mut self.ptr.get().as_mut().value as *mut T }
    }

    #[inline]
    pub fn as_ptr(&self) -> *const T {
        unsafe { &self.ptr.get().as_ref().value as *const T }
    }

    #[inline]
    pub fn ptr_eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

impl<T> Gc<T>
where
    T: Deref + Trace + 'static,
{
    #[inline]
    pub fn as_deref(&self) -> &T::Target {
        &**self
    }
}

impl<T> fmt::Debug for Gc<T>
where
    T: fmt::Debug + Trace + 'static,
{
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_ref().fmt(f)
    }
}

impl<T> fmt::Display for Gc<T>
where
    T: fmt::Display + Trace + 'static,
{
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.as_ref().fmt(f)
    }
}

impl<T> PartialEq for Gc<T>
where
    T: PartialEq + Trace + 'static,
{
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.as_ref().eq(other.as_ref())
    }
}

impl<T> Eq for Gc<T> where T: Eq + Trace + 'static {}

impl<T> Hash for Gc<T>
where
    T: Hash + Trace + 'static,
{
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_ref().hash(state)
    }
}

impl<T> PartialOrd for Gc<T>
where
    T: PartialOrd + Trace + 'static,
{
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.as_ref().partial_cmp(other.as_ref())
    }
}

impl<T> Ord for Gc<T>
where
    T: Ord + Trace + 'static,
{
    #[inline]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_ref().cmp(other.as_ref())
    }
}

impl<T> Borrow<T> for Gc<T>
where
    T: Trace + 'static,
{
    #[inline]
    fn borrow(&self) -> &T {
        &*self
    }
}

impl<T> AsRef<T> for Gc<T>
where
    T: Trace + 'static,
{
    #[inline]
    fn as_ref(&self) -> &T {
        &*self
    }
}

impl<T> Clone for Gc<T>
where
    T: Trace + 'static,
{
    #[inline]
    fn clone(&self) -> Self {
        Self {
            ptr: self.ptr.clone(),
        }
    }
}

impl<T> Trace for Gc<T>
where
    T: Trace + 'static,
{
    #[inline]
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

    #[inline]
    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.get().as_ref() }
    }
}
