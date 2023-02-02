use std::ops::{Deref, DerefMut};
use std::cell::Cell;
use std::ptr::NonNull;

use crate::Trace;

/// Represents a value, as it is stored within the GC.
#[repr(C)]
pub(crate) struct GcBox<T: ?Sized> {
    /// Pointer to next value in the GC chain.  
    pub(crate) marked: Cell<bool>,
    pub(crate) next: Option<NonNull<GcBox<dyn Trace + 'static>>>,
    pub(crate) value: T,
}

impl<T> GcBox<T> {
    /// Creates a singleton `GcBox` value (which isn't part of any chain yet).
    #[inline]
    pub fn new(value: T) -> Self {
        Self {
            marked: Cell::new(false),
            next: None,
            value,
        }
    }
}

impl<T: ?Sized> GcBox<T> {
    /// Clears the `mark` bit for this GC object.
    #[inline]
    pub fn clear_mark(&mut self) {
        self.marked.set(false);
    }

    /// Sets the `mark` bit for this GC object.
    #[inline]
    pub fn mark(&mut self) {
        self.marked.set(true);
    }

    /// Returns whether the `mark` bit is set for this GC object.
    #[inline]
    pub fn is_marked(&self) -> bool {
        self.marked.get()
    }
}

impl<T: Default> Default for GcBox<T> {
    #[inline]
    fn default() -> Self {
        Self {
            marked: Cell::new(false),
            next: None,
            value: T::default(),
        }
    }
}

impl<T> Deref for GcBox<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for GcBox<T> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}
