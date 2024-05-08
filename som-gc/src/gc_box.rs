use std::alloc::Layout;
use std::cell::Cell;
use std::ops::{Deref, DerefMut};

/// Represents a value, as it is stored within the GC.
#[repr(C)]
pub(crate) struct GcBox<T: ?Sized> {
    /// Pointer to next value in the GC chain.  
    pub(crate) layout: Layout,
    pub(crate) marked: Cell<bool>,
    pub(crate) value: T,
}

impl<T> GcBox<T> {
    /// Creates a singleton `GcBox` value (which isn't part of any chain yet).
    #[inline]
    pub fn new(layout: Layout, value: T) -> Self {
        Self {
            marked: Cell::new(false),
            layout,
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
            layout: Layout::new::<T>(),
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
