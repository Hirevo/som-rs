use std::ops::{Deref, DerefMut};

/// Represents a value, as it is stored within the GC.
pub(crate) struct GcBox<T> {
    /// Pointer to next value in the GC chain.  
    ///
    /// We also use the unused bits of this pointer to store the `marked` bitflag.  
    /// Therefore, it not suitable for it to be dereferenced as-is, it must first
    /// be cleared of these flags before any accesses are performed.  
    /// This is also why this is not represented as a `Option<NonNull<GcBox<T>>>`
    pub(crate) next: *mut GcBox<T>,
    pub(crate) value: T,
}

impl<T> GcBox<T> {
    /// Bitflag for whether the current GC object is marked (still referenced).
    pub const MARK_BITMASK: usize = 0b0000_0001;
    /// Bitmask for the bits reserved for GC flags within the `next` pointer in `GcBox`.
    pub const FLAGS_BITMASK: usize = 0b0000_0011;

    /// Creates a singleton `GcBox` value (which isn't part of any chain yet).
    pub fn new(value: T) -> Self {
        Self {
            next: std::ptr::null_mut(),
            value,
        }
    }

    /// Clears the `mark` bit for this GC object.
    pub fn clear_mark(&mut self) {
        self.next = (self.next as usize & !Self::MARK_BITMASK) as *mut _;
    }

    /// Sets the `mark` bit for this GC object.
    pub fn mark(&mut self) {
        self.next = (self.next as usize | Self::MARK_BITMASK) as *mut _;
    }

    /// Returns whether the `mark` bit is set for this GC object.
    pub fn is_marked(&self) -> bool {
        self.next as usize & Self::MARK_BITMASK == 1
    }

    /// Returns the pointer to the next GC object in the chain.
    pub fn next(&self) -> *mut GcBox<T> {
        (self.next as usize & !Self::FLAGS_BITMASK) as *mut _
    }

    /// Sets the pointer to the next GC object in the chain, and preserving all the current bitflags.
    pub fn set_next(&mut self, next: *mut GcBox<T>) {
        self.next = (next as usize | (self.next as usize & Self::MARK_BITMASK)) as *mut _;
    }
}

impl<T: Default> Default for GcBox<T> {
    fn default() -> Self {
        Self {
            next: std::ptr::null_mut(),
            value: T::default(),
        }
    }
}

impl<T> Deref for GcBox<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for GcBox<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}
