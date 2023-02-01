use std::cell::Cell;
use std::marker::PhantomData;
use std::ptr::NonNull;

use crate::gc_box::GcBox;

use crate::gc::Gc;
use crate::trace::Trace;

/// The GC heap itself, which is the storage for all GC-ed objects.
pub struct GcHeap<T>
where
    T: Trace + 'static,
{
    head: *mut GcBox<T>,
}

impl<T> Drop for GcHeap<T>
where
    T: Trace + 'static,
{
    // We properly drop all objects in the heap.
    fn drop(&mut self) {
        let mut head: *mut GcBox<T> = self.head;
        while !head.is_null() {
            // SAFETY: we don't access that reference again after we drop it.
            let next = unsafe { &*head }.next();
            drop(unsafe { Box::from_raw(head) });
            head = next;
        }
    }
}

impl<T> GcHeap<T>
where
    T: Trace + 'static,
{
    /// Creates a new empty GC heap.
    pub fn new() -> Self {
        Self {
            head: std::ptr::null_mut(),
        }
    }

    /// Allocates an object on the GC heap, returning its handle.
    pub fn allocate(&mut self, value: T) -> Gc<T> {
        // TODO: trigger `collect_garbage`
        let mut allocated = Box::new(GcBox::new(value));
        allocated.set_next(self.head);
        self.head = Box::into_raw(allocated);
        Gc {
            // SAFETY: `self.head` is guaranteed to be properly aligned and non-null by `Box::into_raw`.
            ptr: Cell::new(unsafe { NonNull::new_unchecked(self.head) }),
            marker: PhantomData,
        }
    }

    /// Clears the `mark` bits on every GC object.
    fn clear_marks(&mut self) {
        let mut head = self.head;
        while !head.is_null() {
            let head_ref = unsafe { &mut *head };
            head_ref.clear_mark();
            head = head_ref.next();
        }
    }

    /// Performs a sweep on the GC heap (drops all unmarked objects).
    fn sweep(&mut self) {
        let mut head: *mut GcBox<T> = self.head;
        let mut prev: *mut GcBox<T> = std::ptr::null_mut();
        while !head.is_null() {
            // SAFETY: we don't access that reference again after we drop it.
            let head_ref = unsafe { &*head };
            let next = head_ref.next();
            if !head_ref.is_marked() {
                if !prev.is_null() {
                    unsafe { &mut *prev }.set_next(next);
                } else {
                    self.head = next;
                }
                // TODO: introduce a `Finalize`-like mechanism.
                // TODO: maybe perform the drops in a separate thread.
                drop(unsafe { Box::from_raw(head) });
            } else {
                prev = head;
            }
            head = next;
        }
    }

    /// Performs garbage collection (mark-and-sweep) on the GC heap.
    pub fn collect_garbage(&mut self, mut mark_fn: impl FnMut()) {
        self.clear_marks();
        mark_fn();
        self.sweep();
    }
}
