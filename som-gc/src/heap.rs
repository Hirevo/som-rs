use std::cell::Cell;
use std::marker::PhantomData;
use std::ptr::NonNull;

use crate::gc_box::GcBox;

use crate::gc::Gc;
use crate::trace::Trace;

/// The GC heap itself, which is the storage for all GC-ed objects.
pub struct GcHeap {
    head: Option<NonNull<GcBox<dyn Trace + 'static>>>,
}

impl Drop for GcHeap {
    // We properly drop all objects in the heap.
    fn drop(&mut self) {
        let mut head = self.head;
        while let Some(cur) = head {
            // SAFETY: we don't access that reference again after we drop it.
            head = unsafe { cur.as_ref() }.next;
            drop(unsafe { Box::from_raw(cur.as_ptr()) });
        }
    }
}

impl GcHeap {
    /// Creates a new empty GC heap.
    pub fn new() -> Self {
        Self {
            head: None,
        }
    }

    /// Allocates an object on the GC heap, returning its handle.
    pub fn allocate<T: Trace + 'static>(&mut self, value: T) -> Gc<T> {
        // TODO: trigger `collect_garbage`
        let mut allocated = Box::new(GcBox::new(value));
        allocated.next = self.head;
        let ptr = unsafe { NonNull::new_unchecked(Box::into_raw(allocated)) };
        self.head = Some(ptr as NonNull<GcBox<dyn Trace + 'static>>);
        Gc {
            // SAFETY: `self.head` is guaranteed to be properly aligned and non-null by `Box::into_raw`.
            ptr: Cell::new(ptr),
            marker: PhantomData,
        }
    }

    /// Clears the `mark` bits on every GC object.
    fn clear_marks(&mut self) {
        let mut head = self.head;
        while let Some(mut cur) = head {
            let cur = unsafe { cur.as_mut() };
            cur.clear_mark();
            head = cur.next;
        }
    }

    /// Performs a sweep on the GC heap (drops all unmarked objects).
    fn sweep(&mut self) {
        let mut head = self.head;
        let mut prev = None::<NonNull<GcBox<dyn Trace + 'static>>>;
        while let Some(cur) = head {
            // SAFETY: we don't access that reference again after we drop it.
            let cur_ref = unsafe { cur.as_ref() };
            let next = cur_ref.next;
            if !cur_ref.is_marked() {
                if let Some(mut prev_cur) = prev {
                    unsafe { prev_cur.as_mut() }.next = next;
                } else {
                    self.head = next;
                }
                // TODO: introduce a `Finalize`-like mechanism.
                // TODO: maybe perform the drops in a separate thread.
                drop(unsafe { Box::from_raw(cur.as_ptr()) });
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
