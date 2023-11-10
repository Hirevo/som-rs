use std::cell::Cell;
use std::ptr::NonNull;
use std::time::{Duration, Instant};

use crate::gc_box::GcBox;

use crate::gc::Gc;
use crate::trace::Trace;

#[derive(Debug, Clone, PartialEq)]
pub struct GcStats {
    pub collections_performed: usize,
    pub bytes_allocated: usize,
    pub bytes_swept: usize,
    pub total_time_spent: Duration,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GcParams {
    pub threshold: usize,
    pub used_space_ratio: f64,
}

/// The GC heap itself, which is the storage for all GC-ed objects.
pub struct GcHeap {
    stats: GcStats,
    params: GcParams,
    objects: Vec<NonNull<GcBox<dyn Trace + 'static>>>,
}

impl Default for GcParams {
    fn default() -> Self {
        Self {
            threshold: 10_000_000,
            used_space_ratio: 0.7,
        }
    }
}

impl Drop for GcHeap {
    // We properly drop all objects in the heap.
    fn drop(&mut self) {
        for obj in self.objects.iter() {
            // SAFETY: we don't access that reference again after we drop it.
            drop(unsafe { Box::from_raw(obj.as_ptr()) });
        }
    }
}

impl GcHeap {
    /// Creates a new empty GC heap, with the default parameters.
    pub fn new() -> Self {
        Self::with_params(GcParams::default())
    }

    /// Creates a new empty GC heap, with the specified parameters.
    pub fn with_params(params: GcParams) -> Self {
        Self {
            params,
            stats: GcStats {
                collections_performed: 0,
                bytes_allocated: 0,
                bytes_swept: 0,
                total_time_spent: Duration::ZERO,
            },
            objects: Vec::default(),
        }
    }

    /// Returns a reference to the GC's stats.
    pub fn stats(&self) -> &GcStats {
        &self.stats
    }

    /// Returns a reference to the GC's parameters.
    pub fn params(&self) -> &GcParams {
        &self.params
    }

    /// Allocates an object on the GC heap, returning its handle.
    pub fn allocate<T: Trace + 'static>(&mut self, value: T) -> Gc<T> {
        // TODO: trigger `collect_garbage`
        let allocated = Box::new(GcBox::new(value));
        // SAFETY: `self.head` is guaranteed to be properly aligned and non-null by `Box::into_raw`.
        let ptr = unsafe { NonNull::new_unchecked(Box::into_raw(allocated)) };
        self.objects
            .push(ptr as NonNull<GcBox<dyn Trace + 'static>>);
        self.stats.bytes_allocated += std::mem::size_of::<GcBox<T>>();
        Gc {
            ptr: Cell::new(ptr),
        }
    }

    /// Clears the `mark` bits on every GC object.
    #[allow(unused)]
    fn clear_marks(&mut self) {
        for obj in self.objects.iter_mut() {
            let obj = unsafe { obj.as_mut() };
            obj.clear_mark();
        }
    }

    /// Performs a sweep on the GC heap (drops all unmarked objects).
    fn sweep(&mut self) {
        self.objects.retain_mut(|obj| {
            // SAFETY: we don't access that reference again after we drop it.
            let is_retained = unsafe { obj.as_ref() }.marked.replace(false);
            if !is_retained {
                let value = unsafe { Box::from_raw(obj.as_ptr()) };
                self.stats.bytes_allocated -= std::mem::size_of_val::<GcBox<_>>(&*value);
                drop(value);
            }
            is_retained
        });
    }

    /// Performs garbage collection (mark-and-sweep) on the GC heap.
    pub fn collect_garbage(&mut self, mut mark_fn: impl FnMut()) {
        let start = Instant::now();
        let allocated_start = self.stats.bytes_allocated;
        mark_fn();
        self.sweep();
        self.stats.bytes_swept += allocated_start - self.stats.bytes_allocated;
        self.stats.total_time_spent += start.elapsed();
        self.stats.collections_performed += 1;
    }

    /// Performs garbage collection (mark-and-sweep) on the GC heap, only if necessary.
    pub fn maybe_collect_garbage(&mut self, mark_fn: impl FnMut()) {
        if self.stats.bytes_allocated > self.params.threshold {
            self.collect_garbage(mark_fn);

            if self.stats.bytes_allocated as f64
                > self.params.threshold as f64 * self.params.used_space_ratio
            {
                self.params.threshold =
                    (self.stats.bytes_allocated as f64 / self.params.used_space_ratio) as usize
            }
        }
    }
}
