use std::fmt::{self, Debug};
use std::path::Path;
use std::thread::{current, ThreadId};

use measureme::{DetachedTiming, EventId, Profiler as MeasuremeProfiler, TimingGuard};
use once_cell::sync::OnceCell;

/// Represents the SOM profiler.
pub struct Profiler {
    profiler: MeasuremeProfiler,
}

static mut INSTANCE: OnceCell<Profiler> = OnceCell::new();

impl Profiler {
    /// Start a new profiled event.
    pub fn start_event(&self, label: &str, category: &str) -> TimingGuard<'_> {
        let kind = self.profiler.alloc_string(category);
        let id = EventId::from_label(self.profiler.alloc_string(label));
        let thread_id = Self::thread_id_to_u32(current().id());
        self.profiler
            .start_recording_interval_event(kind, id, thread_id)
    }

    /// Start a new detached profiled event.
    pub fn start_detached_event(&self, label: &str, category: &str) -> DetachedTiming {
        let kind = self.profiler.alloc_string(category);
        let id = EventId::from_label(self.profiler.alloc_string(label));
        let thread_id = Self::thread_id_to_u32(current().id());
        self.profiler
            .start_recording_interval_event_detached(kind, id, thread_id)
    }

    /// Finish a detached profiled event.
    pub fn finish_detached_event(&self, timing: DetachedTiming) {
        self.profiler.finish_recording_interval_event(timing)
    }

    fn default() -> Self {
        let profiler = MeasuremeProfiler::new(Path::new("./som-trace"))
            .expect("could not create profiler file");
        Self { profiler }
    }

    /// Return the global instance of the profiler.
    pub fn global() -> &'static Self {
        unsafe { INSTANCE.get_or_init(Self::default) }
    }

    /// Drop the global instance of the profiler.
    ///
    /// # Panics
    ///
    /// Calling `drop` will panic if `INSTANCE` cannot be taken back.
    pub fn drop(&self) {
        // In order to drop the INSTANCE we need to get ownership of it, which isn't possible on a static unless you make it a mutable static.
        // Mutating statics is unsafe, so we need to wrap it as so.
        // This is actually safe though because init and drop are only called at the beginning and end of the application.
        unsafe {
            INSTANCE
                .take()
                .expect("could not take back profiler instance");
        }
    }

    // Sadly we need to use the unsafe method until this is resolved:
    // https://github.com/rust-lang/rust/issues/67939
    // Once `as_64()` is in stable we can do this:
    // https://github.com/rust-lang/rust/pull/68531/commits/ea42b1c5b85f649728e3a3b334489bac6dce890a
    // Until then our options are: use nightly Rust or use `unsafe`
    #[allow(clippy::cast_possible_truncation)]
    fn thread_id_to_u32(tid: ThreadId) -> u32 {
        unsafe { std::mem::transmute::<ThreadId, u64>(tid) as u32 }
    }
}

impl Debug for Profiler {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt("no debug implemented", f)
    }
}
