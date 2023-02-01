mod gc;
mod gc_box;
mod heap;
mod trace;

pub use crate::gc::Gc;
pub use crate::heap::GcHeap;
pub use crate::trace::Trace;