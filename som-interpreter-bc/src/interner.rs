//!
//! This is an implementation of a string interner.
//!
//! It allows to bring down the memory usage from strings and allows for fast comparisons by replacing the strings by essentially an ID.
//!
//! This particular implementation comes from [matklad's "Fast and Simple Rust Interner" blog post](https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html).
//!

use std::collections::HashMap;
use std::mem;

/// An interned string.
///
/// This is fast to move, clone and compare.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct Interned(pub u32);

/// A string interner.
///
/// This particular implementation comes from [matklad's "Fast and Simple Rust Interner" blog post](https://matklad.github.io/2020/03/22/fast-simple-rust-interner.html).
#[derive(Debug)]
pub struct Interner {
    map: HashMap<&'static str, u32>,
    vec: Vec<&'static str>,
    buf: String,
    full: Vec<String>,
}

impl Interner {
    /// Initialize the interner with an initial capacity.
    pub fn with_capacity(cap: usize) -> Self {
        let cap = cap.next_power_of_two();
        Self {
            map: HashMap::default(),
            vec: Vec::new(),
            buf: String::with_capacity(cap),
            full: Vec::new(),
        }
    }

    /// Intern a given string.
    pub fn intern(&mut self, name: &str) -> Interned {
        if let Some(&id) = self.map.get(name) {
            return Interned(id);
        }
        let name = unsafe { self.alloc(name) };
        let id = self.map.len() as u32;
        self.map.insert(name, id);
        self.vec.push(name);

        let id = Interned(id);

        debug_assert!(self.lookup(id) == name);
        debug_assert!(self.intern(name) == id);

        id
    }

    /// Get the string associated to a given interning ID.
    pub fn lookup(&self, id: Interned) -> &str {
        self.vec[id.0 as usize]
    }

    unsafe fn alloc(&mut self, name: &str) -> &'static str {
        let cap = self.buf.capacity();
        if cap < self.buf.len() + name.len() {
            let new_cap = (cap.max(name.len()) + 1).next_power_of_two();
            let new_buf = String::with_capacity(new_cap);
            let old_buf = mem::replace(&mut self.buf, new_buf);
            self.full.push(old_buf);
        }

        let interned = {
            let start = self.buf.len();
            self.buf.push_str(name);
            &self.buf[start..]
        };

        &*(interned as *const str)
    }
}
