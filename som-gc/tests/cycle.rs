use std::cell::RefCell;

use som_gc::{Gc, GcHeap, Trace};

struct Node {
    id: usize,
    edge: Option<Gc<RefCell<Node>>>,
}

impl Trace for Node {
    fn trace(&self) {
        if let Some(edge) = self.edge.as_ref() {
            edge.trace();
        }
    }
}

impl Drop for Node {
    fn drop(&mut self) {
        println!("dropped node {}", self.id);
    }
}

fn main() {
    let mut heap = GcHeap::new();

    let a = heap.allocate(RefCell::new(Node { id: 1, edge: None }));
    let b = heap.allocate(RefCell::new(Node { id: 2, edge: None }));

    a.borrow_mut().edge = Some(b.clone());
    b.borrow_mut().edge = Some(a.clone());

    drop(b);

    println!("collection 1: START");
    heap.collect_garbage(|| a.trace());
    println!("collection 1: END");

    drop(a);

    println!("collection 2: START");
    heap.collect_garbage(|| {});
    println!("collection 2: END");
}
