use som_gc::{GcHeap, Trace};

struct Test {
    value: usize,
}

impl Test {
    pub fn new(value: usize) -> Self {
        Self { value }
    }

    pub fn say_hello(&self) {
        println!("hello from {}", self.value);
    }
}

impl Drop for Test {
    fn drop(&mut self) {
        println!("dropped value {}", self.value);
    }
}

impl Trace for Test {
    fn trace(&self) {}
}

fn main() {
    let mut heap = GcHeap::new();

    let mut stack = Vec::new();

    stack.push(heap.allocate(Test::new(3)));
    stack.push(heap.allocate(Test::new(4)));
    stack.push(heap.allocate(Test::new(5)));

    stack.iter().for_each(|it| it.say_hello());

    println!("collection 1: START");
    heap.collect_garbage(|| stack.trace());
    println!("collection 1: END");

    stack.remove(1);

    println!("collection 2: START");
    heap.collect_garbage(|| stack.trace());
    println!("collection 2: END");
}
