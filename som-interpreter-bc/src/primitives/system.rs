use std::cell::RefCell;
use std::convert::{TryFrom, TryInto};
use std::fs;

use num_bigint::{BigInt, ToBigInt};

use som_gc::{GcHeap, Trace};

use crate::frame::FrameKind;
use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::{SOMValue, Value};
use crate::{expect_args, reverse};

pub static INSTANCE_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[
    ("loadFile:", self::load_file, true),
    ("printString:", self::print_string, true),
    ("printNewline", self::print_newline, true),
    ("errorPrint:", self::error_print, true),
    ("errorPrintln:", self::error_println, true),
    ("load:", self::load, true),
    ("ticks", self::ticks, true),
    ("time", self::time, true),
    ("fullGC", self::full_gc, true),
    ("gcStats", self::gc_stats, true),
    ("exit:", self::exit, true),
    ("global:", self::global, true),
    ("global:put:", self::global_put, true),
    ("hasGlobal:", self::has_global, true),
    ("printStackTrace", self::print_stack_trace, true),
];
pub static CLASS_PRIMITIVES: &[(&str, PrimitiveFn, bool)] = &[];

fn load_file(interpreter: &mut Interpreter, heap: &mut GcHeap, universe: &mut Universe) {
    const SIGNATURE: &str = "System>>#loadFie:";

    expect_args!(SIGNATURE, interpreter, [
        Value::System,
        value => value,
    ]);

    let path = match value {
        Value::String(ref string) => string,
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => panic!("'{}': wrong type", SIGNATURE),
    };

    let value = match fs::read_to_string(path) {
        Ok(value) => SOMValue::new_string(&heap.allocate(value)),
        Err(_) => SOMValue::NIL,
    };

    interpreter.stack.push(value);
}

fn print_string(interpreter: &mut Interpreter, _: &mut GcHeap, universe: &mut Universe) {
    const SIGNATURE: &str = "System>>#printString:";

    expect_args!(SIGNATURE, interpreter, [
        Value::System,
        value => value,
    ]);

    let string = match value {
        Value::String(ref string) => string,
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => panic!("'{}': wrong type", SIGNATURE),
    };

    print!("{}", string);
    use std::io::Write;
    std::io::stdout().flush().unwrap();
    interpreter.stack.push(SOMValue::SYSTEM)
}

fn print_newline(interpreter: &mut Interpreter, _: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &'static str = "System>>#printNewline";

    expect_args!(SIGNATURE, interpreter, [Value::System]);

    println!();
    interpreter.stack.push(SOMValue::NIL);
}

fn error_print(interpreter: &mut Interpreter, _: &mut GcHeap, universe: &mut Universe) {
    const SIGNATURE: &str = "System>>#errorPrint:";

    expect_args!(SIGNATURE, interpreter, [
        Value::System,
        value => value,
    ]);

    let string = match value {
        Value::String(ref string) => string,
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => panic!("'{}': wrong type", SIGNATURE),
    };

    eprint!("{}", string);
    interpreter.stack.push(SOMValue::SYSTEM);
}

fn error_println(interpreter: &mut Interpreter, _: &mut GcHeap, universe: &mut Universe) {
    const SIGNATURE: &str = "System>>#errorPrintln:";

    expect_args!(SIGNATURE, interpreter, [
        Value::System,
        value => value,
    ]);

    let string = match value {
        Value::String(ref string) => string,
        Value::Symbol(sym) => universe.lookup_symbol(sym),
        _ => panic!("'{}': wrong type", SIGNATURE),
    };

    eprintln!("{}", string);
    interpreter.stack.push(SOMValue::SYSTEM);
}

fn load(interpreter: &mut Interpreter, heap: &mut GcHeap, universe: &mut Universe) {
    const SIGNATURE: &str = "System>>#load:";

    expect_args!(SIGNATURE, interpreter, [
        Value::System,
        Value::Symbol(sym) => sym,
    ]);

    let name = universe.lookup_symbol(sym).to_string();
    match universe.load_class(heap, name) {
        Ok(class) => interpreter.stack.push(SOMValue::new_class(&class)),
        Err(err) => panic!("'{}': {}", SIGNATURE, err),
    }
}

fn has_global(interpreter: &mut Interpreter, _: &mut GcHeap, universe: &mut Universe) {
    const SIGNATURE: &str = "System>>#hasGlobal:";

    expect_args!(SIGNATURE, interpreter, [
        Value::System,
        Value::Symbol(sym) => sym,
    ]);

    let value = SOMValue::new_boolean(universe.has_global(sym));

    interpreter.stack.push(value);
}

fn global(interpreter: &mut Interpreter, _: &mut GcHeap, universe: &mut Universe) {
    const SIGNATURE: &str = "System>>#global:";

    expect_args!(SIGNATURE, interpreter, [
        Value::System,
        Value::Symbol(sym) => sym,
    ]);

    let value = universe.lookup_global(sym).unwrap_or(SOMValue::NIL);

    interpreter.stack.push(value);
}

fn global_put(interpreter: &mut Interpreter, _: &mut GcHeap, universe: &mut Universe) {
    const SIGNATURE: &str = "System>>#global:put:";

    expect_args!(SIGNATURE, interpreter, [
        Value::System,
        Value::Symbol(sym) => sym,
        value => value,
    ]);

    let value = value.into();
    universe.assign_global(sym, value);
    interpreter.stack.push(value);
}

fn exit(interpreter: &mut Interpreter, heap: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "System>>#exit:";

    expect_args!(SIGNATURE, interpreter, [
        Value::System,
        Value::Integer(code) => code,
    ]);

    match i32::try_from(code) {
        Ok(code) => {
            heap.collect_garbage(|| {});

            let stats = heap.stats();
            let params = heap.params();

            println!();
            println!("total GC runs: {}", stats.collections_performed);
            println!("total bytes swept: {}", stats.bytes_swept);
            println!("total bytes still allocated: {}", stats.bytes_allocated);
            println!("total GC time: {:?}", stats.total_time_spent);
            println!("final GC threshold: {}", params.threshold);
            println!();
            std::process::exit(code)
        }
        Err(err) => panic!("'{}': {}", SIGNATURE, err),
    }
}

fn ticks(interpreter: &mut Interpreter, _: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "System>>#ticks";

    expect_args!(SIGNATURE, interpreter, [Value::System]);

    match i32::try_from(interpreter.start_time.elapsed().as_micros()) {
        Ok(micros) => interpreter.stack.push(SOMValue::new_integer(micros)),
        Err(err) => panic!("'{}': {}", SIGNATURE, err),
    }
}

fn time(interpreter: &mut Interpreter, _: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "System>>#time";

    expect_args!(SIGNATURE, interpreter, [Value::System]);

    match i32::try_from(interpreter.start_time.elapsed().as_millis()) {
        Ok(micros) => interpreter.stack.push(SOMValue::new_integer(micros)),
        Err(err) => panic!("'{}': {}", SIGNATURE, err),
    }
}

fn print_stack_trace(interpreter: &mut Interpreter, _: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "System>>#printStackTrace";

    expect_args!(SIGNATURE, interpreter, [Value::System]);

    for frame in &interpreter.frames {
        let class = frame.borrow().get_method_holder();
        let method = frame.borrow().get_method();
        let bytecode_idx = frame.borrow().bytecode_idx;
        let block = match frame.borrow().kind() {
            FrameKind::Block { .. } => "$block",
            _ => "",
        };
        println!(
            "{}>>#{}{} @bi: {}",
            class.borrow().name(),
            method.signature(),
            block,
            bytecode_idx
        );
    }

    interpreter.stack.push(SOMValue::TRUE);
}

fn full_gc(interpreter: &mut Interpreter, heap: &mut GcHeap, universe: &mut Universe) {
    const SIGNATURE: &str = "System>>#fullGC";

    expect_args!(SIGNATURE, interpreter, [Value::System]);

    heap.collect_garbage(|| {
        interpreter.trace();
        universe.trace();
    });

    interpreter.stack.push(SOMValue::TRUE);
}

fn gc_stats(interpreter: &mut Interpreter, heap: &mut GcHeap, _: &mut Universe) {
    const SIGNATURE: &str = "System>>#gcStats";

    expect_args!(SIGNATURE, interpreter, [Value::System]);

    let stats = heap.stats().clone();
    let collections_performed = match stats.collections_performed.try_into() {
        Ok(value) => SOMValue::new_integer(value),
        Err(_) => {
            SOMValue::new_big_integer(&heap.allocate(BigInt::from(stats.collections_performed)))
        }
    };
    let bytes_allocated = match stats.bytes_allocated.try_into() {
        Ok(value) => SOMValue::new_integer(value),
        Err(_) => SOMValue::new_big_integer(&heap.allocate(BigInt::from(stats.bytes_allocated))),
    };
    let total_time_spent = SOMValue::new_big_integer(
        &heap.allocate(stats.total_time_spent.as_millis().to_bigint().unwrap()),
    );
    let output = heap.allocate(RefCell::new(vec![
        collections_performed,
        total_time_spent,
        bytes_allocated,
    ]));
    interpreter.stack.push(SOMValue::new_array(&output));
}

/// Search for an instance primitive matching the given signature.
pub fn get_instance_primitive(signature: &str) -> Option<PrimitiveFn> {
    INSTANCE_PRIMITIVES
        .iter()
        .find(|it| it.0 == signature)
        .map(|it| it.1)
}

/// Search for a class primitive matching the given signature.
pub fn get_class_primitive(signature: &str) -> Option<PrimitiveFn> {
    CLASS_PRIMITIVES
        .iter()
        .find(|it| it.0 == signature)
        .map(|it| it.1)
}
