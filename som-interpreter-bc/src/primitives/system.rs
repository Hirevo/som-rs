use std::cell::RefCell;
use std::convert::TryInto;
use std::fs;
use std::io::Write;

use anyhow::Error;
use num_bigint::BigInt;
use once_cell::sync::Lazy;

use som_gc::{GcHeap, Trace};

use crate::frame::FrameKind;
use crate::interner::Interned;
use crate::interpreter::Interpreter;
use crate::primitives::{Primitive, PrimitiveFn, StringLike};
use crate::universe::Universe;
use crate::value::SOMValue;

pub static INSTANCE_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> = Lazy::new(|| {
    Box::new([
        ("loadFile:", self::load_file.into_func(), true),
        ("printString:", self::print_string.into_func(), true),
        ("printNewline", self::print_newline.into_func(), true),
        ("errorPrint:", self::error_print.into_func(), true),
        ("errorPrintln:", self::error_println.into_func(), true),
        ("load:", self::load.into_func(), true),
        ("ticks", self::ticks.into_func(), true),
        ("time", self::time.into_func(), true),
        ("fullGC", self::full_gc.into_func(), true),
        ("gcStats", self::gc_stats.into_func(), true),
        ("exit:", self::exit.into_func(), true),
        ("global:", self::global.into_func(), true),
        ("global:put:", self::global_put.into_func(), true),
        ("hasGlobal:", self::has_global.into_func(), true),
        ("printStackTrace", self::print_stack_trace.into_func(), true),
    ])
});
pub static CLASS_PRIMITIVES: Lazy<Box<[(&str, &'static PrimitiveFn, bool)]>> =
    Lazy::new(|| Box::new([]));

fn load_file(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    universe: &mut Universe,
    _: SOMValue,
    path: StringLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "System>>#loadFie:";

    let path = match path {
        StringLike::String(ref string) => string,
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    let value = match fs::read_to_string(path) {
        Ok(value) => SOMValue::new_string(&heap.allocate(value)),
        Err(_) => SOMValue::NIL,
    };

    interpreter.stack.push(value);

    Ok(())
}

fn print_string(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    universe: &mut Universe,
    _: SOMValue,
    string: StringLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "System>>#printString:";

    let string = match string {
        StringLike::String(ref string) => string,
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    print!("{string}");
    std::io::stdout().flush()?;
    interpreter.stack.push(SOMValue::SYSTEM);

    Ok(())
}

fn print_newline(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    _: SOMValue,
) -> Result<(), Error> {
    const SIGNATURE: &'static str = "System>>#printNewline";

    println!();
    interpreter.stack.push(SOMValue::NIL);

    Ok(())
}

fn error_print(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    universe: &mut Universe,
    _: SOMValue,
    string: StringLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "System>>#errorPrint:";

    let string = match string {
        StringLike::String(ref string) => string,
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    eprint!("{string}");
    std::io::stderr().flush()?;
    interpreter.stack.push(SOMValue::SYSTEM);

    Ok(())
}

fn error_println(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    universe: &mut Universe,
    _: SOMValue,
    string: StringLike,
) -> Result<(), Error> {
    const SIGNATURE: &str = "System>>#errorPrintln:";

    let string = match string {
        StringLike::String(ref string) => string,
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    eprintln!("{string}");
    interpreter.stack.push(SOMValue::SYSTEM);

    Ok(())
}

fn load(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    universe: &mut Universe,
    _: SOMValue,
    class_name: Interned,
) -> Result<(), Error> {
    const SIGNATURE: &str = "System>>#load:";

    let class_name = universe.lookup_symbol(class_name).to_string();
    let class = universe.load_class(heap, class_name)?;
    interpreter.stack.push(SOMValue::new_class(&class));

    Ok(())
}

fn has_global(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    universe: &mut Universe,
    _: SOMValue,
    name: Interned,
) -> Result<(), Error> {
    const SIGNATURE: &str = "System>>#hasGlobal:";

    let value = SOMValue::new_boolean(universe.has_global(name));
    interpreter.stack.push(value);

    Ok(())
}

fn global(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    universe: &mut Universe,
    _: SOMValue,
    name: Interned,
) -> Result<(), Error> {
    const SIGNATURE: &str = "System>>#global:";

    let value = universe.lookup_global(name).unwrap_or(SOMValue::NIL);
    interpreter.stack.push(value);

    Ok(())
}

fn global_put(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    universe: &mut Universe,
    _: SOMValue,
    name: Interned,
    value: SOMValue,
) -> Result<(), Error> {
    const SIGNATURE: &str = "System>>#global:put:";

    universe.assign_global(name, value);
    interpreter.stack.push(value);

    Ok(())
}

fn exit(_: &mut Interpreter, _: &mut GcHeap, _: &mut Universe, status: i32) -> Result<(), Error> {
    const SIGNATURE: &str = "System>>#exit:";

    std::process::exit(status);
}

fn ticks(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    _: SOMValue,
) -> Result<(), Error> {
    const SIGNATURE: &str = "System>>#ticks";

    let micros = interpreter.start_time.elapsed().as_micros().try_into()?;
    interpreter.stack.push(SOMValue::new_integer(micros));

    Ok(())
}

fn time(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    _: SOMValue,
) -> Result<(), Error> {
    const SIGNATURE: &str = "System>>#time";

    let micros = interpreter.start_time.elapsed().as_millis().try_into()?;
    interpreter.stack.push(SOMValue::new_integer(micros));

    Ok(())
}

fn print_stack_trace(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    _: SOMValue,
) -> Result<(), Error> {
    const SIGNATURE: &str = "System>>#printStackTrace";

    for frame in &interpreter.frames {
        let frame_ref = frame.borrow();
        let class = frame_ref.get_method_holder();
        let method = frame_ref.get_method();
        let bytecode_idx = frame_ref.bytecode_idx;
        let block = match frame_ref.kind() {
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

    Ok(())
}

fn full_gc(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    universe: &mut Universe,
    _: SOMValue,
) -> Result<(), Error> {
    const SIGNATURE: &str = "System>>#fullGC";

    heap.collect_garbage(|| {
        interpreter.trace();
        universe.trace();
    });

    interpreter.stack.push(SOMValue::TRUE);

    Ok(())
}

fn gc_stats(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    _: &mut Universe,
    _: SOMValue,
) -> Result<(), Error> {
    const SIGNATURE: &str = "System>>#gcStats";

    let stats = heap.stats().clone();
    let collections_performed = match stats.collections_performed.try_into() {
        Ok(value) => SOMValue::new_integer(value),
        Err(_) => {
            let allocated = heap.allocate(BigInt::from(stats.collections_performed));
            SOMValue::new_big_integer(&allocated)
        }
    };
    let bytes_allocated = match stats.bytes_allocated.try_into() {
        Ok(value) => SOMValue::new_integer(value),
        Err(_) => {
            let allocated = heap.allocate(BigInt::from(stats.bytes_allocated));
            SOMValue::new_big_integer(&allocated)
        }
    };
    let total_time_spent = match stats.total_time_spent.as_millis().try_into() {
        Ok(value) => SOMValue::new_integer(value),
        Err(_) => {
            let allocated = heap.allocate(BigInt::from(stats.total_time_spent.as_millis()));
            SOMValue::new_big_integer(&allocated)
        }
    };
    let output = heap.allocate(RefCell::new(vec![
        collections_performed,
        total_time_spent,
        bytes_allocated,
    ]));
    interpreter.stack.push(SOMValue::new_array(&output));

    Ok(())
}

/// Search for an instance primitive matching the given signature.
pub fn get_instance_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    INSTANCE_PRIMITIVES
        .iter()
        .find(|it| it.0 == signature)
        .map(|it| it.1)
}

/// Search for a class primitive matching the given signature.
pub fn get_class_primitive(signature: &str) -> Option<&'static PrimitiveFn> {
    CLASS_PRIMITIVES
        .iter()
        .find(|it| it.0 == signature)
        .map(|it| it.1)
}
