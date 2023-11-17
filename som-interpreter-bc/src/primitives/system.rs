use std::convert::TryInto;
use std::fs;
use std::io::Write;

use anyhow::{Context, Error};
use num_bigint::BigInt;
use once_cell::sync::Lazy;

use som_gc::{Gc, GcHeap, Trace};

use crate::class::Class;
use crate::convert::{IntegerLike, Nil, Primitive, StringLike, System};
use crate::frame::FrameKind;
use crate::interner::Interned;
use crate::interpreter::Interpreter;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::value::SOMValue;
use crate::SOMRef;

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
    _: &mut Interpreter,
    heap: &mut GcHeap,
    universe: &mut Universe,
    _: SOMValue,
    path: StringLike,
) -> Result<Option<Gc<String>>, Error> {
    const _: &str = "System>>#loadFie:";

    let path = match path {
        StringLike::String(ref string) => string,
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    let Ok(value) = fs::read_to_string(path) else {
        return Ok(None);
    };

    Ok(Some(heap.allocate(value)))
}

fn print_string(
    _: &mut Interpreter,
    _: &mut GcHeap,
    universe: &mut Universe,
    _: SOMValue,
    string: StringLike,
) -> Result<System, Error> {
    const _: &str = "System>>#printString:";

    let string = match string {
        StringLike::String(ref string) => string,
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    print!("{string}");
    std::io::stdout().flush()?;

    Ok(System)
}

fn print_newline(
    _: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    _: SOMValue,
) -> Result<Nil, Error> {
    const _: &'static str = "System>>#printNewline";

    println!();

    Ok(Nil)
}

fn error_print(
    _: &mut Interpreter,
    _: &mut GcHeap,
    universe: &mut Universe,
    _: SOMValue,
    string: StringLike,
) -> Result<System, Error> {
    const _: &str = "System>>#errorPrint:";

    let string = match string {
        StringLike::String(ref string) => string,
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    eprint!("{string}");
    std::io::stderr().flush()?;

    Ok(System)
}

fn error_println(
    _: &mut Interpreter,
    _: &mut GcHeap,
    universe: &mut Universe,
    _: SOMValue,
    string: StringLike,
) -> Result<System, Error> {
    const _: &str = "System>>#errorPrintln:";

    let string = match string {
        StringLike::String(ref string) => string,
        StringLike::Symbol(sym) => universe.lookup_symbol(sym),
    };

    eprintln!("{string}");

    Ok(System)
}

fn load(
    _: &mut Interpreter,
    heap: &mut GcHeap,
    universe: &mut Universe,
    _: SOMValue,
    class_name: Interned,
) -> Result<SOMRef<Class>, Error> {
    const _: &str = "System>>#load:";

    let class_name = universe.lookup_symbol(class_name).to_string();
    let class = universe.load_class(heap, class_name)?;

    Ok(class)
}

fn has_global(
    _: &mut Interpreter,
    _: &mut GcHeap,
    universe: &mut Universe,
    _: SOMValue,
    name: Interned,
) -> Result<bool, Error> {
    const _: &str = "System>>#hasGlobal:";

    Ok(universe.has_global(name))
}

fn global(
    _: &mut Interpreter,
    _: &mut GcHeap,
    universe: &mut Universe,
    _: SOMValue,
    name: Interned,
) -> Result<Option<SOMValue>, Error> {
    const _: &str = "System>>#global:";

    Ok(universe.lookup_global(name))
}

fn global_put(
    _: &mut Interpreter,
    _: &mut GcHeap,
    universe: &mut Universe,
    _: SOMValue,
    name: Interned,
    value: SOMValue,
) -> Result<Option<SOMValue>, Error> {
    const _: &str = "System>>#global:put:";

    Ok(universe.assign_global(name, value).map(|_| value))
}

fn exit(_: &mut Interpreter, _: &mut GcHeap, _: &mut Universe, status: i32) -> Result<(), Error> {
    const _: &str = "System>>#exit:";

    std::process::exit(status);
}

fn ticks(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    _: SOMValue,
) -> Result<i32, Error> {
    const SIGNATURE: &str = "System>>#ticks";

    interpreter
        .start_time
        .elapsed()
        .as_micros()
        .try_into()
        .with_context(|| format!("`{SIGNATURE}`: could not convert `i128` to `i32`"))
}

fn time(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    _: SOMValue,
) -> Result<i32, Error> {
    const SIGNATURE: &str = "System>>#time";

    interpreter
        .start_time
        .elapsed()
        .as_millis()
        .try_into()
        .with_context(|| format!("`{SIGNATURE}`: could not convert `i128` to `i32`"))
}

fn print_stack_trace(
    interpreter: &mut Interpreter,
    _: &mut GcHeap,
    _: &mut Universe,
    _: SOMValue,
) -> Result<bool, Error> {
    const _: &str = "System>>#printStackTrace";

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

    Ok(true)
}

fn full_gc(
    interpreter: &mut Interpreter,
    heap: &mut GcHeap,
    universe: &mut Universe,
    _: SOMValue,
) -> Result<bool, Error> {
    const _: &str = "System>>#fullGC";

    heap.collect_garbage(|| {
        interpreter.trace();
        universe.trace();
    });

    Ok(true)
}

fn gc_stats(
    _: &mut Interpreter,
    heap: &mut GcHeap,
    _: &mut Universe,
    _: SOMValue,
) -> Result<(IntegerLike, IntegerLike, IntegerLike), Error> {
    const _: &str = "System>>#gcStats";

    let stats = heap.stats().clone();
    let collections_performed = match stats.collections_performed.try_into() {
        Ok(value) => IntegerLike::Integer(value),
        Err(_) => {
            let allocated = heap.allocate(BigInt::from(stats.collections_performed));
            IntegerLike::BigInteger(allocated)
        }
    };
    let bytes_allocated = match stats.bytes_allocated.try_into() {
        Ok(value) => IntegerLike::Integer(value),
        Err(_) => {
            let allocated = heap.allocate(BigInt::from(stats.bytes_allocated));
            IntegerLike::BigInteger(allocated)
        }
    };
    let total_time_spent = match stats.total_time_spent.as_millis().try_into() {
        Ok(value) => IntegerLike::Integer(value),
        Err(_) => {
            let allocated = heap.allocate(BigInt::from(stats.total_time_spent.as_millis()));
            IntegerLike::BigInteger(allocated)
        }
    };

    Ok((collections_performed, total_time_spent, bytes_allocated))
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
