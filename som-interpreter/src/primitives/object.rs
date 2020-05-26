use crate::universe::Universe;
use crate::value::Value;

fn println(universe: &mut Universe, args: Vec<Value>) -> Value {
    let arg = args
        .into_iter()
        .next()
        .expect("no arguments to 'Object>>#println:'");
    print!("{:?}", arg);
    arg
}
