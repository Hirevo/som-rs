use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::class::Class;
use crate::instance::Instance;
use crate::universe::Universe;
use crate::SOMRef;

#[derive(Debug, Clone)]
pub enum Value {
    /// A nil value.
    Nil,
    /// A boolean value.
    Boolean(bool),
    /// An integer value.
    Integer(i64),
    /// An integer value.
    Double(f64),
    /// An symbol value (most likely an interned string ?).
    Symbol(Rc<String>),
    /// An string value.
    String(Rc<String>),
    /// An array of values.
    Array(Vec<Value>),
    /// A class instance object (most likely user-defined).
    Instance(SOMRef<Instance>),
    /// A class object (most likely user-defined).
    Class(SOMRef<Class>),
}

impl Value {
    pub fn class(&self, universe: &Universe) -> SOMRef<Class> {
        match self {
            Value::Nil => universe.nil_class(),
            Value::Boolean(true) => universe.true_class(),
            Value::Boolean(false) => universe.false_class(),
            Value::Integer(_) => universe.integer_class(),
            Value::Double(_) => universe.double_class(),
            Value::Symbol(_) => universe.symbol_class(),
            Value::String(_) => universe.string_class(),
            Value::Array(_) => universe.array_class(),
            Value::Instance(instance) => instance.borrow().class().clone(),
            Value::Class(class) => universe.metaclass_class(),
        }
    }

    // /// A method call (aka. receiving a message).
    // ///
    // /// This variant does not look within superclasses.
    // pub fn call(
    //     &mut self,
    //     universe: &mut Universe,
    //     signature: InternedString,
    //     args: Vec<Value>,
    // ) -> Option<Value> {
    //     todo!()
    // }

    // /// A method call (aka. receiving a message).
    // ///
    // /// This variant will recurse through all of the superclasses.
    // pub fn super_call(
    //     &mut self,
    //     universe: &mut Universe,
    //     signature: InternedString,
    //     args: Vec<Value>,
    // ) -> Option<Value> {
    //     todo!()
    // }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Boolean(value) => write!(f, "{}", value),
            Value::Integer(value) => write!(f, "{}", value),
            Value::Double(value) => write!(f, "{}", value),
            Value::Symbol(value) => write!(f, "{}", value),
            Value::String(value) => write!(f, "{}", value),
            Value::Array(values) => {
                write!(f, "[")?;
                for (idx, value) in values.iter().enumerate() {
                    if idx == 0 {
                        write!(f, "{}", value)?;
                    } else {
                        write!(f, ", {}", value)?;
                    }
                }
                write!(f, "]")
            }
            Value::Instance(instance) => write!(
                f,
                "instance of {} class",
                instance.borrow().class().borrow().name()
            ),
            Value::Class(class) => write!(f, "{} class", class.borrow().name()),
        }
    }
}
