use std::fmt;
use std::rc::Rc;

use crate::block::Block;
use crate::class::Class;
use crate::instance::Instance;
use crate::interner::Interned;
use crate::method::Method;
use crate::universe::Universe;
use crate::SOMRef;

/// Represents an SOM value.
#[derive(Clone)]
pub enum Value {
    /// The **nil** value.
    Nil,
    /// The **system** value.
    System,
    /// A boolean value (**true** or **false**).
    Boolean(bool),
    /// An integer value.
    Integer(i64),
    /// An floating-point value.
    Double(f64),
    /// An interned symbol value.
    Symbol(Interned),
    /// A string value.
    String(Rc<String>),
    /// An array of values.
    Array(SOMRef<Vec<Self>>),
    /// A block value, ready to be evaluated.
    Block(Rc<Block>),
    /// A generic (non-primitive) class instance.
    Instance(SOMRef<Instance>),
    /// A bare class object.
    Class(SOMRef<Class>),
    /// A bare invokable.
    Invokable(Rc<Method>),
}

impl Value {
    /// Get the class of the current value.
    pub fn class(&self, universe: &Universe) -> SOMRef<Class> {
        match self {
            Self::Nil => universe.nil_class(),
            Self::System => universe.system_class(),
            Self::Boolean(true) => universe.true_class(),
            Self::Boolean(false) => universe.false_class(),
            Self::Integer(_) => universe.integer_class(),
            Self::Double(_) => universe.double_class(),
            Self::Symbol(_) => universe.symbol_class(),
            Self::String(_) => universe.string_class(),
            Self::Array(_) => universe.array_class(),
            Self::Block(block) => block.class(universe),
            Self::Instance(instance) => instance.borrow().class(),
            Self::Class(class) => class.borrow().class(),
            Self::Invokable(invokable) => invokable.class(universe),
        }
    }

    /// Search for a given method for this value.
    pub fn lookup_method(&self, universe: &Universe, signature: Interned) -> Option<Rc<Method>> {
        self.class(universe).borrow().lookup_method(signature)
    }

    /// Search for a local binding within this value.
    pub fn lookup_local(&self, name: Interned) -> Option<Self> {
        match self {
            Self::Instance(instance) => instance.borrow().lookup_local(name),
            Self::Class(class) => class.borrow().lookup_local(name),
            _ => None,
        }
    }

    /// Assign a value to a local binding within this value.
    pub fn assign_local(&mut self, name: Interned, value: Self) -> Option<()> {
        match self {
            Self::Instance(instance) => instance.borrow_mut().assign_local(name, value),
            Self::Class(class) => class.borrow_mut().assign_local(name, value),
            _ => None,
        }
    }

    /// Get the string representation of this value.
    pub fn as_display(&self, universe: &Universe) -> String {
        match self {
            Self::Nil => "nil".to_string(),
            Self::System => "system".to_string(),
            Self::Boolean(value) => value.to_string(),
            Self::Integer(value) => value.to_string(),
            Self::Double(value) => value.to_string(),
            Self::Symbol(value) => {
                let symbol = universe.lookup_symbol(*value);
                if symbol.chars().any(|ch| ch.is_whitespace() || ch == '\'') {
                    format!("#'{}'", symbol.replace("'", "\\'"))
                } else {
                    format!("#{}", symbol)
                }
            }
            Self::String(value) => value.to_string(),
            Self::Array(values) => {
                // TODO: I think we can do better here (less allocations).
                let strings: Vec<String> = values
                    .borrow()
                    .iter()
                    .map(|value| value.as_display(universe))
                    .collect();
                format!("#({})", strings.join(" "))
            }
            Self::Block(block) => format!("instance of Block{}", block.nb_parameters() + 1),
            Self::Instance(instance) => format!(
                "instance of {} class",
                instance.borrow().class().borrow().name(),
            ),
            Self::Class(class) => class.borrow().name().to_string(),
            Self::Invokable(invokable) => {
                let signature = universe.lookup_symbol(invokable.signature());
                invokable
                    .holder()
                    .upgrade()
                    .map(|holder| format!("{}>>#{}", holder.borrow().name(), signature))
                    .unwrap_or_else(|| format!("??>>#{}", signature))
            }
        }
    }

    /// Get the debug string representation of this value.
    pub fn as_debug(&self, universe: &Universe) -> String {
        match self {
            Self::Nil => "Nil".to_string(),
            Self::System => "System".to_string(),
            Self::Boolean(val) => format!("Boolean({:?})", val),
            Self::Integer(val) => format!("Integer({:?})", val),
            Self::Double(val) => format!("Double({:?})", val),
            Self::Symbol(val) => format!("Symbol({:?})", val),
            Self::String(val) => format!("String({:?})", val),
            Self::Array(val) => format!("Array({:?})", val.borrow()),
            Self::Block(val) => format!("Block({:?})", val),
            Self::Instance(val) => format!("Instance({:?})", val.borrow()),
            Self::Class(val) => format!("Class({:?})", val.borrow()),
            Self::Invokable(val) => {
                let signature = universe.lookup_symbol(val.signature());
                let signature = val
                    .holder()
                    .upgrade()
                    .map(|holder| format!("{}>>#{}", holder.borrow().name(), signature))
                    .unwrap_or_else(|| format!("??>>#{}", signature));
                format!("Invokable({})", signature)
            }
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) | (Self::System, Self::System) => true,
            (Self::Boolean(a), Self::Boolean(b)) => a.eq(b),
            (Self::Integer(a), Self::Integer(b)) => a.eq(b),
            (Self::Integer(a), Self::Double(b)) => (*a as f64).eq(b),
            (Self::Double(a), Self::Integer(b)) => a.eq(&(*b as f64)),
            (Self::Double(a), Self::Double(b)) => a.eq(b),
            (Self::String(a), Self::String(b)) => a.eq(b),
            (Self::Symbol(a), Self::Symbol(b)) => a.eq(b),
            (Self::Array(a), Self::Array(b)) => a.eq(b),
            (Self::Instance(a), Self::Instance(b)) => Rc::ptr_eq(a, b),
            (Self::Class(a), Self::Class(b)) => Rc::ptr_eq(a, b),
            (Self::Block(a), Self::Block(b)) => Rc::ptr_eq(a, b),
            (Self::Invokable(a), Self::Invokable(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nil => f.debug_tuple("Nil").finish(),
            Self::System => f.debug_tuple("System").finish(),
            Self::Boolean(val) => f.debug_tuple("Boolean").field(val).finish(),
            Self::Integer(val) => f.debug_tuple("Integer").field(val).finish(),
            Self::Double(val) => f.debug_tuple("Double").field(val).finish(),
            Self::Symbol(val) => f.debug_tuple("Symbol").field(val).finish(),
            Self::String(val) => f.debug_tuple("String").field(val).finish(),
            Self::Array(val) => f.debug_tuple("Array").field(&val.borrow()).finish(),
            Self::Block(val) => f.debug_tuple("Block").field(val).finish(),
            Self::Instance(val) => f.debug_tuple("Instance").field(&val.borrow()).finish(),
            Self::Class(val) => f.debug_tuple("Class").field(&val.borrow()).finish(),
            Self::Invokable(val) => {
                let holder = val
                    .holder()
                    .upgrade()
                    .map(|holder| holder.borrow().name().to_string())
                    .unwrap_or_else(|| "??".to_string());
                f.debug_tuple("Invokable").field(&holder).finish()
            }
        }
    }
}
