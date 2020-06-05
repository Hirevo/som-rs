use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::time::Instant;

use anyhow::{anyhow, Error};

use crate::class::Class;
use crate::frame::{Frame, FrameKind};
use crate::interner::{Interned, Interner};
use crate::invokable::{Invoke, Return};
use crate::value::Value;
use crate::SOMRef;

/// The core classes of the SOM interpreter.
///
/// This struct allows to always keep a reference to important classes,
/// even in case of modifications to global bindings by user-defined code.
#[derive(Debug)]
pub struct CoreClasses {
    /// The **Object** class.
    pub object_class: SOMRef<Class>,
    /// The **Class** class.
    pub class_class: SOMRef<Class>,
    /// The **Class** class.
    pub metaclass_class: SOMRef<Class>,

    /// The **Nil** class.
    pub nil_class: SOMRef<Class>,
    /// The **Integer** class.
    pub integer_class: SOMRef<Class>,
    /// The **Double** class.
    pub double_class: SOMRef<Class>,
    /// The **Array** class.
    pub array_class: SOMRef<Class>,
    /// The **Method** class.
    pub method_class: SOMRef<Class>,
    /// The **Primitive** class.
    pub primitive_class: SOMRef<Class>,
    /// The **Symbol** class.
    pub symbol_class: SOMRef<Class>,
    /// The **String** class.
    pub string_class: SOMRef<Class>,
    /// The **System** class.
    pub system_class: SOMRef<Class>,

    /// The **Block** class.
    pub block_class: SOMRef<Class>,
    /// The **Block1** class.
    pub block1_class: SOMRef<Class>,
    /// The **Block2** class.
    pub block2_class: SOMRef<Class>,
    /// The **Block3** class.
    pub block3_class: SOMRef<Class>,

    /// The **Boolean** class.
    pub boolean_class: SOMRef<Class>,
    /// The **True** class.
    pub true_class: SOMRef<Class>,
    /// The **False** class.
    pub false_class: SOMRef<Class>,
}

/// The central data structure for the interpreter.
///
/// It represents the complete state of the interpreter, like the known class definitions,
/// the string interner and the stack frames.
pub struct Universe {
    /// The string interner for symbols.
    pub interner: Interner,
    /// The known global bindings.
    pub globals: HashMap<String, Value>,
    /// The path to search in for new classes.
    pub classpath: Vec<PathBuf>,
    /// The interpreter's core classes.
    pub core: CoreClasses,
    /// The time record of the universe's creation.
    pub start_time: Instant,
    /// The interpreter's stack frames.
    pub frames: Vec<SOMRef<Frame>>,
}

impl Universe {
    /// Initialize the universe from the given classpath.
    pub fn with_classpath(classpath: Vec<PathBuf>) -> Result<Self, Error> {
        let interner = Interner::with_capacity(100);
        let mut globals = HashMap::new();

        let object_class = Self::load_system_class(classpath.as_slice(), "Object")?;
        let class_class = Self::load_system_class(classpath.as_slice(), "Class")?;
        let metaclass_class = Self::load_system_class(classpath.as_slice(), "Metaclass")?;

        let nil_class = Self::load_system_class(classpath.as_slice(), "Nil")?;
        let integer_class = Self::load_system_class(classpath.as_slice(), "Integer")?;
        let array_class = Self::load_system_class(classpath.as_slice(), "Array")?;
        let method_class = Self::load_system_class(classpath.as_slice(), "Method")?;
        let symbol_class = Self::load_system_class(classpath.as_slice(), "Symbol")?;
        let primitive_class = Self::load_system_class(classpath.as_slice(), "Primitive")?;
        let string_class = Self::load_system_class(classpath.as_slice(), "String")?;
        let system_class = Self::load_system_class(classpath.as_slice(), "System")?;
        let double_class = Self::load_system_class(classpath.as_slice(), "Double")?;

        let block_class = Self::load_system_class(classpath.as_slice(), "Block")?;
        let block1_class = Self::load_system_class(classpath.as_slice(), "Block1")?;
        let block2_class = Self::load_system_class(classpath.as_slice(), "Block2")?;
        let block3_class = Self::load_system_class(classpath.as_slice(), "Block3")?;

        let boolean_class = Self::load_system_class(classpath.as_slice(), "Boolean")?;
        let true_class = Self::load_system_class(classpath.as_slice(), "True")?;
        let false_class = Self::load_system_class(classpath.as_slice(), "False")?;

        // initializeSystemClass(objectClass, null, "Object");
        // set_super_class(&object_class, &nil_class, &metaclass_class);
        object_class
            .borrow()
            .class()
            .borrow_mut()
            .set_class(&metaclass_class);
        object_class
            .borrow()
            .class()
            .borrow_mut()
            .set_super_class(&class_class);
        // initializeSystemClass(classClass, objectClass, "Class");
        set_super_class(&class_class, &object_class, &metaclass_class);
        // initializeSystemClass(metaclassClass, classClass, "Metaclass");
        set_super_class(&metaclass_class, &class_class, &metaclass_class);
        // initializeSystemClass(nilClass, objectClass, "Nil");
        set_super_class(&nil_class, &object_class, &metaclass_class);
        // initializeSystemClass(arrayClass, objectClass, "Array");
        set_super_class(&array_class, &object_class, &metaclass_class);
        // initializeSystemClass(methodClass, arrayClass, "Method");
        set_super_class(&method_class, &array_class, &metaclass_class);
        // initializeSystemClass(stringClass, objectClass, "String");
        set_super_class(&string_class, &object_class, &metaclass_class);
        // initializeSystemClass(symbolClass, stringClass, "Symbol");
        set_super_class(&symbol_class, &string_class, &metaclass_class);
        // initializeSystemClass(integerClass, objectClass, "Integer");
        set_super_class(&integer_class, &object_class, &metaclass_class);
        // initializeSystemClass(primitiveClass, objectClass, "Primitive");
        set_super_class(&primitive_class, &object_class, &metaclass_class);
        // initializeSystemClass(doubleClass, objectClass, "Double");
        set_super_class(&double_class, &object_class, &metaclass_class);

        set_super_class(&system_class, &object_class, &metaclass_class);

        set_super_class(&block_class, &object_class, &metaclass_class);
        set_super_class(&block1_class, &block_class, &metaclass_class);
        set_super_class(&block2_class, &block_class, &metaclass_class);
        set_super_class(&block3_class, &block_class, &metaclass_class);

        set_super_class(&boolean_class, &object_class, &metaclass_class);
        set_super_class(&true_class, &boolean_class, &metaclass_class);
        set_super_class(&false_class, &boolean_class, &metaclass_class);

        globals.insert("Object".into(), Value::Class(object_class.clone()));
        globals.insert("Class".into(), Value::Class(class_class.clone()));
        globals.insert("Metaclass".into(), Value::Class(metaclass_class.clone()));
        globals.insert("Nil".into(), Value::Class(nil_class.clone()));
        globals.insert("Integer".into(), Value::Class(integer_class.clone()));
        globals.insert("Array".into(), Value::Class(array_class.clone()));
        globals.insert("Method".into(), Value::Class(method_class.clone()));
        globals.insert("Symbol".into(), Value::Class(symbol_class.clone()));
        globals.insert("Primitive".into(), Value::Class(primitive_class.clone()));
        globals.insert("String".into(), Value::Class(string_class.clone()));
        globals.insert("System".into(), Value::Class(system_class.clone()));
        globals.insert("Double".into(), Value::Class(double_class.clone()));
        globals.insert("Boolean".into(), Value::Class(boolean_class.clone()));
        globals.insert("True".into(), Value::Class(true_class.clone()));
        globals.insert("False".into(), Value::Class(false_class.clone()));
        globals.insert("Block".into(), Value::Class(block_class.clone()));
        globals.insert("Block1".into(), Value::Class(block1_class.clone()));
        globals.insert("Block2".into(), Value::Class(block2_class.clone()));
        globals.insert("Block3".into(), Value::Class(block3_class.clone()));

        globals.insert("true".into(), Value::Boolean(true));
        globals.insert("false".into(), Value::Boolean(false));
        globals.insert("nil".into(), Value::Nil);
        globals.insert("system".into(), Value::System);

        Ok(Self {
            globals,
            interner,
            classpath,
            frames: Vec::new(),
            start_time: Instant::now(),
            core: CoreClasses {
                object_class,
                class_class,
                metaclass_class,
                nil_class,
                integer_class,
                array_class,
                method_class,
                symbol_class,
                primitive_class,
                string_class,
                system_class,
                double_class,
                block_class,
                block1_class,
                block2_class,
                block3_class,
                boolean_class,
                true_class,
                false_class,
            },
        })
    }

    /// Load a system class (with an incomplete hierarchy).
    pub fn load_system_class(
        classpath: &[impl AsRef<Path>],
        class_name: impl Into<String>,
    ) -> Result<SOMRef<Class>, Error> {
        let class_name = class_name.into();
        for path in classpath {
            let mut path = path.as_ref().join(class_name.as_str());
            path.set_extension("som");

            // Read file contents.
            let contents = match fs::read_to_string(path.as_path()) {
                Ok(contents) => contents,
                Err(err) if err.kind() == io::ErrorKind::NotFound => continue,
                Err(err) => return Err(Error::from(err)),
            };

            // Collect all tokens from the file.
            let tokens: Vec<_> = som_lexer::Lexer::new(contents.as_str())
                .skip_comments(true)
                .skip_whitespace(true)
                .collect();

            // Parse class definition from the tokens.
            let defn = match som_parser::parse_file(tokens.as_slice()) {
                Some(defn) => defn,
                None => return Err(anyhow!("could not parse the '{}' system class", class_name)),
            };

            if defn.name != class_name {
                return Err(anyhow!(
                    "{}: class name is different from file name.",
                    path.display(),
                ));
            }

            return Ok(Class::from_class_def(defn));
        }

        Err(anyhow!("could not find the '{}' system class", class_name))
    }

    /// Load a class from its name into this universe.
    pub fn load_class(&mut self, class_name: impl Into<String>) -> Result<SOMRef<Class>, Error> {
        let class_name = class_name.into();
        for path in self.classpath.iter() {
            let mut path = path.join(class_name.as_str());
            path.set_extension("som");

            // Read file contents.
            let contents = match fs::read_to_string(path.as_path()) {
                Ok(contents) => contents,
                Err(_) => continue,
            };

            // Collect all tokens from the file.
            let tokens: Vec<_> = som_lexer::Lexer::new(contents.as_str())
                .skip_comments(true)
                .skip_whitespace(true)
                .collect();

            // Parse class definition from the tokens.
            let defn = match som_parser::parse_file(tokens.as_slice()) {
                Some(defn) => defn,
                None => continue,
            };

            if defn.name != class_name {
                return Err(anyhow!(
                    "{}: class name is different from file name.",
                    path.display(),
                ));
            }

            let super_class = if let Some(ref super_class) = defn.super_class {
                match self.lookup_global(super_class) {
                    Some(Value::Class(super_class)) => super_class,
                    _ => self.load_class(super_class)?,
                }
            } else {
                self.core.object_class.clone()
            };

            let class = Class::from_class_def(defn);
            set_super_class(&class, &super_class, &self.core.metaclass_class);

            self.globals.insert(
                class.borrow().name().to_string(),
                Value::Class(class.clone()),
            );

            return Ok(class);
        }

        Err(anyhow!("could not find the '{}' class", class_name))
    }

    /// Load a class from its path into this universe.
    pub fn load_class_from_path(&mut self, path: impl AsRef<Path>) -> Result<SOMRef<Class>, Error> {
        let path = path.as_ref();
        let file_stem = path
            .file_stem()
            .ok_or_else(|| anyhow!("The given path has no file stem"))?;

        // Read file contents.
        let contents = match fs::read_to_string(path) {
            Ok(contents) => contents,
            Err(err) => return Err(Error::from(err)),
        };

        // Collect all tokens from the file.
        let tokens: Vec<_> = som_lexer::Lexer::new(contents.as_str())
            .skip_comments(true)
            .skip_whitespace(true)
            .collect();

        // Parse class definition from the tokens.
        let defn = match som_parser::parse_file(tokens.as_slice()) {
            Some(defn) => defn,
            None => return Err(Error::msg("could not parse file")),
        };

        if defn.name.as_str() != file_stem {
            return Err(anyhow!(
                "{}: class name is different from file name.",
                path.display(),
            ));
        }

        let super_class = if let Some(ref super_class) = defn.super_class {
            match self.lookup_global(super_class) {
                Some(Value::Class(class)) => class,
                _ => self.load_class(super_class)?,
            }
        } else {
            self.core.object_class.clone()
        };

        let class = Class::from_class_def(defn);
        set_super_class(&class, &super_class, &self.core.metaclass_class);

        Ok(class)
    }

    /// Get the **Nil** class.
    pub fn nil_class(&self) -> SOMRef<Class> {
        self.core.nil_class.clone()
    }
    /// Get the **System** class.
    pub fn system_class(&self) -> SOMRef<Class> {
        self.core.system_class.clone()
    }

    /// Get the **Symbol** class.
    pub fn symbol_class(&self) -> SOMRef<Class> {
        self.core.symbol_class.clone()
    }
    /// Get the **String** class.
    pub fn string_class(&self) -> SOMRef<Class> {
        self.core.string_class.clone()
    }
    /// Get the **Array** class.
    pub fn array_class(&self) -> SOMRef<Class> {
        self.core.array_class.clone()
    }

    /// Get the **Integer** class.
    pub fn integer_class(&self) -> SOMRef<Class> {
        self.core.integer_class.clone()
    }
    /// Get the **Double** class.
    pub fn double_class(&self) -> SOMRef<Class> {
        self.core.double_class.clone()
    }

    /// Get the **Block** class.
    pub fn block_class(&self) -> SOMRef<Class> {
        self.core.block_class.clone()
    }
    /// Get the **Block1** class.
    pub fn block1_class(&self) -> SOMRef<Class> {
        self.core.block1_class.clone()
    }
    /// Get the **Block2** class.
    pub fn block2_class(&self) -> SOMRef<Class> {
        self.core.block2_class.clone()
    }
    /// Get the **Block3** class.
    pub fn block3_class(&self) -> SOMRef<Class> {
        self.core.block3_class.clone()
    }

    /// Get the **True** class.
    pub fn true_class(&self) -> SOMRef<Class> {
        self.core.true_class.clone()
    }
    /// Get the **False** class.
    pub fn false_class(&self) -> SOMRef<Class> {
        self.core.false_class.clone()
    }

    /// Get the **Metaclass** class.
    pub fn metaclass_class(&self) -> SOMRef<Class> {
        self.core.metaclass_class.clone()
    }

    /// Get the **Method** class.
    pub fn method_class(&self) -> SOMRef<Class> {
        self.core.method_class.clone()
    }
    /// Get the **Primitive** class.
    pub fn primitive_class(&self) -> SOMRef<Class> {
        self.core.primitive_class.clone()
    }
}

impl Universe {
    /// Execute a piece of code within a new stack frame.
    pub fn with_frame<T>(&mut self, kind: FrameKind, func: impl FnOnce(&mut Self) -> T) -> T {
        self.frames
            .push(Rc::new(RefCell::new(Frame::from_kind(kind))));
        let ret = func(self);
        self.frames.pop();
        ret
    }

    /// Get the current frame.
    pub fn current_frame(&self) -> &SOMRef<Frame> {
        self.frames.last().expect("no frames left")
    }

    /// Get the method invocation frame for the current frame.
    pub fn current_method_frame(&self) -> SOMRef<Frame> {
        Frame::method_frame(self.current_frame())
    }

    /// Intern a symbol.
    pub fn intern_symbol(&mut self, symbol: &str) -> Interned {
        self.interner.intern(symbol)
    }

    /// Lookup a symbol.
    pub fn lookup_symbol(&self, symbol: Interned) -> &str {
        self.interner.lookup(symbol)
    }

    /// Search for a local binding.
    pub fn lookup_local(&self, name: impl AsRef<str>) -> Option<Value> {
        let name = name.as_ref();
        match name {
            "self" => {
                let frame = self.current_frame();
                let self_value = frame.borrow().get_self();
                Some(self_value)
            }
            "super" => {
                let frame = self.current_frame();
                let class = frame.borrow().get_self().class(self);
                let super_class = class.borrow().super_class();
                Some(super_class.map(Value::Class).unwrap_or(Value::Nil))
            }
            name => self.current_frame().borrow().lookup_local(name),
        }
    }

    /// Search for a global binding.
    pub fn lookup_global(&self, name: impl AsRef<str>) -> Option<Value> {
        let name = name.as_ref();
        self.globals.get(name).cloned()
    }

    /// Assign a value to a local binding.
    pub fn assign_local(&mut self, name: impl AsRef<str>, value: Value) -> Option<()> {
        self.current_frame().borrow_mut().assign_local(name, value)
    }

    /// Assign a value to a global binding.
    pub fn assign_global(&mut self, name: impl AsRef<str>, value: Value) -> Option<()> {
        self.globals
            .insert(name.as_ref().to_string(), value)
            .map(|_| ())
    }

    /// Call `System>>#initialize:` with the given name, if it is defined.
    pub fn initialize(&mut self, args: Vec<Value>) -> Option<Return> {
        let initialize = Value::System.lookup_method(self, "initialize:")?;
        let args = Value::Array(Rc::new(RefCell::new(args)));

        Some(initialize.invoke(self, vec![Value::System, args]))
    }
}

impl Universe {
    /// Call `System>>#unknownGlobal:` with the given name, if it is defined.
    pub fn unknown_global(&mut self, name: impl AsRef<str>) -> Option<Return> {
        let sym = self.intern_symbol(name.as_ref());
        let method = Value::System.lookup_method(self, "unknownGlobal:")?;

        match method.invoke(self, vec![Value::System, Value::Symbol(sym)]) {
            Return::Local(value) | Return::NonLocal(value, _) => Some(Return::Local(value)),
            Return::Exception(err) => Some(Return::Exception(format!(
                "(from 'System>>#unknownGlobal:') {}",
                err,
            ))),
            Return::Restart => Some(Return::Exception(
                "(from 'System>>#unknownGlobal:') incorrectly asked for a restart".to_string(),
            )),
        }
    }
}

fn set_super_class(
    class: &SOMRef<Class>,
    super_class: &SOMRef<Class>,
    metaclass_class: &SOMRef<Class>,
) {
    class.borrow_mut().set_super_class(super_class);
    class
        .borrow()
        .class()
        .borrow_mut()
        .set_super_class(&super_class.borrow().class());
    class
        .borrow()
        .class()
        .borrow_mut()
        .set_class(metaclass_class);
}
