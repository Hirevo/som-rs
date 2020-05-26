use std::cell::RefCell;
use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use anyhow::{anyhow, Error};

use som_core::ast::ClassDef;

use crate::class::Class;
use crate::frame::Frame;
use crate::instance::Instance;
use crate::interner::{InternedString, Interner};
use crate::value::Value;
use crate::SOMRef;

#[derive(Debug)]
pub struct Universe {
    /// Symbol interner.
    pub symbol_table: Interner,
    /// Map of known globals.
    pub globals: HashMap<String, SOMRef<Value>>,
    pub classpath: Vec<PathBuf>,

    pub object_class: SOMRef<Class>,
    pub class_class: SOMRef<Class>,
    pub metaclass_class: SOMRef<Class>,

    pub nil_class: SOMRef<Class>,
    pub integer_class: SOMRef<Class>,
    pub double_class: SOMRef<Class>,
    pub array_class: SOMRef<Class>,
    pub method_class: SOMRef<Class>,
    pub symbol_class: SOMRef<Class>,
    pub primitive_class: SOMRef<Class>,
    pub string_class: SOMRef<Class>,
    pub system_class: SOMRef<Class>,
    pub block_class: SOMRef<Class>,

    pub true_class: SOMRef<Class>,
    pub false_class: SOMRef<Class>,

    pub frames: Vec<Frame>,
}

impl Universe {
    pub fn from_classpath(classpath: Vec<PathBuf>) -> Result<Universe, Error> {
        let symbol_table = Interner::with_capacity(100);
        let globals = HashMap::new();

        let object_class = Universe::load_system_class(classpath.as_slice(), "Object")?;
        let class_class = Universe::load_system_class(classpath.as_slice(), "Class")?;
        let metaclass_class = Universe::load_system_class(classpath.as_slice(), "Metaclass")?;

        let nil_class = Universe::load_system_class(classpath.as_slice(), "Nil")?;
        let integer_class = Universe::load_system_class(classpath.as_slice(), "Integer")?;
        let array_class = Universe::load_system_class(classpath.as_slice(), "Array")?;
        let method_class = Universe::load_system_class(classpath.as_slice(), "Method")?;
        let symbol_class = Universe::load_system_class(classpath.as_slice(), "Symbol")?;
        let primitive_class = Universe::load_system_class(classpath.as_slice(), "Primitive")?;
        let string_class = Universe::load_system_class(classpath.as_slice(), "String")?;
        let system_class = Universe::load_system_class(classpath.as_slice(), "System")?;
        let block_class = Universe::load_system_class(classpath.as_slice(), "Block")?;
        let double_class = Universe::load_system_class(classpath.as_slice(), "Double")?;

        let true_class = Universe::load_system_class(classpath.as_slice(), "True")?;
        let false_class = Universe::load_system_class(classpath.as_slice(), "False")?;

        let true_value = Value::Instance(Rc::new(RefCell::new(Instance::from_class(&true_class))));
        let false_value =
            Value::Instance(Rc::new(RefCell::new(Instance::from_class(&false_class))));

        Ok(Universe {
            symbol_table,
            globals,
            classpath,
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
            block_class,
            double_class,
            true_class,
            false_class,
            frames: Vec::new(),
        })
    }

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
            let symbols: Vec<_> = som_lexer::Lexer::new(contents.as_str())
                .skip_comments(true)
                .skip_whitespace(true)
                .collect();

            // Parse class definition from the tokens.
            let defn = match som_parser::parse_file(symbols.as_slice()) {
                Some(defn) => defn,
                None => return Err(anyhow!("could not parse the '{}' system class", class_name)),
            };

            if defn.name != class_name {
                return Err(anyhow!(
                    "{}: class name is different from file name.",
                    path.display(),
                ));
            }

            return Ok(Rc::new(RefCell::new(Class::from_class_def(defn))));
        }

        Err(anyhow!("could not find the '{}' system class", class_name))
    }

    pub fn load_class(
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
                Err(_) => continue,
            };

            // Collect all tokens from the file.
            let symbols: Vec<_> = som_lexer::Lexer::new(contents.as_str())
                .skip_comments(true)
                .skip_whitespace(true)
                .collect();

            // Parse class definition from the tokens.
            let defn = match som_parser::parse_file(symbols.as_slice()) {
                Some(defn) => defn,
                None => continue,
            };

            if defn.name != class_name {
                return Err(anyhow!(
                    "{}: class name is different from file name.",
                    path.display(),
                ));
            }

            return Ok(Rc::new(RefCell::new(Class::from_class_def(defn))));
        }

        Err(anyhow!("could not find the '{}' system class", class_name))
    }

    pub fn nil_class(&self) -> SOMRef<Class> {
        self.nil_class.clone()
    }

    pub fn symbol_class(&self) -> SOMRef<Class> {
        self.symbol_class.clone()
    }
    pub fn string_class(&self) -> SOMRef<Class> {
        self.string_class.clone()
    }
    pub fn array_class(&self) -> SOMRef<Class> {
        self.array_class.clone()
    }

    pub fn integer_class(&self) -> SOMRef<Class> {
        self.integer_class.clone()
    }
    pub fn double_class(&self) -> SOMRef<Class> {
        self.double_class.clone()
    }

    pub fn true_class(&self) -> SOMRef<Class> {
        self.true_class.clone()
    }
    pub fn false_class(&self) -> SOMRef<Class> {
        self.false_class.clone()
    }

    pub fn metaclass_class(&self) -> SOMRef<Class> {
        self.metaclass_class.clone()
    }

    pub fn with_frame<T>(&mut self, self_ref: Value, func: impl FnOnce(&mut Universe) -> T) -> T {
        self.frames.push(Frame::with_self(self_ref));
        let ret = func(self);
        self.frames.pop();
        ret
    }

    pub fn lookup(&self, name: impl AsRef<str>) -> Option<SOMRef<Value>> {
        let name = name.as_ref();
        match name {
            "self" => {
                let frame = self.frames.last().expect("no frame left in universe");
                Some(frame.get_self())
            }
            "super" => {
                let frame = self.frames.last().expect("no frame left in universe");
                let class = frame.self_ref.borrow().class(self);
                let super_class = class.borrow().super_class();
                Some(Rc::new(RefCell::new(Value::Class(super_class))))
            }
            name => {
                for frame in self.frames.iter().rev() {
                    if let Some(value) = frame.bindings.get(name) {
                        return Some(value.clone());
                    }
                }
                None
            }
        }
    }

    pub fn lookup_global(&self, name: impl AsRef<str>) -> Option<SOMRef<Value>> {
        let name = name.as_ref();
        self.globals.get(name).map(Rc::clone)
    }
}
