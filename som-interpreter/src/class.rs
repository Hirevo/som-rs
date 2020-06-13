use std::cell::RefCell;
use std::fmt;
use std::rc::{Rc, Weak};

use indexmap::IndexMap;

use som_core::ast::{ClassDef, MethodBodyKind};
use som_core::span::Span;

use crate::interner::{Interned, Interner};
use crate::method::{Method, MethodKind};
use crate::value::Value;
use crate::{SOMRef, SOMWeakRef};

/// A reference that may be either weak or owned/strong.
#[derive(Debug, Clone)]
pub enum MaybeWeak<A> {
    /// An owned reference.
    Strong(SOMRef<A>),
    /// A weak reference.
    Weak(SOMWeakRef<A>),
}

/// Represents a loaded class.
#[derive(Clone)]
pub struct Class {
    /// The class' name.
    pub name: String,
    /// The class' source code.
    pub source_code: Rc<String>,
    /// The class of this class.
    pub class: MaybeWeak<Class>,
    /// The superclass of this class.
    // TODO: Should probably be `Option<SOMRef<Class>>`.
    pub super_class: SOMWeakRef<Class>,
    /// The class' locals.
    pub locals: IndexMap<Interned, Value>,
    /// The class' methods/invokables.
    pub methods: IndexMap<Interned, Rc<Method>>,
    /// Is this class a static one ?
    pub is_static: bool,
}

impl Class {
    /// Load up a class from its class definition from the AST.
    pub fn from_class_def(
        interner: &mut Interner,
        source_code: String,
        defn: ClassDef,
    ) -> Result<SOMRef<Class>, String> {
        let static_locals = {
            let mut static_locals = IndexMap::new();
            for field in defn.static_locals.iter() {
                let field = field.to_str(source_code.as_str());
                let field_sym = interner.intern(field);
                if static_locals.insert(field_sym, Value::Nil).is_some() {
                    return Err(format!(
                        "{}: the field named '{}' is already defined in this class",
                        defn.name.to_str(source_code.as_str()),
                        field,
                    ));
                }
            }
            static_locals
        };

        let instance_locals = {
            let mut instance_locals = IndexMap::new();
            for field in defn.instance_locals.iter() {
                let field = field.to_str(source_code.as_str());
                let field_sym = interner.intern(field);
                if instance_locals.insert(field_sym, Value::Nil).is_some() {
                    return Err(format!(
                        "{}: the field named '{}' is already defined in this class",
                        defn.name.to_str(source_code.as_str()),
                        field,
                    ));
                }
            }
            instance_locals
        };

        let source_code = Rc::new(source_code);

        let static_class = Rc::new(RefCell::new(Self {
            name: format!("{} class", defn.name.to_str(source_code.as_str())),
            source_code: source_code.clone(),
            class: MaybeWeak::Weak(Weak::new()),
            super_class: Weak::new(),
            locals: static_locals,
            methods: IndexMap::new(),
            is_static: true,
        }));

        let instance_class = Rc::new(RefCell::new(Self {
            name: defn.name.to_str(source_code.as_str()).to_string(),
            source_code: source_code.clone(),
            class: MaybeWeak::Strong(static_class.clone()),
            super_class: Weak::new(),
            locals: instance_locals,
            methods: IndexMap::new(),
            is_static: false,
        }));

        let static_methods = defn
            .static_methods
            .iter()
            .map(|method| {
                let signature = method.signature.clone();
                let kind = match method.body.kind {
                    MethodBodyKind::Primitive => MethodKind::primitive_from_signature(
                        instance_class.borrow().name(),
                        method.signature.as_str(),
                    ),
                    MethodBodyKind::Body { .. } => MethodKind::Defined(method.clone()),
                };
                let signature = interner.intern(signature.as_str());
                let method = Method {
                    kind,
                    signature,
                    holder: Rc::downgrade(&static_class),
                };
                (signature, Rc::new(method))
            })
            .collect();

        let instance_methods = defn
            .instance_methods
            .iter()
            .map(|method| {
                let signature = method.signature.clone();
                let kind = match method.body.kind {
                    MethodBodyKind::Primitive => MethodKind::primitive_from_signature(
                        instance_class.borrow().name(),
                        method.signature.as_str(),
                    ),
                    MethodBodyKind::Body { .. } => MethodKind::Defined(method.clone()),
                };
                let signature = interner.intern(signature.as_str());
                let method = Method {
                    kind,
                    signature,
                    holder: Rc::downgrade(&instance_class),
                };
                (signature, Rc::new(method))
            })
            .collect();

        static_class.borrow_mut().methods = static_methods;
        instance_class.borrow_mut().methods = instance_methods;

        Ok(instance_class)
    }

    /// Get the class' name.
    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    /// Get the class of this class.
    pub fn class(&self) -> SOMRef<Self> {
        match self.class {
            MaybeWeak::Weak(ref weak) => weak.upgrade().unwrap_or_else(|| {
                panic!("superclass dropped, cannot upgrade ref ({})", self.name())
            }),
            MaybeWeak::Strong(ref owned) => owned.clone(),
        }
    }

    /// Set the class of this class (as a weak reference).
    pub fn set_class(&mut self, class: &SOMRef<Self>) {
        self.class = MaybeWeak::Weak(Rc::downgrade(class));
    }

    /// Set the class of this class (as a strong reference).
    pub fn set_class_owned(&mut self, class: &SOMRef<Self>) {
        self.class = MaybeWeak::Strong(class.clone());
    }

    /// Get the superclass of this class.
    pub fn super_class(&self) -> Option<SOMRef<Self>> {
        self.super_class.upgrade()
    }

    /// Set the superclass of this class (as a weak reference).
    pub fn set_super_class(&mut self, class: &SOMRef<Self>) {
        self.super_class = Rc::downgrade(class);
    }

    pub fn resolve_span(&self, span: Span) -> &str {
        span.to_str(self.source_code.as_str())
    }

    /// Search for a given method within this class.
    pub fn lookup_method(&self, signature: Interned) -> Option<Rc<Method>> {
        self.methods.get(&signature).cloned().or_else(|| {
            self.super_class
                .upgrade()?
                .borrow()
                .lookup_method(signature)
        })
    }

    /// Search for a local binding.
    pub fn lookup_local(&self, name: Interned) -> Option<Value> {
        self.locals.get(&name).cloned().or_else(|| {
            let super_class = self.super_class()?;
            let local = super_class.borrow().lookup_local(name)?;
            Some(local)
        })
    }

    /// Assign a value to a local binding.
    pub fn assign_local(&mut self, name: Interned, value: Value) -> Option<()> {
        if let Some(local) = self.locals.get_mut(&name) {
            *local = value;
            return Some(());
        }
        let super_class = self.super_class()?;
        super_class.borrow_mut().assign_local(name, value)?;
        Some(())
    }
}

impl fmt::Debug for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Class")
            .field("name", &self.name)
            .field("locals", &self.locals.keys())
            // .field("class", &self.class)
            // .field("super_class", &self.super_class)
            .finish()
    }
}
