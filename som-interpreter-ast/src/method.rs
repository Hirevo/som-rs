use som_core::ast;

use crate::class::Class;
use crate::primitives;
use crate::primitives::PrimitiveFn;
use crate::universe::Universe;
use crate::{SOMRef, SOMWeakRef};

/// The kind of a class method.
#[derive(Clone)]
pub enum MethodKind {
    /// A user-defined method from the AST.
    Defined(ast::MethodDef),
    /// An interpreter primitive.
    Primitive(PrimitiveFn),
    /// A non-implemented primitive.
    NotImplemented(String),
}

impl MethodKind {
    /// Return the interpreter primitive matching a given class name and signature.
    pub fn primitive_from_signature(
        class_name: impl AsRef<str>,
        signature: impl AsRef<str>,
    ) -> Self {
        let class_name = class_name.as_ref();
        let signature = signature.as_ref();
        let primitive = match class_name {
            "Object" => primitives::object::get_primitive(signature),
            "Class" => primitives::class::get_primitive(signature),
            "Integer" => primitives::integer::get_primitive(signature),
            "Double" => primitives::double::get_primitive(signature),
            "Array" => primitives::array::get_primitive(signature),
            "String" => primitives::string::get_primitive(signature),
            "Symbol" => primitives::symbol::get_primitive(signature),
            "System" => primitives::system::get_primitive(signature),
            "Method" => primitives::method::get_primitive(signature),
            "Primitive" => primitives::method::get_primitive(signature),
            "Block" => primitives::block1::get_primitive(signature),
            "Block1" => primitives::block1::get_primitive(signature),
            "Block2" => primitives::block2::get_primitive(signature),
            "Block3" => primitives::block3::get_primitive(signature),
            _ => None,
        };
        // println!(
        //     "loading primitive of '{}>>#{}': {}",
        //     class_name,
        //     signature,
        //     primitive.is_some()
        // );
        primitive.map(MethodKind::Primitive).unwrap_or_else(|| {
            MethodKind::NotImplemented(format!("{}>>#{}", class_name, signature))
        })
        // .unwrap_or_else(|| panic!("unimplemented primitive: '{}>>#{}'", class_name, signature))
    }

    /// Whether this invocable is a primitive.
    pub fn is_primitive(&self) -> bool {
        matches!(self, Self::Primitive(_))
    }
}

/// Represents a class method.
#[derive(Clone)]
pub struct Method {
    pub kind: MethodKind,
    pub holder: SOMWeakRef<Class>,
    pub signature: String,
}

impl Method {
    pub fn class(&self, universe: &Universe) -> SOMRef<Class> {
        if self.is_primitive() {
            universe.primitive_class()
        } else {
            universe.method_class()
        }
    }

    pub fn kind(&self) -> &MethodKind {
        &self.kind
    }

    pub fn holder(&self) -> &SOMWeakRef<Class> {
        &self.holder
    }

    pub fn signature(&self) -> &str {
        self.signature.as_str()
    }

    /// Whether this invocable is a primitive.
    pub fn is_primitive(&self) -> bool {
        self.kind.is_primitive()
    }
}
