//!
//! This is the bytecode compiler for the Simple Object Machine.
//!
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::{Rc, Weak};

use indexmap::{IndexMap, IndexSet};
use num_bigint::BigInt;

use som_core::ast;
use som_core::bytecode::Bytecode;

use crate::block::Block;
use crate::class::{Class, MaybeWeak};
use crate::interner::{Interned, Interner};
use crate::method::{Method, MethodEnv, MethodKind};
use crate::value::Value;
use crate::SOMRef;

#[derive(Debug, Clone)]
pub enum Literal {
    Symbol(Interned),
    String(Rc<String>),
    Double(f64),
    Integer(i64),
    BigInteger(BigInt),
    Array(Vec<u8>),
    Block(Rc<Block>),
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Literal::Symbol(val1), Literal::Symbol(val2)) => val1.eq(val2),
            (Literal::String(val1), Literal::String(val2)) => val1.eq(val2),
            (Literal::Double(val1), Literal::Double(val2)) => val1.eq(val2),
            (Literal::Integer(val1), Literal::Integer(val2)) => val1.eq(val2),
            (Literal::BigInteger(val1), Literal::BigInteger(val2)) => val1.eq(val2),
            (Literal::Array(val1), Literal::Array(val2)) => val1.eq(val2),
            (Literal::Block(val1), Literal::Block(val2)) => Rc::ptr_eq(val1, val2),
            _ => false,
        }
    }
}

impl Eq for Literal {}

impl Hash for Literal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Literal::Symbol(val) => {
                state.write(b"sym#");
                val.hash(state);
            }
            Literal::String(val) => {
                state.write(b"string#");
                val.hash(state);
            }
            Literal::Double(val) => {
                state.write(b"dbl#");
                val.to_bits().hash(state);
            }
            Literal::Integer(val) => {
                state.write(b"int#");
                val.hash(state);
            }
            Literal::BigInteger(val) => {
                state.write(b"bigint#");
                val.hash(state);
            }
            Literal::Array(val) => {
                state.write(b"array#");
                val.hash(state);
            }
            Literal::Block(val) => {
                state.write(b"blk");
                val.hash(state);
            }
        }
    }
}

enum FoundVar {
    Local(u8, u8),
    Argument(u8, u8),
    Field(u8),
}

trait GenCtxt {
    fn find_var(&mut self, name: &str) -> Option<FoundVar>;
    fn intern_symbol(&mut self, name: &str) -> Interned;
    fn class_name(&self) -> &str;
}

trait InnerGenCtxt: GenCtxt {
    fn as_gen_ctxt(&mut self) -> &mut dyn GenCtxt;
    fn push_instr(&mut self, instr: Bytecode);
    fn push_arg(&mut self, name: String) -> usize;
    fn push_local(&mut self, name: String) -> usize;
    fn push_literal(&mut self, literal: Literal) -> usize;
}

struct BlockGenCtxt<'a> {
    pub outer: &'a mut dyn GenCtxt,
    pub args: IndexSet<String>,
    pub locals: IndexSet<String>,
    pub literals: IndexSet<Literal>,
    pub body: Option<Vec<Bytecode>>,
}

impl GenCtxt for BlockGenCtxt<'_> {
    fn find_var(&mut self, name: &str) -> Option<FoundVar> {
        let name = match name {
            "super" => "self",
            name => name,
        };
        (self.locals.get_index_of(name))
            .map(|idx| FoundVar::Local(0, idx as u8))
            .or_else(|| (self.args.get_index_of(name)).map(|idx| FoundVar::Argument(0, idx as u8)))
            .or_else(|| {
                self.outer.find_var(name).map(|found| match found {
                    FoundVar::Local(up_idx, idx) => FoundVar::Local(up_idx + 1, idx),
                    FoundVar::Argument(up_idx, idx) => FoundVar::Argument(up_idx + 1, idx),
                    FoundVar::Field(idx) => FoundVar::Field(idx),
                })
            })
    }

    fn intern_symbol(&mut self, name: &str) -> Interned {
        self.outer.intern_symbol(name)
    }

    fn class_name(&self) -> &str {
        self.outer.class_name()
    }
}

impl InnerGenCtxt for BlockGenCtxt<'_> {
    fn as_gen_ctxt(&mut self) -> &mut dyn GenCtxt {
        self
    }

    fn push_instr(&mut self, instr: Bytecode) {
        let body = self.body.get_or_insert_with(|| vec![]);
        body.push(instr);
    }

    fn push_arg(&mut self, name: String) -> usize {
        let (idx, _) = self.args.insert_full(name);
        idx
    }

    fn push_local(&mut self, name: String) -> usize {
        let (idx, _) = self.locals.insert_full(name);
        idx
    }

    fn push_literal(&mut self, literal: Literal) -> usize {
        let (idx, _) = self.literals.insert_full(literal);
        idx
    }
}

struct MethodGenCtxt<'a> {
    pub signature: String,
    pub inner: BlockGenCtxt<'a>,
}

impl MethodGenCtxt<'_> {}

impl GenCtxt for MethodGenCtxt<'_> {
    fn find_var(&mut self, name: &str) -> Option<FoundVar> {
        self.inner.find_var(name)
    }

    fn intern_symbol(&mut self, name: &str) -> Interned {
        self.inner.intern_symbol(name)
    }

    fn class_name(&self) -> &str {
        self.inner.class_name()
    }
}

impl InnerGenCtxt for MethodGenCtxt<'_> {
    fn as_gen_ctxt(&mut self) -> &mut dyn GenCtxt {
        self
    }

    fn push_instr(&mut self, instr: Bytecode) {
        self.inner.push_instr(instr)
    }

    fn push_arg(&mut self, name: String) -> usize {
        self.inner.push_arg(name)
    }

    fn push_local(&mut self, name: String) -> usize {
        self.inner.push_local(name)
    }

    fn push_literal(&mut self, literal: Literal) -> usize {
        self.inner.push_literal(literal)
    }
}

trait MethodCodegen {
    fn codegen(&self, ctxt: &mut dyn InnerGenCtxt) -> Option<()>;
}

impl MethodCodegen for ast::Body {
    fn codegen(&self, ctxt: &mut dyn InnerGenCtxt) -> Option<()> {
        for expr in &self.exprs {
            expr.codegen(ctxt)?;
        }
        Some(())
    }
}

impl MethodCodegen for ast::Expression {
    fn codegen(&self, ctxt: &mut dyn InnerGenCtxt) -> Option<()> {
        match self {
            ast::Expression::Reference(name) => {
                match ctxt.find_var(name.as_str()) {
                    Some(FoundVar::Local(up_idx, idx)) => {
                        ctxt.push_instr(Bytecode::PushLocal(up_idx, idx))
                    }
                    Some(FoundVar::Argument(up_idx, idx)) => {
                        ctxt.push_instr(Bytecode::PushArgument(up_idx, idx))
                    }
                    Some(FoundVar::Field(idx)) => ctxt.push_instr(Bytecode::PushField(idx)),
                    None => {
                        let name = ctxt.intern_symbol(name);
                        let idx = ctxt.push_literal(Literal::Symbol(name));
                        ctxt.push_instr(Bytecode::PushGlobal(idx as u8));
                    }
                }
                Some(())
            }
            ast::Expression::Assignment(name, expr) => {
                expr.codegen(ctxt)?;
                ctxt.push_instr(Bytecode::Dup);
                match ctxt.find_var(name.as_str())? {
                    FoundVar::Local(up_idx, idx) => {
                        ctxt.push_instr(Bytecode::PopLocal(up_idx, idx))
                    }
                    FoundVar::Argument(up_idx, idx) => {
                        ctxt.push_instr(Bytecode::PopArgument(up_idx, idx))
                    }
                    FoundVar::Field(idx) => ctxt.push_instr(Bytecode::PopField(idx)),
                }
                Some(())
            }
            ast::Expression::Message(message) => {
                let super_send = match message.receiver.as_ref() {
                    ast::Expression::Reference(value) if value == "super" => true,
                    _ => false,
                };
                message.receiver.codegen(ctxt)?;
                message
                    .values
                    .iter()
                    .try_for_each(|value| value.codegen(ctxt))?;
                let sym = ctxt.intern_symbol(message.signature.as_str());
                let idx = ctxt.push_literal(Literal::Symbol(sym));
                if super_send {
                    ctxt.push_instr(Bytecode::SuperSend(idx as u8));
                } else {
                    ctxt.push_instr(Bytecode::Send(idx as u8));
                }
                Some(())
            }
            ast::Expression::BinaryOp(message) => {
                message.lhs.codegen(ctxt)?;
                message.rhs.codegen(ctxt)?;
                let sym = ctxt.intern_symbol(message.op.as_str());
                let idx = ctxt.push_literal(Literal::Symbol(sym));
                ctxt.push_instr(Bytecode::Send(idx as u8));
                Some(())
            }
            ast::Expression::Exit(expr) => {
                expr.codegen(ctxt)?;
                ctxt.push_instr(Bytecode::ReturnNonLocal);
                Some(())
            }
            ast::Expression::Literal(literal) => {
                fn convert_literal(ctxt: &mut dyn InnerGenCtxt, literal: &ast::Literal) -> Literal {
                    match literal {
                        ast::Literal::Symbol(val) => {
                            Literal::Symbol(ctxt.intern_symbol(val.as_str()))
                        }
                        ast::Literal::String(val) => Literal::String(Rc::new(val.clone())),
                        ast::Literal::Double(val) => Literal::Double(*val),
                        ast::Literal::Integer(val) => Literal::Integer(*val),
                        ast::Literal::BigInteger(val) => Literal::BigInteger(val.parse().unwrap()),
                        ast::Literal::Array(val) => {
                            let literals = val
                                .iter()
                                .map(|val| {
                                    let literal = convert_literal(ctxt, val);
                                    ctxt.push_literal(literal) as u8
                                })
                                .collect();
                            Literal::Array(literals)
                        }
                    }
                }

                let literal = convert_literal(ctxt, literal);
                let idx = ctxt.push_literal(literal);
                ctxt.push_instr(Bytecode::PushConstant(idx as u8));
                Some(())
            }
            ast::Expression::Block(val) => {
                let block = compile_block(ctxt.as_gen_ctxt(), val)?;
                let block = Rc::new(block);
                let block = Literal::Block(block);
                let idx = ctxt.push_literal(block);
                ctxt.push_instr(Bytecode::PushBlock(idx as u8));
                Some(())
            }
            ast::Expression::Term(term) => term
                .body
                .exprs
                .iter()
                .try_for_each(|expr| expr.codegen(ctxt)),
        }
    }
}

struct ClassGenCtxt<'a> {
    pub name: String,
    pub fields: IndexSet<Interned>,
    pub methods: IndexMap<Interned, Rc<Method>>,
    pub interner: &'a mut Interner,
}

impl GenCtxt for ClassGenCtxt<'_> {
    fn find_var(&mut self, name: &str) -> Option<FoundVar> {
        let sym = self.interner.intern(name);
        self.fields
            .get_index_of(&sym)
            .map(|idx| FoundVar::Field(idx as u8))
    }

    fn intern_symbol(&mut self, name: &str) -> Interned {
        self.interner.intern(name)
    }

    fn class_name(&self) -> &str {
        self.name.as_str()
    }
}

fn compile_method(outer: &mut dyn GenCtxt, defn: &ast::MethodDef) -> Option<Method> {
    // println!("(method) compiling '{}' ...", defn.signature);

    let mut ctxt = MethodGenCtxt {
        signature: defn.signature.clone(),
        inner: BlockGenCtxt {
            outer,
            args: {
                let mut args = IndexSet::new();
                args.insert(String::from("self"));
                args
            },
            locals: match &defn.body {
                ast::MethodBody::Primitive => IndexSet::new(),
                ast::MethodBody::Body { locals, .. } => locals.iter().cloned().collect(),
            },
            literals: IndexSet::new(),
            body: None,
        },
    };

    match &defn.kind {
        ast::MethodKind::Unary => {}
        ast::MethodKind::Positional { parameters } => {
            for param in parameters {
                ctxt.push_arg(param.clone());
            }
        }
        ast::MethodKind::Operator { rhs } => {
            ctxt.push_arg(rhs.clone());
        }
    }

    match &defn.body {
        ast::MethodBody::Primitive => {}
        ast::MethodBody::Body { body, .. } => {
            for expr in &body.exprs {
                expr.codegen(&mut ctxt)?;
                ctxt.push_instr(Bytecode::Pop);
            }
            ctxt.push_instr(Bytecode::PushArgument(0, 0));
            ctxt.push_instr(Bytecode::ReturnLocal);
        }
    }

    let method = Method {
        kind: match &defn.body {
            ast::MethodBody::Primitive => MethodKind::primitive_from_signature(
                ctxt.class_name().split_whitespace().next().unwrap(),
                defn.signature.as_str(),
            ),
            // ast::MethodBody::Primitive => MethodKind::NotImplemented(defn.signature.clone()),
            ast::MethodBody::Body { .. } => {
                let env = MethodEnv {
                    locals: ctxt.inner.locals.iter().map(|_| Value::Nil).collect(),
                    literals: ctxt.inner.literals.into_iter().collect(),
                    body: ctxt.inner.body.unwrap_or_default(),
                    inline_cache: RefCell::new(HashMap::new()),
                };
                MethodKind::Defined(env)
            }
        },
        holder: Weak::new(),
        signature: ctxt.signature,
    };

    // println!("(method) compiled '{}' !", defn.signature);

    Some(method)
}

fn compile_block(outer: &mut dyn GenCtxt, defn: &ast::Block) -> Option<Block> {
    // println!("(system) compiling block ...");

    let mut ctxt = BlockGenCtxt {
        outer,
        args: defn.parameters.iter().cloned().collect(),
        locals: defn.locals.iter().cloned().collect(),
        literals: IndexSet::new(),
        body: None,
    };

    let splitted = defn.body.exprs.split_last();
    if let Some((last, rest)) = splitted {
        for expr in rest {
            expr.codegen(&mut ctxt)?;
            ctxt.push_instr(Bytecode::Pop);
        }
        last.codegen(&mut ctxt)?;
        ctxt.push_instr(Bytecode::ReturnLocal);
    }

    let block = Block {
        frame: None,
        locals: ctxt.locals.into_iter().map(|_| Value::Nil).collect(),
        literals: ctxt.literals.into_iter().collect(),
        body: ctxt.body.unwrap_or_default(),
        nb_params: ctxt.args.len(),
        inline_cache: Rc::new(RefCell::new(HashMap::new())),
    };

    // println!("(system) compiled block !");

    Some(block)
}

// println!("compiling '{}' ...", defn.name);
pub fn compile_class(
    interner: &mut Interner,
    defn: &ast::ClassDef,
    super_class: Option<&SOMRef<Class>>,
) -> Option<SOMRef<Class>> {
    let mut locals = IndexSet::new();

    fn collect_static_locals(
        interner: &mut Interner,
        class: &SOMRef<Class>,
        locals: &mut IndexSet<Interned>,
    ) {
        if let Some(class) = class.borrow().super_class() {
            collect_static_locals(interner, &class, locals);
        }
        locals.extend(class.borrow().locals.keys().copied());
    }

    if let Some(super_class) = super_class {
        collect_static_locals(interner, &super_class.borrow().class(), &mut locals);
    }

    locals.extend(
        defn.static_locals
            .iter()
            .map(|name| interner.intern(name.as_str())),
    );

    let mut static_class_ctxt = ClassGenCtxt {
        name: format!("{} class", defn.name),
        fields: locals,
        methods: IndexMap::new(),
        interner,
    };

    let static_class = Rc::new(RefCell::new(Class {
        name: static_class_ctxt.name.clone(),
        class: MaybeWeak::Weak(Weak::new()),
        super_class: Weak::new(),
        locals: IndexMap::new(),
        methods: IndexMap::new(),
        is_static: true,
    }));

    for method in &defn.static_methods {
        let mut method = compile_method(&mut static_class_ctxt, method)?;
        let signature = static_class_ctxt.interner.intern(method.signature.as_str());
        method.holder = Rc::downgrade(&static_class);
        static_class_ctxt.methods.insert(signature, Rc::new(method));
    }

    let mut static_class_mut = static_class.borrow_mut();
    static_class_mut.locals = static_class_ctxt
        .fields
        .into_iter()
        .map(|name| (name, Value::Nil))
        .collect();
    static_class_mut.methods = static_class_ctxt.methods;
    drop(static_class_mut);

    // for method in static_class.borrow().methods.values() {
    //     println!("{}", method);
    // }

    let mut locals = IndexSet::new();

    fn collect_instance_locals(
        interner: &mut Interner,
        class: &SOMRef<Class>,
        locals: &mut IndexSet<Interned>,
    ) {
        if let Some(class) = class.borrow().super_class() {
            collect_instance_locals(interner, &class, locals);
        }
        locals.extend(class.borrow().locals.keys());
    }

    if let Some(super_class) = super_class {
        collect_instance_locals(interner, super_class, &mut locals);
    }

    locals.extend(
        defn.instance_locals
            .iter()
            .map(|name| interner.intern(name.as_str())),
    );

    let mut instance_class_ctxt = ClassGenCtxt {
        name: defn.name.clone(),
        fields: locals,
        methods: IndexMap::new(),
        interner,
    };

    let instance_class = Rc::new(RefCell::new(Class {
        name: instance_class_ctxt.name.clone(),
        class: MaybeWeak::Strong(static_class.clone()),
        super_class: Weak::new(),
        locals: IndexMap::new(),
        methods: IndexMap::new(),
        is_static: false,
    }));

    for method in &defn.instance_methods {
        let mut method = compile_method(&mut instance_class_ctxt, method)?;
        let signature = instance_class_ctxt
            .interner
            .intern(method.signature.as_str());
        method.holder = Rc::downgrade(&instance_class);
        instance_class_ctxt
            .methods
            .insert(signature, Rc::new(method));
    }

    let mut instance_class_mut = instance_class.borrow_mut();
    instance_class_mut.locals = instance_class_ctxt
        .fields
        .into_iter()
        .map(|name| (name, Value::Nil))
        .collect();
    instance_class_mut.methods = instance_class_ctxt.methods;
    drop(instance_class_mut);

    // for method in instance_class.borrow().methods.values() {
    //     println!("{}", method);
    // }

    // println!("compiled '{}' !", defn.name);

    Some(instance_class)
}
