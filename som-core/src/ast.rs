use crate::span::Span;

use std::ops;

#[derive(Debug, Clone, PartialEq)]
pub struct Spanned<T>(Span, T);

impl<T> ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl<T> ops::DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.1
    }
}

impl<T> Spanned<T> {
    pub fn new(span: Span, inner: T) -> Self {
        Spanned(span, inner)
    }

    pub fn span(&self) -> Span {
        self.0
    }
}

/// Represents a class definition.
///
/// Example:
/// ```text
/// Counter = (
///   | total |
///   new = ( self reset )
///   increment = ( total := total + 1 )
///   get = ( ^ total )
///   reset = ( total := 0 )
/// )
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct ClassDef {
    /// The name of the class.
    pub name: Span,
    /// The name of the superclass.
    pub super_class: Option<Span>,
    /// The locals for instances of that class.
    pub instance_locals: Vec<Span>,
    /// The methods declared for instances of that class.
    pub instance_methods: Vec<MethodDef>,
    /// The static locals for that class.
    pub static_locals: Vec<Span>,
    /// The static methods declared for that class.
    pub static_methods: Vec<MethodDef>,
}

/// Represents a method's kind.
///
/// Example:
/// ```text
/// "unary method"       increment = ( self increment: 1 )
/// "positional method"  increment: value = ( total := total + value )
/// "operator method"    + value = ( self increment: value )
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum MethodKind {
    /// A unary method definition.
    Unary,
    /// A positional method definition (keyword-based).
    Positional {
        /// The keywords for the method's paramters.
        keywords: Vec<Span>,
        /// The binding names for the method's parameters.
        parameters: Vec<Span>,
    },
    /// A binary operator method definiton.
    Operator {
        /// The binding name for the right-hand side.
        rhs: Span,
    },
}

/// Represents a method definition.
///
/// Example:
/// ```text
/// "unary method"       increment = ( self increment: 1 )
/// "positional method"  increment: value = ( total := total + value )
/// "operator method"    + value = ( self increment: value )
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct MethodDef {
    /// The whole method definition span.
    pub span: Span,
    /// The method's kind.
    pub kind: MethodKind,
    /// The method's signature (eg. `println`, `at:put:` or `==`).
    pub signature: String,
    /// The method's body.
    pub body: MethodBody,
}

/// Represents a method's body.
///
/// Exemple:
/// ```text
/// "primitive method body"
/// printString: string = primitive
///
/// "actual method body, with a local"
/// double: value = ( |clone| clone := double. ^ (double + clone) )
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct MethodBody {
    pub span: Span,
    pub kind: MethodBodyKind,
}

/// Represents the kind of a method's body.
///
/// Exemple:
/// ```text
/// "primitive method body"
/// printString: string = primitive
///
/// "actual method body, with a local"
/// double: value = ( |clone| clone := double. ^ (double + clone) )
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum MethodBodyKind {
    /// A primitive (meant to be implemented by the VM itself).
    Primitive,
    /// An actual body for the method, with locals.
    Body { locals: Vec<Span>, body: Body },
}

/// Represents the contents of a body (within a term or block).
///
/// Exemple:
/// ```text
/// "body within a term"
/// new = (
///     local := counter + 5.
///     (counter get) > 5
/// )
///
/// "body within a block"
/// [ :arg |
///     local := counter + arg.
///     arg * (counter get)
/// ]
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Body {
    /// The expressions in the body.
    pub exprs: Vec<Expression>,
    /// Is the last expression terminated with a period ?
    pub full_stopped: bool,
}

/// Represents an expression.
///
/// Exemple:
/// ```text
/// "reference"          counter
/// "assignment"         counter := 10
/// "messsage send"      counter incrementBy: 5
/// "binary operation"   counter <= 5
/// "exit operation"     ^counter
/// "literal"            'foo'
/// "block"              [ :value | counter incrementBy: value ]
/// "term"               ( counter increment )
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub span: Span,
    pub kind: ExpressionKind,
}

/// Represents the kind of an expression.
///
/// Exemple:
/// ```text
/// "reference"          counter
/// "assignment"         counter := 10
/// "messsage send"      counter incrementBy: 5
/// "binary operation"   counter <= 5
/// "exit operation"     ^counter
/// "literal"            'foo'
/// "block"              [ :value | counter incrementBy: value ]
/// "term"               ( counter increment )
/// ```

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind {
    /// A reference to a binding (eg. `counter`).
    Reference,
    /// An assignment to a binding (eg. `counter := 10`).
    Assignment(Span, Box<Expression>),
    /// A message send (eg. `counter incrementBy: 5`).
    Message(Message),
    // /// A binary operation (eg. `counter <= 5`).
    // BinaryOp(BinaryOp),
    /// An exit operation (eg. `^counter`).
    Exit(Box<Expression>),
    /// A literal (eg. `'foo'`, `10`, `#foo`, ...).
    Literal(Literal),
    /// A block (eg. `[ :value | counter incrementBy: value ]`).
    Block(Block),
    /// A term (eg. `( counter increment )`).
    Term(Term),
}

/// Represents a message send.
///
/// Exemple:
/// ```text
/// "unary message send"
/// 'hello, world' println
///
/// "positional message send"
/// range from: 0 to: 10
///
/// "binary operator message send"
/// value == 3
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Message {
    /// The object to which the message is sent to.
    pub receiver: Box<Expression>,
    /// The signature of the message (eg. "ifTrue:ifFalse:").
    pub signature: String,
    /// The list of dynamic values that are passed.
    pub kind: MessageKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MessageKind {
    Unary,
    Binary {
        rhs: Box<Expression>,
    },
    Positional {
        keywords: Vec<Span>,
        values: Vec<Expression>,
    },
}

/// Represents a binary operation.
///
/// Exemple:
/// ```text
/// counter <= 2
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOp {
    /// Represents the operator symbol.
    pub op: Span,
    /// Represents the left-hand side.
    pub lhs: Box<Expression>,
    /// Represents the right-hand side.
    pub rhs: Box<Expression>,
}

/// Represents a block.
///
/// Exemple:
/// ```text
/// "simple block"
/// [ 'hello, world' println ]
///
/// "block with parameter"
/// [ :value | value * 2 ]
///
/// "block with parameter and local"
/// [ :value | |serialized| serialized := value asString. serialized println ]
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    /// Represents the parameters' names.
    pub parameters: Vec<Span>,
    /// The names of the locals.
    pub locals: Vec<Span>,
    /// Represents the block's body.
    pub body: Body,
}

/// Represents a term.
///
/// Exemple:
/// ```text
/// "simple term"
/// ( 1 + 1 )
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct Term {
    /// The body of the term.
    pub body: Body,
}

/// Represents a literal.
///
/// Exemple:
/// ```text
/// #foo     "symbol literal"
/// 'hello'  "string literal"
/// 3.14     "double literal"
/// 42       "integer literal"
/// ```
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    /// Represents a symbol literal (eg. `#foo`).
    Symbol(Span),
    /// Represents a string literal (eg. `'hello'`).
    String(Span),
    /// Represents a decimal number literal (eg. `3.14`).
    Double,
    /// Represents a integer number literal (eg. `42`).
    Integer,
    /// Represents an array literal (eg. `#(1 2 3)`)
    Array(Vec<Expression>),
}
