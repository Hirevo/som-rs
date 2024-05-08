use std::convert::TryInto;
use std::fmt;
use std::mem::size_of;
use std::ptr::NonNull;

use num_bigint::BigInt;

use som_gc::{Gc, GcBox, Trace};

use crate::block::Block;
use crate::class::Class;
use crate::instance::Instance;
use crate::interner::Interned;
use crate::method::Method;
use crate::universe::Universe;
use crate::SOMRef;

static_assertions::const_assert_eq!(size_of::<f64>(), 8);
static_assertions::assert_eq_size!(f64, u64, *const ());

/// Canonical `NaN` representation (minimum bitfield to represent `NaN`).
///
/// Since we are hijacking most bits in `NaN` values, all legitimate `NaN` will need
/// to be "canonicalized" to this representation, which will be the only actual `NaN` value.
///
/// This isn't that bad since SOM doesn't have many bit fiddling facilities anyway, so it is
/// unlikely one would need to inspect bits in `NaN` values.
///
/// An `NaN` is indicated by:
/// - sign bit is `0`
/// - exponent bits are all `1`
/// - first mantissa bit is `1`
const CANON_NAN_BITS: u64 = 0x7FF8000000000000;

// However as long as any bit is set in the mantissa with the exponent of all
// ones this value is a NaN, and it even ignores the sign bit.
// (NOTE: we have to use __builtin_isnan here since some isnan implementations are not constexpr)
// FIXME: `is_nan` is not a `const fn` yet
// static_assertions::const_assert!(f64::from_bits(0x7FF0000000000001).is_nan());
// static_assertions::const_assert!(f64::from_bits(0xFFF0000000040000).is_nan());

/// Base bit pattern needed to be set for all tags.
///
/// All tags will add at least one more bit, to distinguish from `CANON_NAN_BITS`.
///
/// This means (after `<< TAG_SHIFT`):
/// - sign bit is `0`
/// - exponent bits are all `1`
/// - first mantissa bit is `1`
const BASE_TAG: u64 = 0x7FF8;

/// Base bit pattern needed to be set for all tags that are GC-managed.
///
/// It is similar to `BASE_TAG` instead that it sets the sign bit to signify that
/// this is a pointer-type.
///
/// This means (after `<< TAG_SHIFT`):
/// - sign bit is `1`
/// - exponent bits are all `1`
/// - first mantissa bit is `1`
const CELL_BASE_TAG: u64 = 0x8000 | BASE_TAG;

// On all current 64-bit systems this code runs, pointers actually only use the
// lowest 6 bytes which fits neatly into our NaN payload with the top two bytes
// left over for marking it as a NaN and tagging the type.
// Note that we do need to take care when extracting the pointer value but this
// is explained in the extract_pointer method.

// Tags for non-pointer types

/// Tag bits for the `Nil` type.
const NIL_TAG: u64 = 0b001 | BASE_TAG;
/// Tag bits for the `System` type.
const SYSTEM_TAG: u64 = 0b010 | BASE_TAG;
/// Tag bits for the `Integer` type.
const INTEGER_TAG: u64 = 0b011 | BASE_TAG; // Same bit position as `BIG_INTEGER_TAG`
/// Tag bits for the `Boolean` type.
const BOOLEAN_TAG: u64 = 0b100 | BASE_TAG;
/// Tag bits for the `Symbol` type.
const SYMBOL_TAG: u64 = 0b101 | BASE_TAG;

// The following non-pointer type tags are still available (maybe useful for optimisations ?):

// /// Tag bits for the `???` type.
// const RESERVED1_TAG: u64 = 0b110 | BASE_TAG;
// /// Tag bits for the `???` type.
// const RESERVED2_TAG: u64 = 0b111 | BASE_TAG;

// Tags for pointer types

/// Tag bits for the `String` type.
const STRING_TAG: u64 = 0b001 | CELL_BASE_TAG;
/// Tag bits for the `Array` type.
const ARRAY_TAG: u64 = 0b010 | CELL_BASE_TAG;
/// Tag bits for the `BigInteger` type.
const BIG_INTEGER_TAG: u64 = 0b011 | CELL_BASE_TAG; // Same bit position as `INTEGER_TAG`
/// Tag bits for the `Block` type.
const BLOCK_TAG: u64 = 0b100 | CELL_BASE_TAG;
/// Tag bits for the `Class` type.
const CLASS_TAG: u64 = 0b101 | CELL_BASE_TAG;
/// Tag bits for the `Instance` type.
const INSTANCE_TAG: u64 = 0b110 | CELL_BASE_TAG;
/// Tag bits for the `Invokable` type.
const INVOKABLE_TAG: u64 = 0b111 | CELL_BASE_TAG;

/// The amount of bits to shift tags in the correct position within a 64-bit value.
const TAG_SHIFT: u64 = 48;

/// Bit pattern used to quickly extract the tag bits from a 64-bit value.
const TAG_EXTRACTION: u64 = 0xFFFF << TAG_SHIFT;

/// Bit pattern used to quickly check if a given 64-bit value houses a pointer-type value.
const IS_CELL_PATTERN: u64 = CELL_BASE_TAG << TAG_SHIFT;

// Here is a nice diagram to summarize how our NaN-boxing works:
// (s = sign bit, e = exponent bit, m = mantissa bit)
//
//     tag bits                       payload bits
// SEEEEEEEEEEEMMMM MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM
// 0111111111111000 000... -> is the only real NaN
// 0111111111111xxx yyy... -> xxx = non-pointer type, yyy = value
// 1111111111111xxx yyy... -> xxx = pointer type,     yyy = pointer value

/// Represents an SOM value.
#[derive(Clone, Copy, Eq, Hash)]
pub struct Value {
    /// The 64-bit value that is used to store SOM values using NaN-boxing.
    encoded: u64,
}

impl Default for Value {
    fn default() -> Self {
        Self::NIL
    }
}

impl Value {
    /// Returns the tag bits of the value.
    #[inline(always)]
    pub fn tag(self) -> u64 {
        (self.encoded & TAG_EXTRACTION) >> TAG_SHIFT
    }
    /// Returns the payload bits of the value.
    #[inline(always)]
    pub fn payload(self) -> u64 {
        self.encoded & !TAG_EXTRACTION
    }

    /// Returns whether this value is a pointer type value.
    #[inline(always)]
    pub fn is_cell(self) -> bool {
        (self.encoded & IS_CELL_PATTERN) == IS_CELL_PATTERN
    }

    // `is_*` methods for pointer types

    /// Returns whether this value is a big integer.
    #[inline(always)]
    pub fn is_big_integer(self) -> bool {
        self.tag() == BIG_INTEGER_TAG
    }
    /// Returns whether this value is a string.
    #[inline(always)]
    pub fn is_string(self) -> bool {
        self.tag() == STRING_TAG
    }
    /// Returns whether this value is an array.
    #[inline(always)]
    pub fn is_array(self) -> bool {
        self.tag() == ARRAY_TAG
    }
    /// Returns whether this value is a block.
    #[inline(always)]
    pub fn is_block(self) -> bool {
        self.tag() == BLOCK_TAG
    }
    /// Returns whether this value is a class.
    #[inline(always)]
    pub fn is_class(self) -> bool {
        self.tag() == CLASS_TAG
    }
    /// Returns whether this value is an instance.
    #[inline(always)]
    pub fn is_instance(self) -> bool {
        self.tag() == INSTANCE_TAG
    }
    /// Returns whether this value is an invocable.
    #[inline(always)]
    pub fn is_invocable(self) -> bool {
        self.tag() == INVOKABLE_TAG
    }

    // `is_*` methods for pointer types

    /// Returns whether this value is `nil``.
    #[inline(always)]
    pub fn is_nil(self) -> bool {
        self.tag() == NIL_TAG
    }
    /// Returns whether this value is `system`.
    #[inline(always)]
    pub fn is_system(self) -> bool {
        self.tag() == SYSTEM_TAG
    }
    /// Returns whether this value is an integer.
    #[inline(always)]
    pub fn is_integer(self) -> bool {
        self.tag() == INTEGER_TAG
    }
    /// Returns whether this value is a boolean.
    #[inline(always)]
    pub fn is_boolean(self) -> bool {
        self.tag() == BOOLEAN_TAG
    }
    /// Returns whether this value is a symbol.
    #[inline(always)]
    pub fn is_symbol(self) -> bool {
        self.tag() == SYMBOL_TAG
    }

    /// Returns whether this value is a double.
    #[inline(always)]
    pub fn is_double(self) -> bool {
        // A double is any value which does not have the full exponent and top mantissa bit set or has
        // exactly only those bits set.
        (self.encoded & CANON_NAN_BITS) != CANON_NAN_BITS || (self.encoded == CANON_NAN_BITS)
    }

    // `as_*` for pointer types

    /// Returns this value as a big integer, if such is its type.
    #[inline(always)]
    pub fn as_big_integer(self) -> Option<Gc<BigInt>> {
        self.is_big_integer().then(|| self.extract_gc_cell())
    }
    /// Returns this value as a string, if such is its type.
    #[inline(always)]
    pub fn as_string(self) -> Option<Gc<String>> {
        self.is_string().then(|| self.extract_gc_cell())
    }
    /// Returns this value as an array, if such is its type.
    #[inline(always)]
    pub fn as_array(self) -> Option<SOMRef<Vec<Value>>> {
        self.is_array().then(|| self.extract_gc_cell())
    }
    /// Returns this value as a block, if such is its type.
    #[inline(always)]
    pub fn as_block(self) -> Option<Gc<Block>> {
        self.is_block().then(|| self.extract_gc_cell())
    }
    /// Returns this value as a class, if such is its type.
    #[inline(always)]
    pub fn as_class(self) -> Option<SOMRef<Class>> {
        self.is_class().then(|| self.extract_gc_cell())
    }
    /// Returns this value as an instance, if such is its type.
    #[inline(always)]
    pub fn as_instance(self) -> Option<SOMRef<Instance>> {
        self.is_instance().then(|| self.extract_gc_cell())
    }
    /// Returns this value as an invocable, if such is its type.
    #[inline(always)]
    pub fn as_invokable(self) -> Option<Gc<Method>> {
        self.is_invocable().then(|| self.extract_gc_cell())
    }

    // `as_*` for non pointer types

    /// Returns this value as an integer, if such is its type.
    #[inline(always)]
    pub fn as_integer(self) -> Option<i32> {
        self.is_integer()
            .then(|| (self.encoded & 0xFFFFFFFF) as i32)
    }
    /// Returns this value as a boolean, if such is its type.
    #[inline(always)]
    pub fn as_boolean(self) -> Option<bool> {
        self.is_boolean().then(|| (self.encoded & 0x1) == 0x1)
    }
    /// Returns this value as a symbol, if such is its type.
    #[inline(always)]
    pub fn as_symbol(self) -> Option<Interned> {
        self.is_symbol()
            .then(|| Interned((self.encoded & 0xFFFFFFFF) as u32))
    }

    /// Returns this value as a double, if such is its type.
    #[inline(always)]
    pub fn as_double(self) -> Option<f64> {
        self.is_double().then(|| f64::from_bits(self.encoded))
    }

    #[inline(always)]
    pub const fn new(tag: u64, value: u64) -> Self {
        // NOTE: Pointers in x86-64 use just 48 bits however are supposed to be
        //       sign extended up from the 47th bit.
        //       This means that all bits above the 47th should be the same as
        //       the 47th. When storing a pointer we thus drop the top 16 bits as
        //       we can recover it when extracting the pointer again.
        //       See also: Value::extract_pointer.
        Self {
            encoded: CANON_NAN_BITS
                | ((tag << TAG_SHIFT) & TAG_EXTRACTION)
                | (value & !TAG_EXTRACTION),
        }
    }

    /// The `nil` value.
    pub const NIL: Self = Self::new(NIL_TAG, 0);
    /// The `system` value.
    pub const SYSTEM: Self = Self::new(SYSTEM_TAG, 0);
    /// The boolean `true` value.
    pub const TRUE: Self = Self::new(BOOLEAN_TAG, 1);
    /// The boolean `false` value.
    pub const FALSE: Self = Self::new(BOOLEAN_TAG, 0);

    /// The integer `0` value.
    pub const INTEGER_ZERO: Self = Self::new(INTEGER_TAG, 0);
    /// The integer `1` value.
    pub const INTEGER_ONE: Self = Self::new(INTEGER_TAG, 1);

    /// Returns a new boolean value.
    #[inline(always)]
    pub fn new_boolean(value: bool) -> Self {
        if value {
            Self::TRUE
        } else {
            Self::FALSE
        }
    }

    /// Returns a new integer value.
    #[inline(always)]
    pub fn new_integer(value: i32) -> Self {
        Self::new(INTEGER_TAG, value as u64)
    }

    /// Returns a new double value.
    #[inline(always)]
    pub fn new_double(value: f64) -> Self {
        Self {
            encoded: if value.is_nan() {
                // To represent an actual `NaN`, we canonicalize it to `CANON_NAN_BITS`.
                CANON_NAN_BITS
            } else {
                value.to_bits()
            },
        }
    }

    /// Returns a new symbol value.
    #[inline(always)]
    pub fn new_symbol(value: Interned) -> Self {
        Self::new(SYMBOL_TAG, value.0.into())
    }

    // `new_*` for pointer types

    /// Returns a new big integer value.
    #[inline(always)]
    pub fn new_big_integer(value: &Gc<BigInt>) -> Self {
        Self::new(
            BIG_INTEGER_TAG,
            (value.as_raw_ptr().as_ptr() as usize).try_into().unwrap(),
        )
    }
    /// Returns a new string value.
    #[inline(always)]
    pub fn new_string(value: &Gc<String>) -> Self {
        Self::new(
            STRING_TAG,
            (value.as_raw_ptr().as_ptr() as usize).try_into().unwrap(),
        )
    }
    /// Returns a new array value.
    #[inline(always)]
    pub fn new_array(value: &SOMRef<Vec<Self>>) -> Self {
        Self::new(
            ARRAY_TAG,
            (value.as_raw_ptr().as_ptr() as usize).try_into().unwrap(),
        )
    }
    /// Returns a new block value.
    #[inline(always)]
    pub fn new_block(value: &Gc<Block>) -> Self {
        Self::new(
            BLOCK_TAG,
            (value.as_raw_ptr().as_ptr() as usize).try_into().unwrap(),
        )
    }
    /// Returns a new class value.
    #[inline(always)]
    pub fn new_class(value: &SOMRef<Class>) -> Self {
        Self::new(
            CLASS_TAG,
            (value.as_raw_ptr().as_ptr() as usize).try_into().unwrap(),
        )
    }
    /// Returns a new instance value.
    #[inline(always)]
    pub fn new_instance(value: &SOMRef<Instance>) -> Self {
        Self::new(
            INSTANCE_TAG,
            (value.as_raw_ptr().as_ptr() as usize).try_into().unwrap(),
        )
    }
    /// Returns a new invocable value.
    #[inline(always)]
    pub fn new_invokable(value: &Gc<Method>) -> Self {
        Self::new(
            INVOKABLE_TAG,
            (value.as_raw_ptr().as_ptr() as usize).try_into().unwrap(),
        )
    }

    #[inline(always)]
    fn extract_gc_cell<T: Trace>(self) -> Gc<T> {
        let ptr: *const GcBox<T> = self.extract_pointer::<GcBox<T>>();
        let ptr = NonNull::new(ptr as *mut _).unwrap();
        Gc::from_raw(ptr)
    }

    #[inline(always)]
    fn extract_pointer<T>(self) -> *const T {
        assert!(self.is_cell());
        self.extract_pointer_bits() as *const T
    }

    #[inline(always)]
    fn extract_pointer_bits(self) -> u64 {
        // For x86_64 the top 16 bits should be sign extending the "real" top bit (47th).
        // So first shift the top 16 bits away then using the right shift it sign extends the top 16 bits.
        (((self.encoded << 16) as i64) >> 16) as u64
    }

    /// Get the class of the current value.
    #[inline(always)]
    pub fn class(&self, universe: &Universe) -> SOMRef<Class> {
        match self.tag() {
            NIL_TAG => universe.nil_class(),
            SYSTEM_TAG => universe.system_class(),
            BOOLEAN_TAG => {
                if self.as_boolean().unwrap() {
                    universe.true_class()
                } else {
                    universe.false_class()
                }
            }
            INTEGER_TAG | BIG_INTEGER_TAG => universe.integer_class(),
            SYMBOL_TAG => universe.symbol_class(),
            STRING_TAG => universe.string_class(),
            ARRAY_TAG => universe.array_class(),
            BLOCK_TAG => self.as_block().unwrap().class(universe),
            INSTANCE_TAG => self.as_instance().unwrap().borrow().class(),
            CLASS_TAG => self.as_class().unwrap().borrow().class(),
            INVOKABLE_TAG => self.as_invokable().unwrap().class(universe),
            _ => {
                if self.is_double() {
                    universe.double_class()
                } else {
                    panic!("unknown tag");
                }
            }
        }
    }

    /// Search for a given method for this value.
    pub fn lookup_method(&self, universe: &Universe, signature: Interned) -> Option<Gc<Method>> {
        self.class(universe).borrow().lookup_method(signature)
    }

    /// Search for a local binding within this value.
    pub fn lookup_local(&self, idx: usize) -> Option<Self> {
        if let Some(instance) = self.as_instance() {
            instance.borrow().lookup_local(idx)
        } else if let Some(class) = self.as_class() {
            class.borrow().lookup_local(idx)
        } else {
            None
        }
    }

    /// Assign a value to a local binding within this value.
    pub fn assign_local(&mut self, idx: usize, value: Self) -> Option<()> {
        if let Some(instance) = self.as_instance() {
            instance.borrow_mut().assign_local(idx, value)
        } else if let Some(class) = self.as_class() {
            class.borrow_mut().assign_local(idx, value)
        } else {
            None
        }
    }

    /// Get the string representation of this value.
    pub fn to_string(&self, universe: &Universe) -> String {
        match self.tag() {
            NIL_TAG => "nil".to_string(),
            SYSTEM_TAG => "system".to_string(),
            BOOLEAN_TAG => self.as_boolean().unwrap().to_string(),
            INTEGER_TAG => self.as_integer().unwrap().to_string(),
            BIG_INTEGER_TAG => self.as_big_integer().unwrap().to_string(),
            _ if self.is_double() => self.as_double().unwrap().to_string(),
            SYMBOL_TAG => {
                let symbol = universe.lookup_symbol(self.as_symbol().unwrap());
                if symbol.chars().any(|ch| ch.is_whitespace() || ch == '\'') {
                    format!("#'{}'", symbol.replace("'", "\\'"))
                } else {
                    format!("#{}", symbol)
                }
            }
            STRING_TAG => self.as_string().unwrap().to_string(),
            ARRAY_TAG => {
                // TODO: I think we can do better here (less allocations).
                let strings: Vec<String> = self
                    .as_array()
                    .unwrap()
                    .borrow()
                    .iter()
                    .map(|value| value.to_string(universe))
                    .collect();
                format!("#({})", strings.join(" "))
            }
            BLOCK_TAG => {
                let block = self.as_block().unwrap();
                format!("instance of Block{}", block.nb_parameters() + 1)
            }
            INSTANCE_TAG => {
                let instance = self.as_instance().unwrap();
                format!(
                    "instance of {} class",
                    instance.borrow().class().borrow().name(),
                )
            }
            CLASS_TAG => self.as_class().unwrap().borrow().name().to_string(),
            INVOKABLE_TAG => {
                let invokable = self.as_invokable().unwrap();
                format!(
                    "{}>>#{}",
                    invokable.holder.borrow().name(),
                    invokable.signature(),
                )
            }
            _ => {
                panic!("unknown tag")
            }
        }
    }
}

impl Trace for Value {
    fn trace(&self) {
        if !self.is_cell() {
            return;
        }

        if let Some(value) = self.as_string() {
            value.trace();
        } else if let Some(value) = self.as_array() {
            value.trace();
        } else if let Some(value) = self.as_block() {
            value.trace();
        } else if let Some(value) = self.as_instance() {
            value.trace();
        } else if let Some(value) = self.as_class() {
            value.trace();
        } else if let Some(value) = self.as_invokable() {
            value.trace();
        } else if let Some(value) = self.as_big_integer() {
            value.trace();
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        ValueEnum::from(*self) == ValueEnum::from(*other)
    }
}

impl From<Value> for ValueEnum {
    fn from(value: Value) -> Self {
        if let Some(value) = value.as_double() {
            Self::Double(value)
        } else if value.is_nil() {
            Self::Nil
        } else if value.is_system() {
            Self::System
        } else if let Some(value) = value.as_integer() {
            Self::Integer(value)
        } else if let Some(value) = value.as_big_integer() {
            Self::BigInteger(value)
        } else if let Some(value) = value.as_boolean() {
            Self::Boolean(value)
        } else if let Some(value) = value.as_symbol() {
            Self::Symbol(value)
        } else if let Some(value) = value.as_string() {
            Self::String(value)
        } else if let Some(value) = value.as_array() {
            Self::Array(value)
        } else if let Some(value) = value.as_block() {
            Self::Block(value)
        } else if let Some(value) = value.as_instance() {
            Self::Instance(value)
        } else if let Some(value) = value.as_class() {
            Self::Class(value)
        } else if let Some(value) = value.as_invokable() {
            Self::Invokable(value)
        } else {
            todo!()
        }
    }
}

impl From<ValueEnum> for Value {
    fn from(value: ValueEnum) -> Self {
        match value {
            ValueEnum::Nil => Self::NIL,
            ValueEnum::System => Self::SYSTEM,
            ValueEnum::Boolean(value) => Self::new_boolean(value),
            ValueEnum::Integer(value) => Self::new_integer(value),
            ValueEnum::BigInteger(value) => Self::new_big_integer(&value),
            ValueEnum::Double(value) => Self::new_double(value),
            ValueEnum::Symbol(value) => Self::new_symbol(value),
            ValueEnum::String(value) => Self::new_string(&value),
            ValueEnum::Array(value) => Self::new_array(&value),
            ValueEnum::Block(value) => Self::new_block(&value),
            ValueEnum::Instance(value) => Self::new_instance(&value),
            ValueEnum::Class(value) => Self::new_class(&value),
            ValueEnum::Invokable(value) => Self::new_invokable(&value),
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        ValueEnum::from(*self).fmt(f)
    }
}

/// Represents an SOM value.
#[derive(Clone)]
pub enum ValueEnum {
    /// The **nil** value.
    Nil,
    /// The **system** value.
    System,
    /// A boolean value (**true** or **false**).
    Boolean(bool),
    /// An integer value.
    Integer(i32),
    /// A big integer value (arbitrarily big).
    BigInteger(Gc<BigInt>),
    /// An floating-point value.
    Double(f64),
    /// An interned symbol value.
    Symbol(Interned),
    /// A string value.
    String(Gc<String>),
    /// An array of values.
    Array(SOMRef<Vec<Value>>),
    /// A block value, ready to be evaluated.
    Block(Gc<Block>),
    /// A generic (non-primitive) class instance.
    Instance(SOMRef<Instance>),
    /// A bare class object.
    Class(SOMRef<Class>),
    /// A bare invokable.
    Invokable(Gc<Method>),
}

impl Trace for ValueEnum {
    #[inline]
    fn trace(&self) {
        match self {
            ValueEnum::Nil => {}
            ValueEnum::System => {}
            ValueEnum::Boolean(_) => {}
            ValueEnum::Integer(_) => {}
            ValueEnum::BigInteger(value) => {
                value.trace();
            }
            ValueEnum::Double(_) => {}
            ValueEnum::Symbol(_) => {}
            ValueEnum::String(string) => {
                string.trace();
            }
            ValueEnum::Array(array) => {
                array.trace();
            }
            ValueEnum::Block(block) => {
                block.trace();
            }
            ValueEnum::Instance(instance) => {
                instance.trace();
            }
            ValueEnum::Class(class) => {
                class.trace();
            }
            ValueEnum::Invokable(method) => {
                method.trace();
            }
        }
    }
}

impl ValueEnum {
    /// Get the class of the current value.
    #[inline(always)]
    pub fn class(&self, universe: &Universe) -> SOMRef<Class> {
        match self {
            Self::Nil => universe.nil_class(),
            Self::System => universe.system_class(),
            Self::Boolean(true) => universe.true_class(),
            Self::Boolean(false) => universe.false_class(),
            Self::Integer(_) => universe.integer_class(),
            Self::BigInteger(_) => universe.integer_class(),
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
    #[inline(always)]
    pub fn lookup_method(&self, universe: &Universe, signature: Interned) -> Option<Gc<Method>> {
        self.class(universe).borrow().lookup_method(signature)
    }

    /// Search for a local binding within this value.
    #[inline(always)]
    pub fn lookup_local(&self, idx: usize) -> Option<Self> {
        match self {
            Self::Instance(instance) => instance.borrow().lookup_local(idx).map(|it| it.into()),
            Self::Class(class) => class.borrow().lookup_local(idx).map(|it| it.into()),
            _ => None,
        }
    }

    /// Assign a value to a local binding within this value.
    #[inline(always)]
    pub fn assign_local(&mut self, idx: usize, value: Self) -> Option<()> {
        match self {
            Self::Instance(instance) => instance.borrow_mut().assign_local(idx, value.into()),
            Self::Class(class) => class.borrow_mut().assign_local(idx, value.into()),
            _ => None,
        }
    }

    /// Get the string representation of this value.
    pub fn to_string(&self, universe: &Universe) -> String {
        match self {
            Self::Nil => "nil".to_string(),
            Self::System => "system".to_string(),
            Self::Boolean(value) => value.to_string(),
            Self::Integer(value) => value.to_string(),
            Self::BigInteger(value) => value.to_string(),
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
                    .map(|value| value.to_string(universe))
                    .collect();
                format!("#({})", strings.join(" "))
            }
            Self::Block(block) => format!("instance of Block{}", block.nb_parameters() + 1),
            Self::Instance(instance) => format!(
                "instance of {} class",
                instance.borrow().class().borrow().name(),
            ),
            Self::Class(class) => class.borrow().name().to_string(),
            Self::Invokable(invokable) => format!(
                "{}>>#{}",
                invokable.holder.borrow().name(),
                invokable.signature(),
            ),
        }
    }
}

impl PartialEq for ValueEnum {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Nil, Self::Nil) | (Self::System, Self::System) => true,
            (Self::Boolean(a), Self::Boolean(b)) => a.eq(b),
            (Self::Integer(a), Self::Integer(b)) => a.eq(b),
            (Self::Integer(a), Self::Double(b)) | (Self::Double(b), Self::Integer(a)) => {
                (*a as f64).eq(b)
            }
            (Self::Double(a), Self::Double(b)) => a.eq(b),
            (Self::BigInteger(a), Self::BigInteger(b)) => a.eq(b),
            (Self::BigInteger(a), Self::Integer(b)) | (Self::Integer(b), Self::BigInteger(a)) => {
                a.as_ref().eq(&BigInt::from(*b))
            }
            (Self::Symbol(a), Self::Symbol(b)) => a.eq(b),
            (Self::String(a), Self::String(b)) => Gc::ptr_eq(a, b),
            (Self::Array(a), Self::Array(b)) => Gc::ptr_eq(a, b),
            (Self::Instance(a), Self::Instance(b)) => Gc::ptr_eq(a, b),
            (Self::Class(a), Self::Class(b)) => Gc::ptr_eq(a, b),
            (Self::Block(a), Self::Block(b)) => Gc::ptr_eq(a, b),
            (Self::Invokable(a), Self::Invokable(b)) => Gc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl fmt::Debug for ValueEnum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nil => f.debug_tuple("Nil").finish(),
            Self::System => f.debug_tuple("System").finish(),
            Self::Boolean(val) => f.debug_tuple("Boolean").field(val).finish(),
            Self::Integer(val) => f.debug_tuple("Integer").field(val).finish(),
            Self::BigInteger(val) => f.debug_tuple("BigInteger").field(val).finish(),
            Self::Double(val) => f.debug_tuple("Double").field(val).finish(),
            Self::Symbol(val) => f.debug_tuple("Symbol").field(val).finish(),
            Self::String(val) => f.debug_tuple("String").field(val.as_ref()).finish(),
            Self::Array(val) => f.debug_tuple("Array").field(val.as_ref()).finish(),
            Self::Block(val) => f.debug_tuple("Block").field(val.as_ref()).finish(),
            Self::Instance(val) => f.debug_tuple("Instance").field(&val.borrow()).finish(),
            Self::Class(val) => f.debug_tuple("Class").field(val.as_ref()).finish(),
            Self::Invokable(val) => {
                let signature = format!("{}>>#{}", val.holder.borrow().name(), val.signature());
                f.debug_tuple("Invokable").field(&signature).finish()
            }
        }
    }
}
