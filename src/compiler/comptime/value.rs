use std::{cell::RefCell, fmt::{self, Display}, rc::Rc};

use itertools::Itertools;
use num_bigint::{BigInt, BigUint};
use num_traits::{One, Signed, ToPrimitive, Zero};

use crate::compiler::{
	parser::{nodes::*, Var},
	semantic_analyzer::Type,
};

use super::{error::EvalErrorKind, rational::Rational};

/// A value known at compile time. Comptime-only types hold infinite precision, sized types hold exactly what runtime would.
#[derive(Debug, Clone)]
pub enum Value<'a> {
	/// `ty` is `ComptimeInt`, `Int`, `Usize` or `Isize`.
	Int { value: BigInt, ty: Type },
	/// The bits of a bool type. `ty` is `ComptimeBool` (0 or 1) or `Bool(bits)`.
	Bool { value: BigUint, ty: Type },
	Real(Rational),
	/// An `f32` or `f64`. An `f32` is kept rounded to `f32`.
	Float { value: f64, bits: u64 },
	Str(String),
	/// The empty tuple is VOID.
	Tuple(Vec<Value<'a>>),
	Nothing,
	Func(FuncValue<'a>),
	Extern(Rc<RefCell<Var>>),
	/// Points at a variable's slot.
	Ptr(usize),
}

#[derive(Debug, Clone)]
pub struct FuncValue<'a> {
	pub node: &'a Func,
	/// The variable the function was declared as, so runtime code can refer to it.
	pub name: Option<Rc<RefCell<Var>>>,
}

pub type OpResult<'a> = Result<Option<Value<'a>>, EvalErrorKind>;

impl<'a> Value<'a> {
	pub fn void() -> Value<'a> {
		Value::Tuple(Vec::new())
	}

	pub fn b1(value: bool) -> Value<'a> {
		Value::Bool { value: BigUint::from(value as u8), ty: Type::Bool(1) }
	}

	/// The type of this value, for the types the analyzer can name.
	pub fn ty(&self) -> Type {
		match self {
			Value::Int { ty, .. } | Value::Bool { ty, .. } => ty.clone(),
			Value::Real(_) => Type::ComptimeReal,
			Value::Float { bits, .. } => Type::Float(*bits),
			Value::Str(_) => Type::Str,
			Value::Tuple(items) => Type::Tuple(items.iter().map(Value::ty).collect()),
			Value::Nothing => Type::Nothing,
			Value::Func(_) | Value::Extern(_) | Value::Ptr(_) => Type::Unknown,
		}
	}

	/// Whether this value only exists at compile time, so it needs a sized type to reach runtime.
	pub fn is_comptime_only(&self) -> bool {
		match self {
			Value::Int { ty, .. } | Value::Bool { ty, .. } => ty.is_comptime(),
			Value::Real(_) => true,
			Value::Tuple(items) => items.iter().any(Value::is_comptime_only),
			_ => false,
		}
	}

	/// For conditions.
	pub fn as_bool(&self) -> Option<bool> {
		match self {
			Value::Bool { value, .. } => Some(!value.is_zero()),
			Value::Int { value, .. } => Some(!value.is_zero()), // a ComptimeInt casts to b1
			_ => None,
		}
	}

	pub fn as_int(&self) -> Option<&BigInt> {
		match self {
			Value::Int { value, .. } => Some(value),
			_ => None,
		}
	}
}

impl Display for Value<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Value::Int { value, ty } if ty.is_comptime() => write!(f, "{}", value),
			Value::Int { value, ty } => write!(f, "{}{}", value, ty),
			// a single bit reads as a plain bool
			Value::Bool { value, ty: Type::ComptimeBool } if value.bits() <= 1 => write!(f, "{}", !value.is_zero()),
			Value::Bool { value, ty: Type::ComptimeBool } => write!(f, "0x{:x}", value),
			Value::Bool { value, ty: Type::Bool(1) } => write!(f, "{}", !value.is_zero()),
			Value::Bool { value, ty } => write!(f, "0x{:x} ({})", value, ty),
			Value::Real(value) => write!(f, "{}", value),
			Value::Float { value, bits } => write!(f, "{:?}f{}", value, bits),
			Value::Str(value) => write!(f, "{:?}", value),
			Value::Tuple(items) if items.is_empty() => write!(f, "()"),
			Value::Tuple(items) if items.len() == 1 => write!(f, "({},)", items[0]),
			Value::Tuple(items) => write!(f, "({})", items.iter().join(", ")),
			Value::Nothing => write!(f, "NOTHING"),
			Value::Func(func) => match &func.name {
				Some(name) => write!(f, "fn {}", name.borrow().name),
				None => write!(f, "fn"),
			},
			Value::Extern(var) => write!(f, "extern {}", var.borrow().name),
			Value::Ptr(slot) => write!(f, "ptr to #{}", slot),
		}
	}
}

/// The values an int type can hold.
pub fn int_range(ty: &Type) -> Option<(BigInt, BigInt)> {
	let (signed, bits) = match ty {
		Type::Int { signed, bits } => (*signed, *bits),
		Type::Usize => (false, 64),
		Type::Isize => (true, 64),
		_ => return None,
	};
	Some(match (signed, bits) {
		(_, 0) => (BigInt::zero(), BigInt::zero()),
		(true, bits) => (-(BigInt::one() << (bits - 1)), (BigInt::one() << (bits - 1)) - 1),
		(false, bits) => (BigInt::zero(), (BigInt::one() << bits) - 1),
	})
}

/// Wraps an int into a type's range, like two's complement does at runtime.
pub fn wrap(value: BigInt, ty: &Type) -> BigInt {
	let Some((min, max)) = int_range(ty) else { return value };
	let size = &max - &min + 1;
	let mut value: BigInt = (value - &min) % &size;
	if value.is_negative() {
		value += &size;
	}
	value + min
}

fn bool_mask(bits: u64) -> BigUint {
	(BigUint::one() << bits) - 1u32
}

fn fits(value: &BigInt, ty: &Type) -> bool {
	match int_range(ty) {
		Some((min, max)) => *value >= min && *value <= max,
		None => true,
	}
}

fn overflow(value: impl Display, ty: &Type) -> EvalErrorKind {
	EvalErrorKind::DoesNotFit { value: value.to_string(), ty: ty.clone() }
}

/// Gives a comptime int the sized type it's used with, failing when it doesn't fit.
fn int_as(value: &BigInt, ty: &Type) -> Result<BigInt, EvalErrorKind> {
	if fits(value, ty) { Ok(value.clone()) } else { Err(overflow(value, ty)) }
}

fn round_float(value: f64, bits: u64) -> f64 {
	if bits == 32 { value as f32 as f64 } else { value }
}

/// The two sides of an operator, brought to one type like the analyzer does.
enum Operands {
	Int(BigInt, BigInt, Type),
	Real(Rational, Rational),
	Float(f64, f64, u64),
	Bool(BigUint, BigUint, Type),
}

fn operands(lhs: &Value, rhs: &Value) -> Result<Option<Operands>, EvalErrorKind> {
	use Value as V;
	Ok(Some(match (lhs, rhs) {
		(V::Int { value: a, ty: ta }, V::Int { value: b, ty: tb }) => match (ta.is_comptime(), tb.is_comptime()) {
			(true, true) => Operands::Int(a.clone(), b.clone(), Type::ComptimeInt),
			(true, false) => Operands::Int(int_as(a, tb)?, b.clone(), tb.clone()),
			(false, true) => Operands::Int(a.clone(), int_as(b, ta)?, ta.clone()),
			(false, false) => Operands::Int(a.clone(), b.clone(), ta.clone()),
		},
		(V::Int { value: a, ty }, V::Real(b)) if ty.is_comptime() => Operands::Real(Rational::from_int(a.clone()), b.clone()),
		(V::Real(a), V::Int { value: b, ty }) if ty.is_comptime() => Operands::Real(a.clone(), Rational::from_int(b.clone())),
		(V::Real(a), V::Real(b)) => Operands::Real(a.clone(), b.clone()),
		(V::Float { value: a, bits }, V::Float { value: b, .. }) => Operands::Float(*a, *b, *bits),
		(V::Float { value: a, bits }, comptime @ (V::Int { .. } | V::Real(_))) => Operands::Float(*a, round_float(to_f64(comptime), *bits), *bits),
		(comptime @ (V::Int { .. } | V::Real(_)), V::Float { value: b, bits }) => Operands::Float(round_float(to_f64(comptime), *bits), *b, *bits),
		(V::Bool { value: a, ty: ta }, V::Bool { value: b, ty: tb }) => {
			// unsized bits take the other side's size, if they fit
			let ty = if ta.is_comptime() { tb.clone() } else { ta.clone() };
			let a = if ta.is_comptime() { bits_of(&BigInt::from(a.clone()), &ty)? } else { a.clone() };
			let b = if tb.is_comptime() { bits_of(&BigInt::from(b.clone()), &ty)? } else { b.clone() };
			Operands::Bool(a, b, ty)
		},
		// a comptime int used as a bool type's bits
		(V::Bool { value: a, ty }, V::Int { value: b, ty: tb }) if tb.is_comptime() => Operands::Bool(a.clone(), bits_of(b, ty)?, ty.clone()),
		(V::Int { value: a, ty: ta }, V::Bool { value: b, ty }) if ta.is_comptime() => Operands::Bool(bits_of(a, ty)?, b.clone(), ty.clone()),
		_ => return Ok(None),
	}))
}

/// An int's bits in a bool type. An unsized bool takes any number of bits.
fn bits_of(value: &BigInt, ty: &Type) -> Result<BigUint, EvalErrorKind> {
	let bits = match ty {
		Type::Bool(bits) => Some(*bits),
		_ => None,
	};
	match value.to_biguint() {
		Some(value) if bits.is_none_or(|bits| value.bits() <= bits) => Ok(value),
		_ => Err(overflow(value, ty)),
	}
}

fn to_f64(value: &Value) -> f64 {
	match value {
		Value::Int { value, .. } => value.to_f64().unwrap_or(f64::NAN),
		Value::Real(value) => value.to_f64(),
		Value::Float { value, .. } => *value,
		_ => f64::NAN,
	}
}

pub fn bin_op<'a>(op: BinOpKind, lhs: &Value<'a>, rhs: &Value<'a>) -> OpResult<'a> {
	match op {
		BinOpKind::Arith(arith) => arith_op(arith, lhs, rhs),
		BinOpKind::Cmp(cmp) => cmp_op(cmp, lhs, rhs),
		BinOpKind::Bitwise(BitwiseBinOpKind::Shl | BitwiseBinOpKind::Shr) => shift_op(op, lhs, rhs),
		BinOpKind::Bitwise(bitwise) => {
			let Some(Operands::Bool(a, b, ty)) = operands(lhs, rhs)? else { return Ok(None) };
			let value = match bitwise {
				BitwiseBinOpKind::And => a & b,
				BitwiseBinOpKind::Or => a | b,
				BitwiseBinOpKind::Xor => a ^ b,
				BitwiseBinOpKind::Shl | BitwiseBinOpKind::Shr => unreachable!("handled above"),
			};
			Ok(Some(Value::Bool { value, ty }))
		},
		// on b1s, without short-circuiting. the evaluator short-circuits before getting here
		BinOpKind::Logic(logic) => {
			let (Some(a), Some(b)) = (lhs.as_bool(), rhs.as_bool()) else { return Ok(None) };
			Ok(Some(Value::b1(match logic {
				LogicBinOpKind::And => a && b,
				LogicBinOpKind::Or => a || b,
			})))
		},
	}
}

fn arith_op<'a>(op: ArithBinOp, lhs: &Value<'a>, rhs: &Value<'a>) -> OpResult<'a> {
	Ok(Some(match operands(lhs, rhs)? {
		Some(Operands::Int(a, b, ty)) => {
			let value = match op.kind {
				ArithBinOpKind::Add => a + b,
				ArithBinOpKind::Sub => a - b,
				ArithBinOpKind::Mul => a * b,
				// num_bigint rounds division toward zero, like the backends
				ArithBinOpKind::Div if b.is_zero() => return Err(EvalErrorKind::DivisionByZero),
				ArithBinOpKind::Div => a / b,
				ArithBinOpKind::Mod if b.is_zero() => return Err(EvalErrorKind::DivisionByZero),
				ArithBinOpKind::Mod => a % b,
			};
			let value = if ty.is_comptime() {
				value
			} else if op.allow_wrap {
				wrap(value, &ty)
			} else if fits(&value, &ty) {
				value
			} else {
				return Err(overflow(&value, &ty));
			};
			Value::Int { value, ty }
		},
		Some(Operands::Real(a, b)) => Value::Real(match op.kind {
			ArithBinOpKind::Add => a.add(&b),
			ArithBinOpKind::Sub => a.sub(&b),
			ArithBinOpKind::Mul => a.mul(&b),
			ArithBinOpKind::Div => a.div(&b).ok_or(EvalErrorKind::DivisionByZero)?,
			ArithBinOpKind::Mod => a.rem(&b).ok_or(EvalErrorKind::DivisionByZero)?,
		}),
		Some(Operands::Float(a, b, bits)) => {
			let value = match (bits, op.kind) {
				(32, ArithBinOpKind::Add) => (a as f32 + b as f32) as f64,
				(32, ArithBinOpKind::Sub) => (a as f32 - b as f32) as f64,
				(32, ArithBinOpKind::Mul) => (a as f32 * b as f32) as f64,
				(32, ArithBinOpKind::Div) => (a as f32 / b as f32) as f64,
				(32, ArithBinOpKind::Mod) => (a as f32 % b as f32) as f64,
				(_, ArithBinOpKind::Add) => a + b,
				(_, ArithBinOpKind::Sub) => a - b,
				(_, ArithBinOpKind::Mul) => a * b,
				(_, ArithBinOpKind::Div) => a / b,
				(_, ArithBinOpKind::Mod) => a % b,
			};
			Value::Float { value, bits }
		},
		Some(Operands::Bool(..)) | None => return Ok(None),
	}))
}

fn cmp_op<'a>(op: CmpBinOpKind, lhs: &Value<'a>, rhs: &Value<'a>) -> OpResult<'a> {
	use std::cmp::Ordering;
	let ordering = match operands(lhs, rhs)? {
		Some(Operands::Int(a, b, _)) => Some(a.cmp(&b)),
		Some(Operands::Real(a, b)) => Some(a.cmp(&b)),
		Some(Operands::Float(a, b, _)) => a.partial_cmp(&b),
		Some(Operands::Bool(a, b, _)) => Some(a.cmp(&b)),
		None => match (lhs, rhs) {
			(Value::Str(a), Value::Str(b)) => Some(a.cmp(b)),
			(Value::Ptr(a), Value::Ptr(b)) if matches!(op, CmpBinOpKind::Eq | CmpBinOpKind::Ne) => Some(a.cmp(b)),
			(Value::Tuple(a), Value::Tuple(b)) if matches!(op, CmpBinOpKind::Eq | CmpBinOpKind::Ne) => {
				let mut equal = true;
				for (a, b) in a.iter().zip(b) {
					match cmp_op(CmpBinOpKind::Eq, a, b)? {
						Some(value) => equal &= value.as_bool() == Some(true),
						None => return Ok(None),
					}
				}
				Some(if equal { Ordering::Equal } else { Ordering::Less })
			},
			_ => return Ok(None),
		},
	};
	// a comparison with NaN is false, except for `!=`
	let Some(ordering) = ordering else {
		return Ok(Some(Value::b1(op == CmpBinOpKind::Ne)));
	};
	Ok(Some(Value::b1(match op {
		CmpBinOpKind::Eq => ordering == Ordering::Equal,
		CmpBinOpKind::Ne => ordering != Ordering::Equal,
		CmpBinOpKind::Lt => ordering == Ordering::Less,
		CmpBinOpKind::Le => ordering != Ordering::Greater,
		CmpBinOpKind::Gt => ordering == Ordering::Greater,
		CmpBinOpKind::Ge => ordering != Ordering::Less,
	})))
}

fn shift_op<'a>(op: BinOpKind, lhs: &Value<'a>, rhs: &Value<'a>) -> OpResult<'a> {
	let (Value::Bool { value, ty }, Some(amount)) = (lhs, rhs.as_int()) else { return Ok(None) };
	// unsized bits have no end to shift past
	let bits = match ty {
		Type::Bool(bits) => *bits,
		_ => u64::MAX,
	};
	let amount = match amount.to_u64() {
		Some(amount) if amount < bits && amount <= u32::MAX as u64 => amount as usize,
		_ => return Err(EvalErrorKind::ShiftTooFar { amount: amount.to_string(), ty: ty.clone() }),
	};
	let value = match (op, ty) {
		(BinOpKind::Bitwise(BitwiseBinOpKind::Shl), Type::Bool(bits)) => (value << amount) & bool_mask(*bits),
		(BinOpKind::Bitwise(BitwiseBinOpKind::Shl), _) => value << amount,
		_ => value >> amount,
	};
	Ok(Some(Value::Bool { value, ty: ty.clone() }))
}

pub fn una_op<'a>(op: UnaOpKind, value: &Value<'a>) -> OpResult<'a> {
	Ok(Some(match (op, value) {
		(UnaOpKind::Identity, value @ (Value::Int { .. } | Value::Real(_) | Value::Float { .. })) => value.clone(),
		(UnaOpKind::Negate, Value::Int { value, ty }) => {
			let negated = -value;
			if !fits(&negated, ty) {
				return Err(overflow(&negated, ty));
			}
			Value::Int { value: negated, ty: ty.clone() }
		},
		(UnaOpKind::Negate, Value::Real(value)) => Value::Real(value.neg()),
		(UnaOpKind::Negate, Value::Float { value, bits }) => Value::Float { value: -value, bits: *bits },
		(UnaOpKind::Not, Value::Bool { value, ty: Type::Bool(bits) }) => Value::Bool { value: value ^ bool_mask(*bits), ty: Type::Bool(*bits) },
		// unsized bits go on forever, so only a single bit can be flipped
		(UnaOpKind::Not, Value::Bool { value, ty }) if value.bits() <= 1 => Value::Bool { value: value ^ BigUint::one(), ty: ty.clone() },
		(UnaOpKind::Not, Value::Bool { value, .. }) => return Err(EvalErrorKind::UnsizedNot { value: format!("0x{:x}", value) }),
		_ => return Ok(None),
	}))
}

/// `value#to`. `None` when the cast needs runtime, like turning a pointer into an address.
pub fn cast<'a>(value: &Value<'a>, to: &Type) -> OpResult<'a> {
	Ok(Some(match (value, to) {
		(value, to) if value.ty() == *to => value.clone(),
		(Value::Tuple(items), Type::Tuple(tys)) if items.len() == tys.len() => {
			let mut cast_items = Vec::new();
			for (item, ty) in items.iter().zip(tys) {
				match cast(item, ty)? {
					Some(item) => cast_items.push(item),
					None => return Ok(None),
				}
			}
			Value::Tuple(cast_items)
		},
		// to ints
		(Value::Int { value, ty }, to) if int_range(to).is_some() => {
			let value = if ty.is_comptime() { int_as(value, to)? } else { wrap(value.clone(), to) };
			Value::Int { value, ty: to.clone() }
		},
		(Value::Bool { value, ty: Type::ComptimeBool }, to) if int_range(to).is_some() => Value::Int { value: int_as(&BigInt::from(value.clone()), to)?, ty: to.clone() },
		(Value::Bool { value, .. }, to) if int_range(to).is_some() => Value::Int { value: wrap(BigInt::from(value.clone()), to), ty: to.clone() },
		(Value::Real(value), to) if int_range(to).is_some() => Value::Int { value: int_as(&value.trunc(), to)?, ty: to.clone() },
		(Value::Float { value: float, .. }, to) if int_range(to).is_some() => {
			let Some(exact) = Rational::from_f64(float.trunc()) else { return Err(overflow(float, to)) };
			Value::Int { value: int_as(&exact.trunc(), to)?, ty: to.clone() }
		},
		// to f32 and f64, which comptime can do exactly like runtime
		(Value::Bool { value, ty: Type::ComptimeBool }, Type::Float(bits @ (32 | 64))) => {
			let value = bits_of(&BigInt::from(value.clone()), &Type::Bool(*bits))?;
			match bits {
				32 => Value::Float { value: f32::from_bits(value.to_u32().unwrap_or(0)) as f64, bits: 32 },
				_ => Value::Float { value: f64::from_bits(value.to_u64().unwrap_or(0)), bits: 64 },
			}
		},
		(Value::Bool { value, ty: Type::Bool(32) }, Type::Float(32)) => Value::Float { value: f32::from_bits(value.to_u32().unwrap_or(0)) as f64, bits: 32 },
		(Value::Bool { value, ty: Type::Bool(64) }, Type::Float(64)) => Value::Float { value: f64::from_bits(value.to_u64().unwrap_or(0)), bits: 64 },
		(value @ (Value::Int { .. } | Value::Real(_) | Value::Float { .. }), Type::Float(bits @ (32 | 64))) => Value::Float { value: round_float(to_f64(value), *bits), bits: *bits },
		// to bool types, which hold the bits
		(Value::Int { value, ty }, Type::Bool(bits)) if ty.is_comptime() => Value::Bool { value: bits_of(value, to)?, ty: Type::Bool(*bits) },
		(Value::Int { value, .. }, Type::Bool(bits)) => {
			let unsigned = wrap(value.clone(), &Type::Int { signed: false, bits: *bits });
			Value::Bool { value: unsigned.to_biguint().unwrap_or_default(), ty: Type::Bool(*bits) }
		},
		(Value::Float { value, bits: 32 }, Type::Bool(32)) => Value::Bool { value: BigUint::from((*value as f32).to_bits()), ty: Type::Bool(32) },
		(Value::Float { value, bits: 64 }, Type::Bool(64)) => Value::Bool { value: BigUint::from(value.to_bits()), ty: Type::Bool(64) },
		(Value::Bool { value, ty: Type::ComptimeBool }, Type::Bool(bits)) => Value::Bool { value: bits_of(&BigInt::from(value.clone()), to)?, ty: Type::Bool(*bits) },
		(Value::Bool { value, .. }, Type::Bool(bits)) => Value::Bool { value: value & bool_mask(*bits), ty: Type::Bool(*bits) },
		_ => return Ok(None),
	}))
}

/// Brings a value to the type of what it's assigned to, like a comptime int stored in a u8.
pub fn implicit_cast<'a>(value: &Value<'a>, to: &Type) -> OpResult<'a> {
	if !value.is_comptime_only() || to.is_comptime() || matches!(to, Type::Unknown) {
		return Ok(Some(value.clone()));
	}
	cast(value, to)
}
