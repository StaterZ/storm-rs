use std::fmt::{self, Display};

use itertools::Itertools;

/// The bits of a `usize`/`isize`. Everything targets 64 bit for now.
pub const POINTER_BITS: u64 = 64;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
	// comptime-only, these implicitly cast to runtime types
	IntLit,
	RealLit,
	BoolLit,

	Int { signed: bool, bits: u64 },
	Usize,
	Isize,
	Float(u64),
	Bool(u64),
	Str,
	Ptr(Box<Type>),
	/// The empty tuple is VOID.
	Tuple(Vec<Type>),
	Func { arg: Box<Type>, ret: Box<Type> },

	/// What a `;` turns a VOID into. The only value that doesn't need to be caught.
	Nothing,
	/// The type of `ret`, `break` and friends, which never produce a value. Casts to anything.
	Never,
	/// Not known yet, like an unannotated function argument. Anything goes.
	Unknown,
}

impl Type {
	pub const VOID: Type = Type::Tuple(Vec::new());

	pub fn is_comptime(&self) -> bool {
		matches!(self, Type::IntLit | Type::RealLit | Type::BoolLit)
	}

	pub fn is_int(&self) -> bool {
		matches!(self, Type::Int { .. } | Type::Usize | Type::Isize | Type::IntLit)
	}

	pub fn is_numeric(&self) -> bool {
		self.is_int() || matches!(self, Type::Float(_) | Type::RealLit)
	}

	pub fn is_bool(&self) -> bool {
		matches!(self, Type::Bool(_) | Type::BoolLit)
	}

	/// Whether a value of this type may be dropped without being caught.
	pub fn is_discardable(&self) -> bool {
		*self == Type::VOID || matches!(self, Type::Nothing | Type::Never | Type::Unknown)
	}

	/// The number of bits in memory, for runtime number and bool types.
	pub fn bits(&self) -> Option<u64> {
		match self {
			Type::Int { bits, .. } | Type::Float(bits) | Type::Bool(bits) => Some(*bits),
			Type::Usize | Type::Isize => Some(POINTER_BITS),
			_ => None,
		}
	}

	/// Whether a `from` value can be used where a `to` is expected without a cast.
	pub fn implicitly_casts_to(&self, to: &Type) -> bool {
		match (self, to) {
			_ if self == to => true,
			(Type::Unknown | Type::Never, _) | (_, Type::Unknown) => true,
			(Type::IntLit, to) => to.is_numeric() || to.is_bool(),
			(Type::RealLit, Type::Float(_)) => true,
			(Type::BoolLit, Type::Bool(_)) => true,
			(Type::Tuple(from), Type::Tuple(to)) => from.len() == to.len() && from.iter().zip(to).all(|(from, to)| from.implicitly_casts_to(to)),
			_ => false,
		}
	}

	/// Whether `self#to` is allowed.
	pub fn explicitly_casts_to(&self, to: &Type) -> bool {
		if self.implicitly_casts_to(to) {
			return true;
		}
		match (self, to) {
			// numbers convert between each other by value
			(from, to) if from.is_numeric() && to.is_numeric() && !to.is_comptime() => true,
			// bools reinterpret the bits of a number, so the sizes have to match
			(from, Type::Bool(bits)) | (Type::Bool(bits), from) if from.is_numeric() => from.bits().is_none_or(|from_bits| from_bits == *bits),
			(Type::Bool(_), Type::Bool(_)) => true,
			(Type::Ptr(_), Type::Usize) | (Type::Usize, Type::Ptr(_)) | (Type::Ptr(_), Type::Ptr(_)) => true,
			(Type::Tuple(from), Type::Tuple(to)) => from.len() == to.len() && from.iter().zip(to).all(|(from, to)| from.explicitly_casts_to(to)),
			_ => false,
		}
	}

	/// The type both sides of a binary operator or both branches of an `if` become.
	pub fn unify(&self, other: &Type) -> Option<Type> {
		match (self, other) {
			_ if self == other => Some(self.clone()),
			(Type::Unknown, _) | (_, Type::Unknown) => Some(Type::Unknown),
			(Type::Never, other) | (other, Type::Never) => Some(other.clone()),
			(Type::IntLit, Type::RealLit) | (Type::RealLit, Type::IntLit) => Some(Type::RealLit),
			(comptime, runtime) | (runtime, comptime) if comptime.is_comptime() && comptime.implicitly_casts_to(runtime) => Some(runtime.clone()),
			(Type::Tuple(a), Type::Tuple(b)) if a.len() == b.len() => a.iter().zip(b).map(|(a, b)| a.unify(b)).collect::<Option<_>>().map(Type::Tuple),
			_ => None,
		}
	}

	/// The runtime type a name like `i32`, `u7`, `f64`, `b1`, `usize` or `str` refers to.
	pub fn from_name(name: &str) -> Option<Type> {
		match name {
			"usize" => return Some(Type::Usize),
			"isize" => return Some(Type::Isize),
			"str" => return Some(Type::Str),
			_ => { },
		}
		let (kind, bits) = name.split_at_checked(1)?;
		if bits.is_empty() || !bits.chars().all(|c| c.is_ascii_digit()) || (bits.len() > 1 && bits.starts_with('0')) {
			return None;
		}
		let bits = bits.parse::<u64>().ok()?;
		match kind {
			"i" => Some(Type::Int { signed: true, bits }),
			"u" => Some(Type::Int { signed: false, bits }),
			"f" if bits > 0 => Some(Type::Float(bits)),
			"b" if bits > 0 => Some(Type::Bool(bits)),
			_ => None,
		}
	}
}

impl Display for Type {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Type::IntLit => write!(f, "IntLit"),
			Type::RealLit => write!(f, "RealLit"),
			Type::BoolLit => write!(f, "BoolLit"),
			Type::Int { signed: true, bits } => write!(f, "i{}", bits),
			Type::Int { signed: false, bits } => write!(f, "u{}", bits),
			Type::Usize => write!(f, "usize"),
			Type::Isize => write!(f, "isize"),
			Type::Float(bits) => write!(f, "f{}", bits),
			Type::Bool(bits) => write!(f, "b{}", bits),
			Type::Str => write!(f, "str"),
			Type::Ptr(ty) => write!(f, "{}&", ty),
			Type::Tuple(items) if items.is_empty() => write!(f, "VOID"),
			Type::Tuple(items) if items.len() == 1 => write!(f, "({},)", items[0]),
			Type::Tuple(items) => write!(f, "({})", items.iter().join(", ")),
			Type::Func { arg, ret } => write!(f, "fn{} -> {}", match **arg {
				Type::Tuple(_) => arg.to_string(),
				_ => format!("({})", arg),
			}, ret),
			Type::Nothing => write!(f, "NOTHING"),
			Type::Never => write!(f, "never"),
			Type::Unknown => write!(f, "unknown"),
		}
	}
}
