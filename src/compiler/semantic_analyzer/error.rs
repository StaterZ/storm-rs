use std::fmt::{self, Display};

use thiserror::Error;

use crate::compiler::source::{self, error_gen};

use super::Type;

#[derive(Debug, Clone, PartialEq, Error)]
pub enum SemErrorKind {
	#[error("Found mut outside let")]
	MutWithoutLet,
	#[error("Found mut inside mut")]
	DoubleMut,
	#[error("Found let inside let")]
	DoubleLet,
	#[error("Symbol '{0}' isn't defined")]
	UndefinedSymbol(String),
	#[error("Symbol '{0}' assigned but isn't mutable")]
	AssignImmutableSymbol(String),
	#[error("Compound assignments like '+=' need an existing target, not a declaration or '_'")]
	CompoundAssignDeclaration,

	#[error("Unknown type '{0}'")]
	UnknownType(String),
	#[error("Expected {expected}, found {found}")]
	Mismatch { expected: Type, found: Type },
	#[error("Can't cast {from} to {to}")]
	InvalidCast { from: Type, to: Type },
	#[error("'{op}' can't be used on {lhs} and {rhs}")]
	InvalidBinOp { op: String, lhs: Type, rhs: Type },
	#[error("'{op}' can't be used on {ty}")]
	InvalidUnaOp { op: String, ty: Type },
	#[error("Bit shifts work on bool types like b8, found {0}. Cast it with '#'")]
	ShiftNonBool(Type),
	#[error("A value of type {0} has to be caught, by binding it with 'let _ = ...' or cascading it out of the block")]
	UncaughtValue(Type),
	#[error("Mutable bindings need a runtime type, found the comptime type {0}. Give it one with a cast like '0#i64'")]
	MutComptime(Type),
	#[error("Can't destructure {found} into a tuple of {len}")]
	DestructureMismatch { found: Type, len: usize },
	#[error("Can't call {0}")]
	NotCallable(Type),
	#[error("{0} has no fields")]
	NoFields(Type),
	#[error("Can't dereference {0}")]
	NotAPointer(Type),
	#[error("Can't break with a value out of a 'while' or 'for', found {0}")]
	BreakValueInWhile(Type),
	#[error("'break' and 'continue' have to be inside a loop")]
	BreakOutsideLoop,
	#[error("Literal {value} doesn't fit in {ty}")]
	LiteralDoesNotFit { value: String, ty: Type },
}

#[derive(Debug, Clone)]
pub struct SemError {
	pub kind: SemErrorKind,
	pub range: source::Range,
}

impl SemError {
	pub fn to_meta<'a>(&'a self, document: &'a source::DocumentMeta<'a>) -> SemErrorMeta<'a> {
		SemErrorMeta { error: self, document }
	}
}

pub struct SemErrorMeta<'a> {
	error: &'a SemError,
	document: &'a source::DocumentMeta<'a>,
}

impl Display for SemErrorMeta<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		writeln!(f, "{}", self.error.kind)?;
		write!(f, "{}", error_gen::generate_error_line(self.error.range.with_meta(self.document)))
	}
}
