use std::fmt::{self, Display};

use thiserror::Error;

use crate::compiler::{semantic_analyzer::Type, source::{self, error_gen}};

#[derive(Debug, Clone, PartialEq, Error)]
pub enum EvalErrorKind {
	#[error("{value} doesn't fit in {ty}")]
	DoesNotFit { value: String, ty: Type },
	#[error("Division by zero")]
	DivisionByZero,
	#[error("Can't shift {ty} by {amount} bits")]
	ShiftTooFar { amount: String, ty: Type },
	#[error("'{name}' is {ty} at compile time, but it's needed at runtime, where values need a size. Give it a type with a cast like '0#i64'")]
	NeedsRuntimeType { name: String, ty: Type },
	#[error("The comptime value {value} is needed at runtime, but doesn't fit in the default i64. Give it a type with a cast")]
	TooLargeForRuntime { value: String },
	#[error("The unsized bits {value} are needed at runtime. Give them a size with a cast like '0xff#b8'")]
	UnsizedAtRuntime { value: String },
	#[error("Can't flip the bits of {value}, which has no size. Give it one with a cast like '0xff#b8'")]
	UnsizedNot { value: String },
}

#[derive(Debug, Clone)]
pub struct EvalError {
	pub kind: EvalErrorKind,
	pub range: source::Range,
}

impl EvalError {
	pub fn to_meta<'a>(&'a self, document: &'a source::DocumentMeta<'a>) -> EvalErrorMeta<'a> {
		EvalErrorMeta { error: self, document }
	}
}

pub struct EvalErrorMeta<'a> {
	error: &'a EvalError,
	document: &'a source::DocumentMeta<'a>,
}

impl Display for EvalErrorMeta<'_> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		writeln!(f, "{}", self.error.kind)?;
		write!(f, "{}", error_gen::generate_error_line(self.error.range.with_meta(self.document)))
	}
}
