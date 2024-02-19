use std::{error::Error, fmt::Display};

use super::{
	Node,
	ResultSH,
	super::lexer::TokenKindTag,
	super::source,
};

#[derive(Debug)]
pub enum RuleErrorKind {
	StreamExhausted,
	UnexpectedToken(TokenKindTag),
	ExpectedToken {
		expected: TokenKindTag,
		found: TokenKindTag,
	},
}

#[derive(Debug)]
pub struct RuleError {
	pub(super) kind: RuleErrorKind,
	pub(super) source_range: Option<source::Range>,
}

#[derive(Debug)]
pub struct RuleErrorMeta<'a> {
	error: RuleError,
	document: &'a source::Document,
}

impl Display for RuleErrorKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			RuleErrorKind::StreamExhausted => write!(f, "Stream was exhausted"),
			RuleErrorKind::UnexpectedToken(token_tag) => write!(f, "Unexpected token '{}'", token_tag),
			RuleErrorKind::ExpectedToken { expected, found } => write!(f, "Expected token '{}', found '{}'", expected, found),
		}
	}
}

impl RuleError {
	pub fn to_meta(self, document: &source::Document) -> RuleErrorMeta {
		RuleErrorMeta {
			error: self,
			document,
		}
	}
}

impl<'a> Display for RuleErrorMeta<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		writeln!(f, "{}", self.error.kind)?;
		if let Some(source_range) = self.error.source_range {
			write!(f, "{}", source::error_gen::generate_error_line(source_range.to_meta(&self.document)))?;
		} else {
			write!(f, "No source location >:/")?;
		}
		Ok(())
	}
}

impl Error for RuleErrorKind { }
impl<'a> Error for RuleErrorMeta<'a> { }

pub type RuleResult = ResultSH<Node, RuleErrorKind>;

#[macro_export]
macro_rules! shed_errors {
	($expr:expr) => {
		match $expr {
			Ok(Ok(ok)) => ok,
			Ok(Err(err)) => return Ok(Err(err)),
			Err(err) => return Err(err),
		}
	};
}
