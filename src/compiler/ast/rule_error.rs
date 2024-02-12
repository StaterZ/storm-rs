use std::{error::Error, fmt::Display};

use crate::compiler::lexer::TokenKindTag;

use super::Node;

#[derive(Debug)]
pub enum RuleError {
	StreamExhausted,
	UnexpectedToken(TokenKindTag),
	ExpectedToken {
		expected: TokenKindTag,
		found: TokenKindTag,
	},
	Temp(String),
}

impl Display for RuleError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			RuleError::StreamExhausted => write!(f, "Stream was exhauted"),
			RuleError::UnexpectedToken(token_tag) => write!(f, "Unexpected token '{}'", token_tag),
			RuleError::ExpectedToken { expected, found } => write!(f, "Expected token '{}', found '{}'", expected, found),
			RuleError::Temp(msg) => write!(f, "TempError: '{}'", msg),
		}
	}
}

impl Error for RuleError {
	
}

pub type RuleResult = Result<Result<Node, RuleError>, RuleError>;

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
