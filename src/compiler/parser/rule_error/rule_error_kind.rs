use std::{error::Error, fmt::Display};

use crate::compiler::lexer::TokenTag;

#[derive(Debug)]
pub enum RuleErrorKind {
	StreamExhausted,
	UnexpectedToken(TokenTag),
	ExpectedToken {
		expected: TokenTag,
		found: TokenTag,
	},
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

impl Error for RuleErrorKind { }
