use std::{error::Error, fmt::Display};

use itertools::Itertools;

use crate::compiler::lexer::TokenTag;

#[derive(Debug)]
pub enum RuleErrorKind {
	StreamExhausted,
	UnexpectedToken(TokenTag),
	ExpectedOneOf {
		expected: Vec<String>,
		found: TokenTag,
	},
	PatternInExpr,
	ExprInPattern,
}

impl Display for RuleErrorKind {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			RuleErrorKind::StreamExhausted => write!(f, "Stream was exhausted"),
			RuleErrorKind::UnexpectedToken(token_tag) => write!(f, "Unexpected token '{}'", token_tag),
			RuleErrorKind::ExpectedOneOf { expected, found } => write!(f, "Unexpected token '{}', expected one of {}", found, expected.iter().join(", ")),
			RuleErrorKind::PatternInExpr => write!(f, "Pattern can't be used as an expression"),
			RuleErrorKind::ExprInPattern => write!(f, "Expression can't be used as a pattern"),
		}
	}
}

impl Error for RuleErrorKind { }
