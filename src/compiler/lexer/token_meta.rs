use std::fmt::Display;

use crate::compiler::source;

use super::Token;

pub struct TokenMeta<'a> {
	pub token: &'a Token,
	pub document: &'a source::Document,
}

impl<'a> Display for TokenMeta<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{} -> {:?}", self.token.range.clone().to_meta(self.document), self.token.kind)
	}
}
