use std::fmt::Display;

use crate::compiler::source;

use super::Token;

pub struct TokenMeta<'a, 'b> {
	pub token: &'a Token,
	pub document: &'b source::DocumentMeta<'b>,
}

impl<'a, 'b> Display for TokenMeta<'a, 'b> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{} -> {:?}", self.token.range.with_meta(self.document), self.token.kind)
	}
}
