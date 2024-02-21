use std::fmt::Display;

use super::super::source;

use super::Token;

pub struct TokenMeta<'a, 'b> {
	pub token: &'a Token,
	pub document: &'b source::Document,
}

impl<'a, 'b> Display for TokenMeta<'a, 'b> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{} -> {:?}", self.token.range.clone().to_meta(self.document), self.token.kind)
	}
}
