use crate::compiler::source;

use super::{TokenKind, TokenMeta};

#[derive(Debug)]
pub struct Token {
	pub kind: TokenKind,
	pub range: source::Range,
}

impl Token {
	pub fn with_meta<'a, 'b>(&'a self, document: &'b source::DocumentMeta<'b>) -> TokenMeta<'a, 'b> {
		TokenMeta {
			token: self,
			document,
		}
	}
}
