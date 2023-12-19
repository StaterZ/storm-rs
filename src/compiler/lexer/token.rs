use crate::compiler::source;

use super::{TokenKind, TokenMeta};

#[derive(Debug)]
pub struct Token {
	pub kind: TokenKind,
	pub range: source::Range,
}

impl Token {
	pub fn with_meta<'a>(&'a self, file: &'a source::SourceFile) -> TokenMeta<'a> {
		TokenMeta {
			token: self,
			file,
		}
	}
}
