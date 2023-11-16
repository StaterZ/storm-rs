use crate::compiler::source::{SourceFile, SourceRange};

use super::{TokenKind, TokenMeta};

#[derive(Debug)]
pub struct Token {
	pub kind: TokenKind,
	pub source: SourceRange,
}

impl Token {
	pub fn with_meta<'a>(&'a self, file: &'a SourceFile) -> TokenMeta<'a> {
		TokenMeta {
			token: self,
			file,
		}
	}
}
