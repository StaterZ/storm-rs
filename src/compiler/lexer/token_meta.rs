use std::fmt::Display;

use crate::compiler::source::SourceFile;

use super::Token;

pub struct TokenMeta<'a> {
	pub token: &'a Token,
	pub file: &'a SourceFile,
}

impl<'a> Display for TokenMeta<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{} -> {:?}", self.token.source.clone().to_meta(self.file), self.token.kind)
	}
}
