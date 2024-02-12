use std::fmt::Display;

use szu::tag_enum;

tag_enum!(
	pub enum (
		#[derive(Debug, PartialEq, Eq, Clone, strum::AsRefStr)] TokenKind,
		#[derive(Debug, PartialEq, Eq, Clone, Copy, strum::AsRefStr)] TokenKindTag,
	) {
		Space,
		NewLine,
		Comment,
		MultilineComment,

		Dot,
		Comma,
		Semicolon,

		LParen,
		RParen,
		LBracket,
		RBracket,
		LBrace,
		RBrace,

		Plus,
		Dash,
		Star,
		Slash,
		Percent,
		LShift,
		RShift,
		
		Equals,
		LessThan,
		GreaterThan,
		Equality,
		Eof,

		Let,
		If,
		Else,
		Give,
		Return,

		Ipt,
		Yield,
		
		IntLit(u64),
		StrLit(String),
		Identifier(String),
	}
);

impl Display for TokenKindTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}
