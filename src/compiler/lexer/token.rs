use std::fmt::Display;

use szu::tag_enum;

tag_enum!(
	pub enum (
		#[derive(Debug, PartialEq, Eq, Clone, strum::AsRefStr, enum_as_inner::EnumAsInner)] Token,
		#[derive(Debug, PartialEq, Eq, Clone, Copy, strum::AsRefStr)] TokenTag,
	) {
		Space,
		NewLine,
		Comment,
		MultilineComment,
		
		Dot,
		Comma,
		Colon,
		Semicolon,
		Eof,

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
		Ampersand,
		Bar,
		
		Equals,
		Eq,
		Ne,
		Lt,
		Le,
		Gt,
		Ge,
		And,
		Or,
		Bang,

		Let,
		Mut,
		Discard,

		Return,
		Break,
		Continue,
		Unreachable,

		Plex,

		Fn,
		Loop,
		While,
		For,
		In,
		If,
		Else,
		
		//temp?
		Ipt,
		Yield,
		Hash,
		
		True,
		False,
		IntLit(u64),
		StrLit(String),
		Identifier(String),
	}
);

impl Display for TokenTag {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.as_ref())
	}
}
