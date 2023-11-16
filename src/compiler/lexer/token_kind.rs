use szu::{tag_enum_helper, tag_enum, replace_macro_arg};

tag_enum!(
	pub enum (
		#[derive(Debug, PartialEq, Eq, Clone, strum::AsRefStr)] TokenKind,
		#[derive(Debug, PartialEq, Eq, Clone, Copy)] TokenKindTag,
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
		Ipt,
		Yield,
		
		IntLit(u64),
		StrLit(String),
		Identifier(String),

		KeywordGet,
		KeywordSet,
		KeywordMov,
		KeywordAdd,
		KeywordSub,
		KeywordMul,
		KeywordDiv,
		KeywordLbl,
		KeywordJmp,
		KeywordBeqz,
		KeywordBltz,
	}
);
