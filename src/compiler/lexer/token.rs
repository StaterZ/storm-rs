use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Clone, strum::AsRefStr, strum::EnumDiscriminants, enum_as_inner::EnumAsInner)]
#[strum_discriminants(name(TokenTag), derive(strum::AsRefStr))]
pub enum Token {
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

impl Token {
	pub fn is_whitespace(&self) -> bool {
		matches!(self,
			| Token::Space
			| Token::NewLine
			| Token::Comment
			| Token::MultilineComment)
	}
}

impl Display for TokenTag {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", self.as_ref())
	}
}

impl PartialEq<TokenTag> for crate::compiler::source::Sourced<Token> {
	fn eq(&self, other: &TokenTag) -> bool {
		&**self == other
	}
}
impl PartialEq<TokenTag> for &crate::compiler::source::Sourced<Token> {
	fn eq(&self, other: &TokenTag) -> bool {
		&***self == other
	}
}
