use logos::Logos;

#[derive(Debug, PartialEq, Clone, Default)]
pub enum LexingError {
	#[default] Geh,
}

#[derive(Logos, logos_display::Debug, logos_display::Display, PartialEq, Clone)]
#[logos(
	error = LexingError,
	skip r"[ \t\n\f]+",
	skip r"//[^/][^\n]*",
	skip r"/\*(?:[^*]|\*[^/])*\*/",
)]
pub enum Token {
	#[token("let")]
	Let,
	#[token("loop")]
	Loop,
	#[token("while")]
	While,
	#[token("for")]
	For,
	#[token("if")]
	If,
	#[token("else")]
	Else,
	// #[token("match")]
	// Match,

	#[token("ret")]
	Ret,
	#[token("break")]
	Break,
	#[token("continue")]
	Continue,
	#[token("unreachable")]
	Unreachable,

	// #[token("pub")]
	// Pub,
	// #[token("mut")]
	// Mut,
	// #[token("struct")]
	// Struct,
	// #[token("fn")]
	// Fn,

	// Modern way of allowing identifiers, read: https://unicode.org/reports/tr31/
	#[regex(r"[\p{XID_Start}_]\p{XID_Continue}*", |lex| lex.slice().to_string())]
	Identifier(String),
	#[regex(r"\d+", |lex| lex.slice().parse::<u128>().unwrap(), priority = 2)]
	Integer(u128),
	#[regex(r#""(?:[^"]|\\")*""#, |lex| {
		let slice = lex.slice();
		let len = slice.len();
		unescaper::unescape(&slice[1..(len-1)]).expect("failed to unescape string")
	})]
	String(String),
	
	#[token("(")]
	LParen,
	#[token(")")]
	RParen,
	#[token("{")]
	LBrace,
	#[token("}")]
	RBrace,
	#[token("[")]
	LBracket,
	#[token("]")]
	RBracket,

	#[token("=")]
	Assign,
	#[token(";")]
	Semicolon,
	#[token(":")]
	Colon,
	#[token(",")]
	Coma,
	#[token(".")]
	Dot,

	#[token("<")]
	Lt,
	#[token("<=")]
	Le,
	#[token(">")]
	Gt,
	#[token(">=")]
	Ge,
	#[token("==")]
	Eq,
	#[token("!=")]
	Ne,

	#[token("+")]
	Add,
	#[token("-")]
	Sub,
	#[token("*")]
	Mul,
	#[token("/")]
	Div,
	// #[token("%")]
	// Rem,
	#[token("&&")]
	And,
	#[token("||")]
	Or,
	#[token("!")]
	Not,
}
