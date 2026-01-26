use logos::Logos;

use super::LexicalErrorKind;

#[derive(Logos, logos_display::Debug, logos_display::Display, PartialEq, Eq, Clone)]
#[logos(
	error = LexicalErrorKind,
	skip r"[ \t\r\n\f]+",
	skip r"//[^\n]*",
	skip r"/\*(?:[^*]|\*[^/])*\*/",
)]
pub enum Token {
	#[token(".")]
	Dot,
	#[token(",")]
	Comma,
	#[token(":")]
	Colon,
	#[token(";")]
	Semicolon,
	
	#[token("(")]
	LParen,
	#[token(")")]
	RParen,
	#[token("[")]
	LBracket,
	#[token("]")]
	RBracket,
	#[token("{")]
	LBrace,
	#[token("}")]
	RBrace,

	#[token("+")]
	Plus,
	#[token("-")]
	Dash,
	#[token("*")]
	Star,
	#[token("/")]
	Slash,
	#[token("%")]
	Percent,
	#[token("#")]
	Hash,
	#[token("<<")]
	LShift,
	#[token(">>")]
	RShift,
	#[token("&")]
	Ampersand,
	#[token("|")]
	Bar,

	#[token("=")]
	Equals,
	#[token("==")]
	Eq,
	#[token("!=")]
	Ne,
	#[token("<")]
	Lt,
	#[token("<=")]
	Le,
	#[token(">")]
	Gt,
	#[token(">=")]
	Ge,
	#[token("&&")]
	And,
	#[token("||")]
	Or,
	#[token("!")]
	Bang,

	#[token("let")]
	Let,
	#[token("mut")]
	Mut,
	#[token("_", priority = 3)]
	Discard,

	#[token("ret")]
	Return,
	#[token("break")]
	Break,
	#[token("continue")]
	Continue,
	#[token("unreachable")]
	Unreachable,

	#[token("plex")]
	Plex,
	#[token("fn")]
	Fn,
	#[token("pub")]
	Pub,

	#[token("loop")]
	Loop,
	#[token("while")]
	While,
	#[token("for")]
	For,
	#[token("in")]
	In,
	#[token("if")]
	If,
	#[token("else")]
	Else,
	#[token("match")]
	Match,

	#[token("true")]
	True,
	#[token("false")]
	False,
	#[regex(r#""(?:[^"]|\\")*""#, |lex| {
		let slice = lex.slice();
		let len = slice.len();
		unescaper::unescape(&slice[1..(len-1)]).map_err(|_err| LexicalErrorKind::BadString)
	})]
	String(String),
	#[regex(r"\d+", |lex| lex.slice().parse::<u64>().map_err(|err| LexicalErrorKind::BadInteger(err)), priority = 2)]
	Integer(u64),
	#[regex(r"\d+\.\d+", |lex| lex.slice().parse::<u64>().map_err(|err| LexicalErrorKind::BadInteger(err)), priority = 2)]
	Real(u64),
	#[regex(r"[\p{XID_Start}_]\p{XID_Continue}*", |lex| lex.slice().to_string())] // Modern way of allowing identifiers, read: https://unicode.org/reports/tr31/
	Identifier(String),
}
