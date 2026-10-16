use std::fmt::Display;

use logos::{Logos, Skip};
use unescaper::unescape;

use crate::compiler::source::line_break::find_line_break;

use super::{lexer_error::LexerErrorKind, literal::{lex_num, NumLit}};

#[derive(Logos, logos_display::Debug, logos_display::Display, PartialEq, Eq, Clone, strum::AsRefStr, strum::EnumDiscriminants, enum_as_inner::EnumAsInner)]
#[strum_discriminants(name(TokenTag), derive(strum::AsRefStr))]
#[logos(
	error = LexerErrorKind,
	skip r"[ \t\r\n\f]+",
)]
pub enum Token {
	// Comments are skipped by their callbacks. Logos never produces these two variants.
	#[token("//", line_comment)]
	LineComment,
	#[token("/*", block_comment)]
	BlockComment,

	#[token(".")]
	Dot,
	#[token("...")]
	Ellipsis,
	#[token(".*")]
	Deref,
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
	#[token("->")]
	Arrow,
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
	#[token("extern")]
	Extern,

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

	//temp?
	#[token("ipt")]
	Ipt,
	#[token("yield")]
	Yield,

	#[token("true")]
	True,
	#[token("false")]
	False,

	#[regex(r#""(?:[^"\\]|\\.)*""#, |lex| {
		let s = lex.slice();
		unescape(&s[1..s.len() - 1]).map_err(|e| LexerErrorKind::InvalidEscapeSequence(e.to_string()))
	})]
	StrLit(String),
	#[regex(r"[0-9][0-9a-zA-Z_]*(\.[0-9][0-9a-zA-Z_]*)?", lex_num)]
	NumLit(NumLit),
	// Unicode identifiers: https://unicode.org/reports/tr31/
	#[regex(r"[\p{XID_Start}_]\p{XID_Continue}*", |lex| lex.slice().to_string())]
	#[regex(r#"@"(?:[^"\\]|\\.)*""#, |lex| {
		let s = lex.slice();
		unescape(&s[2..s.len() - 1]).map_err(|e| LexerErrorKind::InvalidEscapeSequence(e.to_string()))
	})]
	Identifier(String),
}

fn line_comment(lex: &mut logos::Lexer<Token>) -> Skip {
	let rest = lex.remainder();
	let len = find_line_break(rest).map_or(rest.len(), |(index, _)| index);
	lex.bump(len);
	Skip
}

fn block_comment(lex: &mut logos::Lexer<Token>) -> Result<Skip, LexerErrorKind> {
	let mut chars = lex.remainder().char_indices().peekable();
	let mut depth = 1usize;
	while let Some((_, c)) = chars.next() {
		match (c, chars.peek()) {
			('/', Some((_, '*'))) => {
				depth += 1;
				chars.next();
			},
			('*', Some(&(i, '/'))) => {
				depth -= 1;
				chars.next();
				if depth == 0 {
					lex.bump(i + '/'.len_utf8());
					return Ok(Skip);
				}
			},
			_ => { },
		}
	}
	Err(LexerErrorKind::MultilineCommentDelimiterNotClosed)
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
