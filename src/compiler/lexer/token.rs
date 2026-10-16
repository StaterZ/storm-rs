use std::fmt::Display;

use logos::{Logos, Skip};
use unescaper::unescape;

use super::lexer_error::LexerErrorKind;

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
	#[regex(r"[0-9][0-9a-zA-Z_]*", lex_int)]
	IntLit(u64),
	// Unicode identifiers: https://unicode.org/reports/tr31/
	#[regex(r"[\p{XID_Start}_]\p{XID_Continue}*", |lex| lex.slice().to_string())]
	#[regex(r#"@"(?:[^"\\]|\\.)*""#, |lex| {
		let s = lex.slice();
		unescape(&s[2..s.len() - 1]).map_err(|e| LexerErrorKind::InvalidEscapeSequence(e.to_string()))
	})]
	Identifier(String),

	// TODO: remove me
	// Logos never matches this. The `lex` function adds it at the end of the stream.
	Eof,
}

fn line_comment(lex: &mut logos::Lexer<Token>) -> Skip {
	let rest = lex.remainder();
	let len = rest.find(['\r', '\n']).unwrap_or(rest.len());
	lex.bump(len);
	Skip
}

fn block_comment(lex: &mut logos::Lexer<Token>) -> Result<Skip, LexerErrorKind> {
	// `/` and `*` are ASCII, so scanning bytes never splits a char
	let bytes = lex.remainder().as_bytes();
	let mut depth = 1usize;
	let mut i = 0;
	while i + 1 < bytes.len() {
		match (bytes[i], bytes[i + 1]) {
			(b'/', b'*') => {
				depth += 1;
				i += 2;
			},
			(b'*', b'/') => {
				depth -= 1;
				i += 2;
				if depth == 0 {
					lex.bump(i);
					return Ok(Skip);
				}
			},
			_ => i += 1,
		}
	}
	Err(LexerErrorKind::MultilineCommentDelimiterNotClosed)
}

fn lex_int(lex: &mut logos::Lexer<Token>) -> Result<u64, LexerErrorKind> {
	let s = lex.slice();

	if let Some(rest) = s.strip_prefix('0') {
		let radix = match rest.chars().next() {
			Some('b') => Some(2),
			Some('q') => Some(4),
			Some('o') => Some(8),
			Some('d') => Some(10),
			Some('x') => Some(16),
			_ => None,
		};
		if let Some(radix) = radix {
			return parse_digits(&rest[1..], radix);
		}
	}

	match s.split_once('r') {
		Some((radix, digits)) => {
			let radix = parse_digits(radix, 10)?;
			if radix > 36 {
				return Err(LexerErrorKind::IntRadixTooLarge(radix));
			}
			parse_digits(digits, radix)
		},
		None => parse_digits(s, 10),
	}
}

fn parse_digits(s: &str, radix: u64) -> Result<u64, LexerErrorKind> {
	if s.is_empty() {
		return Err(LexerErrorKind::IntHasNoDigits);
	}
	if s.starts_with('_') {
		return Err(LexerErrorKind::DigitsHaveLeadingUnderscore);
	}
	if s.ends_with('_') {
		return Err(LexerErrorKind::IntHasTrailingUnderscore);
	}

	let mut value = 0u64;
	for c in s.chars().filter(|&c| c != '_') {
		let digit_value = c.to_digit(36).expect("the regex only allows ASCII letters and digits") as u64;
		if digit_value >= radix {
			return Err(LexerErrorKind::InvalidDigitForRadix { digit_symbol: c, digit_value, radix });
		}
		value = value
			.checked_mul(radix)
			.and_then(|v| v.checked_add(digit_value))
			.ok_or(LexerErrorKind::IntOverflow)?;
	}
	Ok(value)
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
