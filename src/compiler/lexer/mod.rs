mod iter;

use std::{collections::HashMap, str::Chars};
use lazy_static::lazy_static;
use strum;
use szu::{tag_enum, tag_enum_helper};
use self::iter::{FilePosExt, FilePos};

use super::stream::Stream;

tag_enum!(
	pub enum (
		#[derive(Debug, PartialEq, Eq, Clone, strum::AsRefStr)] TokenKind,
		#[derive(Debug, PartialEq, Eq, Clone, Copy)] TokenKindTag,
	) {
		Space,
		NewLine,
		Comment,
		MultilineComment,

		Eof,
		Dot,
		Comma,
		Semicolon,

		Plus,
		Dash,
		Star,
		Slash,

		Equals,
		Compare,
		LParen,
		RParen,
		LBracket,
		RBracket,
		LBrace,
		RBrace,

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

#[derive(Debug)]
pub struct SourceRange {
	pub index: usize,
	pub length: usize,
}

#[derive(Debug)]
pub struct Token {
	pub kind: TokenKind,
	pub source: SourceRange,
}

fn parse_radix(stream: &mut Stream<impl Iterator<Item = char> + Clone>) -> Result<Option<u32>, String> {
	let mut stream = stream.dup();
	if stream.get().expect(|c| *c == '0').is_some() {
		lazy_static!{
			static ref RADICES: HashMap<char, u32> = vec![
				('b', 2),
				('q', 4),
				('o', 8),
				('d', 10),
				('x', 16),
			].into_iter().collect();
		}

		match stream.get().current {
			Some(c) => match RADICES.get(&c) {
				Some(&radix) => {
					stream.get().next();
					stream.nip();
					return Ok(Some(radix));
				},
				None => match c {
					'a'..='z' | 'A'..='Z' => {
						stream.nip();
						return Err(format!("Invalid radix symbol \'{:?}\'", c));
					},
					_ => stream.pop(),
				}
			},
			None => stream.pop(),
		};
	} else {
		stream.pop();
	}

	Ok(None)
}

fn parse_int(stream: &mut Stream<impl Iterator<Item = char> + Clone>) -> Result<Option<u64>, String> {
	let mut stream = stream.dup();

	let radix = parse_radix(&mut stream.get())?;
	let mut is_match = radix.is_some();
	let radix = radix.unwrap_or(10);

	let mut has_trailing_underscore = false;
	let mut value: u64 = 0;
	let mut i: usize = 0;
	while let Some(c) = stream.get().current {
		if c == '_' {
			if i == 0 {
				stream.pop();
				return Err("Leading underscore, this is not allowed".to_string());
			}

			has_trailing_underscore = true;
		} else {
			match c.to_digit(36) {
				Some(digit_value) => {
					if digit_value >= radix {
						stream.pop();
						return if is_match {
							Err(format!("Invalid digit \'{}\' for radix {}", c, radix))
						} else {
							Ok(None)
						}
					}
		
					value *= radix as u64;
					value += digit_value as u64;
		
					is_match = true;
					has_trailing_underscore = false;
				},
				None => break,
			};
		}

		stream.get().next();
		i += 1;
	}

	if has_trailing_underscore {
		stream.pop();
		return Err("Trailing underscore, this is not allowed".to_string());
	}

	if !is_match {
		stream.pop();
		return Ok(None);
	}

	if stream.get().check(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '_')) {
		stream.pop();
		return Ok(None);
	}

	stream.nip();
	return Ok(Some(value));
}

fn next_token_kind<I: Iterator<Item = char>>(stream: &mut Stream<impl Iterator<Item = char> + Clone>) -> Result<TokenKind, String> {
	{
		fn is_space(c: &char) -> bool { matches!(c, ' ' | '\t') }
		if stream.expect(is_space).is_some() {
			while stream.expect(is_space).is_some() { }
			return Ok(TokenKind::Space);
		}
	}

	{
		let r = stream.expect(|c| *c == '\r').is_some();
		let n = stream.expect(|c| *c == '\n').is_some();
		if r || n {
			return Ok(TokenKind::NewLine);
		}
	}

	if stream.expect(|c| *c == '/').is_some() {
		if stream.expect(|c| *c == '/').is_some() {
			stream.skip_while(|c| !matches!(c, '\r' | '\n')).next();
			return Ok(TokenKind::Comment);
		}

		return Ok(TokenKind::Slash);
	}

	if stream.expect(|c| *c == '=').is_some() {
		if stream.expect(|c| *c == '=').is_some() {
			return Ok(TokenKind::Compare);
		}

		return Ok(TokenKind::Equals);
	}

	if stream.expect(|c| *c == ';').is_some() {
		return Ok(TokenKind::Semicolon);
	}

	if let Some(value) = parse_int(stream)? {
		return Ok(TokenKind::IntLit(value));
	}

	{
		let ident_str = stream.expect(|c| *c == '@').is_some();
		if stream.expect(|c| *c == '\"').is_some() {
			let mut value = String::new();
			while stream.expect(|c| *c == '\"').is_none() {
				stream.expect(|c| *c == '\\');
				match stream.next() {
					Some(c) => value.push(c),
					None => return Err("String literal not closed".to_string()),
				}
			}

			return Ok(if ident_str {
				TokenKind::Identifier(value)
			} else {
				TokenKind::StrLit(value)
			});
		}
	}

	if let Some(ident_begin) = stream.expect(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '_')) {
		let mut value = ident_begin.to_string();
		while let Some(ident) = stream.expect(|c| matches!(c, 'a'..='z' | 'A'..='Z' | '0'..='9' | '_')) {
			value.push(ident);
		}

		lazy_static!{
			static ref KEYWORDS: HashMap<&'static str, TokenKindTag> = vec![
				("let", TokenKindTag::Let),
				("ipt", TokenKindTag::Ipt),
				("yield", TokenKindTag::Yield),

				("get", TokenKindTag::KeywordGet),
				("set", TokenKindTag::KeywordSet),
				("mov", TokenKindTag::KeywordMov),
				("add", TokenKindTag::KeywordAdd),
				("sub", TokenKindTag::KeywordSub),
				("mul", TokenKindTag::KeywordMul),
				("div", TokenKindTag::KeywordDiv),
				("lbl", TokenKindTag::KeywordLbl),
				("jmp", TokenKindTag::KeywordJmp),
				("beqz", TokenKindTag::KeywordBeqz),
				("bltz", TokenKindTag::KeywordBltz),
			].into_iter().collect();
		}

		if let Some(&keyword) = KEYWORDS.get(value.as_str()) {
			return Ok(keyword.into());
		}
		
		return Ok(TokenKind::Identifier(value));
	}

	if stream.current.is_none() {
		return Ok(TokenKind::Eof);
	}

	return Err(format!("No token factory matching stream '{}'", match stream.current {
		Some(c) => c.to_string(),
		None => "EOF".to_string(),
	}));
}

pub fn lex(src_in: &str) -> Result<Vec<Token>, String> {
	let mut stream = Stream::new(src_in.chars().file_pos());
	let mut tokens = Vec::<Token>::new();
	let mut start_index: usize = 0;
	loop {
		match next_token_kind::<Stream<FilePos<Chars>>>(&mut stream) {
			Ok(kind) => {
				let is_eof = kind == TokenKind::Eof;
				
				tokens.push(Token{
					kind: kind,
					source: SourceRange {
						index: start_index,
						length: stream.index - start_index,
					},
				});
				start_index = stream.index;

				if is_eof {
					return Ok(tokens);
				}
			},
			Err(err) => return Err(format!("[pos:{}] {}", stream.get_inner(), err)),
		}
	}
}
