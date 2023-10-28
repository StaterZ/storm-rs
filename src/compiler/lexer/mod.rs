use std::{collections::HashMap, fmt::Display};
use lazy_static::lazy_static;
use strum;
use szu::{tag_enum, tag_enum_helper, replace_macro_arg};

use super::{
	stream::{Stream, StreamExt},
	source_meta::{SourcePos, SourceRange, SourceFile},
};

mod file_pos;

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

#[derive(Debug)]
pub struct Token {
	pub kind: TokenKind,
	pub source: SourceRange,
}

impl Token {
	pub fn with_source<'a>(&'a self, source: &'a SourceFile) -> TokenWithSource<'a> {
		TokenWithSource {
			inner: self,
			source,
		}
	}
}

pub struct TokenWithSource<'a> {
	pub inner: &'a Token,
	pub source: &'a SourceFile,
}

impl<'a> Display for TokenWithSource<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{} -> {:?}", self.inner.source.with_source(self.source), self.inner.kind)
	}
}

fn parse_radix(stream: &mut Stream<
	impl Iterator<Item = (usize, (usize, char))> + Clone,
	impl (Fn(&(usize, (usize, char))) -> &char) + Clone,
	impl (Fn((usize, (usize, char))) -> char) + Clone,
	char,
>) -> Result<Option<u32>, String> {
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

		match stream.get().get_peeker().get_current() {
			Some(c) => match RADICES.get(&c) {
				Some(&radix) => {
					stream.get().next();
					stream.nip();
					return Ok(Some(radix));
				},
				None => match c {
					'a'..='z' | 'A'..='Z' => {
						let msg = format!("Invalid radix symbol \'{:?}\'", c);
						stream.nip();
						return Err(msg);
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

fn parse_int(stream: &mut Stream<
	impl Iterator<Item = (usize, (usize, char))> + Clone,
	impl (Fn(&(usize, (usize, char))) -> &char) + Clone,
	impl (Fn((usize, (usize, char))) -> char) + Clone,
	char,
>) -> Result<Option<u64>, String> {
	let mut stream = stream.dup();

	let radix = parse_radix(&mut stream.get())?;
	let mut is_match = radix.is_some();
	let radix = radix.unwrap_or(10);

	let mut has_trailing_underscore = false;
	let mut value = 0u64;
	let mut i = 0usize;
	while let Some(&c) = stream.get().get_peeker().get_current() {
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
						let result = if is_match {
							Err(format!("Invalid digit \'{}\' for radix {}", c, radix))
						} else {
							Ok(None)
						};
						stream.pop();

						return result;
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

fn next_token_kind(stream: &mut Stream<
	impl Iterator<Item = (usize, (usize, char))> + Clone,
	impl (Fn(&(usize, (usize, char))) -> &char) + Clone,
	impl (Fn((usize, (usize, char))) -> char) + Clone,
	char,
>) -> Result<TokenKind, String> {
	{
		fn is_space(c: &char) -> bool { matches!(*c, ' ' | '\t') }
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

	if stream.expect(|c| *c == '.').is_some() {
		return Ok(TokenKind::Dot);
	}
	if stream.expect(|c| *c == ',').is_some() {
		return Ok(TokenKind::Comma);
	}
	if stream.expect(|c| *c == ';').is_some() {
		return Ok(TokenKind::Semicolon);
	}
	
	if stream.expect(|c| *c == '(').is_some() {
		return Ok(TokenKind::LParen);
	}
	if stream.expect(|c| *c == ')').is_some() {
		return Ok(TokenKind::RBrace);
	}
	if stream.expect(|c| *c == '[').is_some() {
		return Ok(TokenKind::LBracket);
	}
	if stream.expect(|c| *c == ']').is_some() {
		return Ok(TokenKind::RBracket);
	}
	if stream.expect(|c| *c == '{').is_some() {
		return Ok(TokenKind::LBrace);
	}
	if stream.expect(|c| *c == '}').is_some() {
		return Ok(TokenKind::RBrace);
	}

	if stream.expect(|c| *c == '+').is_some() {
		return Ok(TokenKind::Plus);
	}
	if stream.expect(|c| *c == '-').is_some() {
		return Ok(TokenKind::Dash);
	}
	if stream.expect(|c| *c == '*').is_some() {
		return Ok(TokenKind::Star);
	}
	if stream.expect(|c| *c == '/').is_some() {
		if stream.expect(|c| *c == '/').is_some() {
			stream.skip_while(|c| !matches!(c, '\r' | '\n')).next();
			return Ok(TokenKind::Comment);
		}
		if stream.expect(|c| *c == '*').is_some() {
			let mut indent = 0usize;
			while indent > 0 {
				match stream.next() {
					Some(c) => match c {
						'/' if stream.expect(|c| *c == '*').is_some() => indent += 1,
						'*' if stream.expect(|c| *c == '/').is_some() => indent -= 1,
						_ => {},
					},
					None => return Err("Multi-line comment not closed".to_string()),
				}
			}
			stream.skip_while(|c| !matches!(c, '\r' | '\n')).next();
			return Ok(TokenKind::MultilineComment);
		}
		
		return Ok(TokenKind::Slash);
	}
	if stream.expect(|c| *c == '%').is_some() {
		return Ok(TokenKind::Percent);
	}
	if stream.expect(|c| *c == '<').is_some() {
		if stream.expect(|c| *c == '<').is_some() {
			return Ok(TokenKind::LShift);
		}

		return Ok(TokenKind::LessThan);
	}
	if stream.expect(|c| *c == '>').is_some() {
		if stream.expect(|c| *c == '>').is_some() {
			return Ok(TokenKind::RShift);
		}

		return Ok(TokenKind::GreaterThan);
	}

	if stream.expect(|c| *c == '=').is_some() {
		if stream.expect(|c| *c == '=').is_some() {
			return Ok(TokenKind::Equality);
		}

		return Ok(TokenKind::Equals);
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

	if stream.get_peeker().get_current().is_none() {
		return Ok(TokenKind::Eof);
	}

	return Err(format!("No token factory matching stream '{}'", match stream.get_peeker().get_current() {
		Some(c) => c.to_string(),
		None => "EOF".to_string(),
	}));
}

pub fn lex(src_in: &str) -> Result<Vec<Token>, String> {
	let mut tokens = Vec::<Token>::new();
	let mut stream = src_in.char_indices().enumerate().stream(|(_, (_, c))| c, |(_, (_, c))| c);

	fn get_current_source_pos(stream: &mut Stream<
		impl Iterator<Item = (usize, (usize, char))>,
		impl Fn(&(usize, (usize, char))) -> &char,
		impl Fn((usize, (usize, char))) -> char,
		char,
	>) -> Option<SourcePos> {
		stream
			.get_peeker()
			.get_current_raw()
			.map(|&(char_index, (byte_index, _))| SourcePos{
				char_index,
				byte_index,
			})
	}

	loop {
		let begin_index = get_current_source_pos(&mut stream);
		match next_token_kind(&mut stream) {
			Ok(kind) => {
				let end_index = get_current_source_pos(&mut stream);
				let is_eof = kind == TokenKind::Eof;
				
				tokens.push(Token{
					kind: kind,
					source: SourceRange {
						begin: begin_index,
						end: end_index,
					},
				});
				
				if is_eof {
					return Ok(tokens);
				}
			},
			Err(err) => return Err(format!("[pos:{}] {}", stream.get_peeker().get_current_raw().unwrap().0, err)),
		}
	}
}
