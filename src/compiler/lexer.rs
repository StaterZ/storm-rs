use std::str::Chars;
use std::collections::HashMap;
use lazy_static::lazy_static;
use strum;
use szu::tag_enum;
use super::stream::Stream;

tag_enum!(
	pub enum (
		#[derive(Debug, PartialEq, Eq, Clone, strum::AsRefStr)] TokenKind,
		#[derive(Debug, PartialEq, Eq, Clone, Copy)] TokenKindTag,
	) {
		Space,
		NewLine,
		Eof,
		Equals,
		Semicolon,
		Compare,
		Let,
		Identifier(String),
		StrLit(String),
		IntLit(u64),
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

impl TokenKindTag {
	fn to_kind(self) -> TokenKind {
		match self {
			TokenKindTag::Space => TokenKind::Space,
			TokenKindTag::NewLine => TokenKind::NewLine,
			TokenKindTag::Eof => TokenKind::Eof,
			TokenKindTag::Equals => TokenKind::Equals,
			TokenKindTag::Semicolon => TokenKind::Semicolon,
			TokenKindTag::Compare => TokenKind::Compare,
			TokenKindTag::Let => TokenKind::Let,
			TokenKindTag::Identifier => panic!("bad tag conversion"),
			TokenKindTag::StrLit => panic!("bad tag conversion"),
			TokenKindTag::IntLit => panic!("bad tag conversion"),
		}
	}
}

fn next_token_kind(stream: &mut Stream<Chars>) -> Result<TokenKind, String> {
	{
		fn is_space(c: &char) -> bool { *c == ' ' || *c == '\t' }
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

	if stream.expect(|c| *c == '=').is_some() {
		if stream.expect(|c| *c == '=').is_some() {
			return Ok(TokenKind::Compare);
		}

		return Ok(TokenKind::Equals);
	}

	if stream.expect(|c| *c == ';').is_some() {
		return Ok(TokenKind::Semicolon);
	}
	
	if stream.expect(|c| *c == '\"').is_some() {
		let mut value = String::new();
		while stream.expect(|c| *c == '\"').is_none() {
			stream.expect(|c| *c == '\\');
			value.push(stream.next().unwrap());
		}

		return Ok(TokenKind::StrLit(value));
	}

	fn parse_int(stream: &mut Stream<Chars>) -> Result<Option<u64>, String> {
		let mut stream = stream.dup();

		fn parse_radix(stream: &mut Stream<Chars>) -> Result<Option<u32>, String> {
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

		if stream.get().check(|c| match c {
			'a'..='z' | 'A'..='Z' | '_' => true,
			_ => false,
		}) {
			stream.pop();
			return Ok(None);
		}

		stream.nip();
		return Ok(Some(value));
	}

	if let Some(value) = parse_int(stream)? {
		return Ok(TokenKind::IntLit(value));
	}

	if let Some(ident_begin) = stream.expect(|c| match c {
		'a'..='z' | 'A'..='Z' | '_' => true,
		_ => false,
	}) {
		let mut value = ident_begin.to_string();
		while let Some(ident) = stream.expect(|c| match c {
			'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => true,
			_ => false,
		}) {
			value.push(ident);
		}

		lazy_static!{
			static ref KEYWORDS: HashMap<&'static str, TokenKindTag> = vec![
				("let", TokenKindTag::Let),
			].into_iter().collect();
		}

		if let Some(&keyword) = KEYWORDS.get(value.as_str()) {
			return Ok(keyword.to_kind());
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
	let mut stream = Stream::new(src_in.chars());
	let mut tokens = Vec::<Token>::new();
	let mut start_index: usize = 0;
	loop {
		match next_token_kind(&mut stream) {
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
			Err(err) => {
				return Err(err);
			}
		}
	}
}
